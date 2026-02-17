// use lettre 11
#[macro_use]
extern crate log;

use configparser::ini::Ini;
use lettre::message::{header, Mailbox, MultiPart, SinglePart};
use lettre::transport::smtp::PoolConfig;
use lettre::{Address, Message, SmtpTransport, Transport};
use std::collections::HashMap;
use std::error::Error;
use std::fs::{self, OpenOptions};
use std::io::{BufRead, BufReader, Write};
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::time::{Duration, SystemTime, UNIX_EPOCH};
use serde::{Deserialize, Serialize};
use v_common::module::common::load_onto;
use v_common::module::info::ModuleInfo;
use v_common::module::module_impl::{get_cmd, get_inner_binobj_as_individual, init_log, wait_load_ontology, Module, PrepareError};
use v_common::module::veda_backend::Backend;
use v_common::search::common::FTQuery;
use v_common::v_api::common_type::ResultCode;
use v_common::v_queue::consumer::Consumer;
use v_individual_model::onto::individual::Individual;
use v_individual_model::onto::onto_impl::Onto;

const ATTACHMENTS_DB_PATH: &str = "data/files";
const RETRY_CONFIG_CANDIDATES: [&str; 4] = [
    "../../config/veda-fanout-email.ini",
    "../config/veda-fanout-email.ini",
    "./config/veda-fanout-email.ini",
    "config/veda-fanout-email.ini",
];

#[derive(Debug, Clone)]
struct RetryConfig {
    enabled: bool,
    retry_on_transient_only: bool,
    base_delay: u64,
    max_delay: u64,
    max_attempts: u32,
    max_total: u64,
    heartbeat_check: u64,
}

impl Default for RetryConfig {
    fn default() -> Self {
        Self {
            enabled: true,
            retry_on_transient_only: true,
            base_delay: 60,
            max_delay: 3600,
            max_attempts: 24,
            max_total: 172800,
            heartbeat_check: 5,
        }
    }
}

#[derive(Debug, Clone)]
struct RetryStorageConfig {
    pending_file_path: String,
    dead_letter_file_path: String,
    temp_file_path: String,
    max_record_size_bytes: usize,
    max_pending_records: usize,
}

impl Default for RetryStorageConfig {
    fn default() -> Self {
        Self {
            pending_file_path: "./data/email-retry-pending.jsonl".to_string(),
            dead_letter_file_path: "./data/email-retry-dead-letter.jsonl".to_string(),
            temp_file_path: "./data/email-retry-pending.tmp".to_string(),
            max_record_size_bytes: 1024 * 1024,
            max_pending_records: 10_000,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct PendingEmailRecord {
    msg_uri: String,
    from: String,
    to: Vec<String>,
    subject: Option<String>,
    body: Option<String>,
    attachments: Vec<String>,
    attempt_count: u32,
    first_failed_at: u64,
    next_retry_at: u64,
    last_error: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct DeadLetterRecord {
    moved_at: u64,
    reason: String,
    raw_line: Option<String>,
    record: Option<PendingEmailRecord>,
}

pub struct Context {
    onto: Onto,
    smtp_client: Option<SmtpTransport>,
    default_mail_sender: String,
    always_use_mail_sender: bool,
    sys_ticket: String,
    module_info: ModuleInfo,
    retry_config: RetryConfig,
    retry_storage: RetryStorageConfig,
    last_retry_check_ts: u64,
}

fn main() -> Result<(), i32> {
    init_log("FANOUT_EMAIL");

    wait_load_ontology();

    let (retry_config, retry_storage) = load_retry_settings();
    info!("Email retry config loaded: enabled={}, base_delay={}, max_delay={}, max_attempts={}, max_total={}",
        retry_config.enabled,
        retry_config.base_delay,
        retry_config.max_delay,
        retry_config.max_attempts,
        retry_config.max_total
    );
    info!(
        "Email retry storage loaded: pending_file_path={}, dead_letter_file_path={}",
        retry_storage.pending_file_path, retry_storage.dead_letter_file_path
    );

    let mut queue_consumer = Consumer::new("./data/queue", "fanout_email0", "individuals-flow").expect("!!!!!!!!! FAIL QUEUE");

    let module_info = ModuleInfo::new("./data", "fanout_email0", true);
    if module_info.is_err() {
        error!("failed to start, err = {:?}", &module_info.err());
        return Err(-1);
    }

    let mut module = Module::default();
    let mut backend = Backend::default();
    let systicket = backend.get_sys_ticket_id();

    let mut ctx = Context {
        onto: Onto::default(),
        smtp_client: None,
        default_mail_sender: String::default(),
        always_use_mail_sender: false,
        sys_ticket: systicket.unwrap_or_default(),
        module_info: module_info.unwrap(),
        retry_config,
        retry_storage,
        last_retry_check_ts: 0,
    };

    connect_to_smtp(&mut ctx, &mut backend);

    info!("load ontology start");
    load_onto(&mut backend.storage, &mut ctx.onto);
    info!("load ontology end");

    module.listen_queue(
        &mut queue_consumer,
        &mut ctx,
        &mut (before_batch as fn(&mut Backend, &mut Context, size_batch: u32) -> Option<u32>),
        &mut (prepare as fn(&mut Backend, &mut Context, &mut Individual, my_consumer: &Consumer) -> Result<bool, PrepareError>),
        &mut (after_batch as fn(&mut Backend, &mut Context, prepared_batch_size: u32) -> Result<bool, PrepareError>),
        &mut (heartbeat as fn(&mut Backend, &mut Context) -> Result<(), PrepareError>),
        &mut backend,
    );
    Ok(())
}

fn heartbeat(module: &mut Backend, ctx: &mut Context) -> Result<(), PrepareError> {
    process_retry_queue(module, ctx);
    Ok(())
}

fn before_batch(_module: &mut Backend, _ctx: &mut Context, _size_batch: u32) -> Option<u32> {
    None
}

fn after_batch(_module: &mut Backend, _ctx: &mut Context, _prepared_batch_size: u32) -> Result<bool, PrepareError> {
    Ok(false)
}

fn now_ts() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or_default()
}

fn parse_bool(v: Option<String>, default: bool) -> bool {
    v.and_then(|s| s.parse::<bool>().ok()).unwrap_or(default)
}

fn parse_u64(v: Option<String>, default: u64) -> u64 {
    v.and_then(|s| s.parse::<u64>().ok()).unwrap_or(default)
}

fn parse_duration(v: Option<String>, default: u64) -> u64 {
    let Some(raw) = v else {
        return default;
    };
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        return default;
    }

    let mut normalized = String::with_capacity(trimmed.len() + 8);
    let mut prev_is_alpha = false;
    for ch in trimmed.chars() {
        let is_digit = ch.is_ascii_digit();
        if prev_is_alpha && is_digit {
            normalized.push(' ');
        }
        normalized.push(ch);
        prev_is_alpha = ch.is_ascii_alphabetic();
    }

    match humantime::parse_duration(&normalized) {
        Ok(d) => d.as_secs(),
        Err(e) => {
            error!(
                "Failed to parse duration '{}', using default {} seconds: {}",
                raw, default, e
            );
            default
        },
    }
}

fn parse_usize(v: Option<String>, default: usize) -> usize {
    v.and_then(|s| s.parse::<usize>().ok()).unwrap_or(default)
}

fn load_retry_settings() -> (RetryConfig, RetryStorageConfig) {
    let mut retry_config = RetryConfig::default();
    let mut retry_storage = RetryStorageConfig::default();

    let cfg_path = RETRY_CONFIG_CANDIDATES
        .iter()
        .find(|p| Path::new(p).exists())
        .map(|p| p.to_string());

    let Some(cfg_path) = cfg_path else {
        warn!(
            "Retry config file not found in known paths: {:?}, using defaults",
            RETRY_CONFIG_CANDIDATES
        );
        return (retry_config, retry_storage);
    };

    info!("Using retry config file: {}", cfg_path);
    let mut ini = Ini::new();
    match ini.load(&cfg_path) {
        Ok(_) => {
            retry_config.enabled = parse_bool(ini.get("retry", "enabled"), retry_config.enabled);
            retry_config.retry_on_transient_only = parse_bool(
                ini.get("retry", "retry_on_transient_only"),
                retry_config.retry_on_transient_only,
            );
            retry_config.base_delay = parse_duration(
                ini.get("retry", "base_delay"),
                retry_config.base_delay,
            );
            retry_config.max_delay = parse_duration(
                ini.get("retry", "max_delay"),
                retry_config.max_delay,
            );
            retry_config.max_attempts =
                parse_u64(ini.get("retry", "max_attempts"), retry_config.max_attempts as u64) as u32;
            retry_config.max_total = parse_duration(
                ini.get("retry", "max_total"),
                retry_config.max_total,
            );
            retry_config.heartbeat_check = parse_duration(
                ini.get("retry", "heartbeat_check"),
                retry_config.heartbeat_check,
            );

            retry_storage.pending_file_path = ini
                .get("retry_storage", "pending_file_path")
                .unwrap_or(retry_storage.pending_file_path);
            retry_storage.dead_letter_file_path = ini
                .get("retry_storage", "dead_letter_file_path")
                .unwrap_or(retry_storage.dead_letter_file_path);
            retry_storage.temp_file_path = ini
                .get("retry_storage", "temp_file_path")
                .unwrap_or(retry_storage.temp_file_path);
            retry_storage.max_record_size_bytes = parse_usize(
                ini.get("retry_storage", "max_record_size_bytes"),
                retry_storage.max_record_size_bytes,
            );
            retry_storage.max_pending_records = parse_usize(
                ini.get("retry_storage", "max_pending_records"),
                retry_storage.max_pending_records,
            );
        },
        Err(e) => {
            error!("Failed to read retry config {}: {}", cfg_path, e);
        },
    }

    (retry_config, retry_storage)
}

fn ensure_parent_dir(file_path: &str) {
    let parent_dir: Option<PathBuf> = Path::new(file_path).parent().map(|p| p.to_path_buf());
    if let Some(parent) = parent_dir {
        if !parent.as_os_str().is_empty() && !parent.exists() {
            if let Err(e) = fs::create_dir_all(&parent) {
                error!("Failed to create directory {}: {}", parent.display(), e);
            }
        }
    }
}

fn append_dead_letter(storage: &RetryStorageConfig, reason: String, record: Option<PendingEmailRecord>, raw_line: Option<String>) {
    ensure_parent_dir(&storage.dead_letter_file_path);
    let dead = DeadLetterRecord {
        moved_at: now_ts(),
        reason,
        raw_line,
        record,
    };

    match serde_json::to_string(&dead) {
        Ok(line) => {
            match OpenOptions::new()
                .create(true)
                .append(true)
                .open(&storage.dead_letter_file_path)
            {
                Ok(mut file) => {
                    if let Err(e) = writeln!(file, "{}", line) {
                        error!("Failed to append dead-letter record: {}", e);
                    }
                },
                Err(e) => error!(
                    "Failed to open dead-letter file {}: {}",
                    storage.dead_letter_file_path, e
                ),
            }
        },
        Err(e) => error!("Failed to serialize dead-letter record: {}", e),
    }
}

fn load_pending_records(storage: &RetryStorageConfig) -> Vec<PendingEmailRecord> {
    if !Path::new(&storage.pending_file_path).exists() {
        return Vec::new();
    }

    let file = match fs::File::open(&storage.pending_file_path) {
        Ok(f) => f,
        Err(e) => {
            error!("Failed to read pending file {}: {}", storage.pending_file_path, e);
            return Vec::new();
        },
    };

    let mut records = Vec::new();
    for line_result in BufReader::new(file).lines() {
        let line = match line_result {
            Ok(l) => l,
            Err(e) => {
                error!("Failed to read pending line: {}", e);
                continue;
            },
        };

        if line.trim().is_empty() {
            continue;
        }

        if line.len() > storage.max_record_size_bytes {
            append_dead_letter(
                storage,
                format!("record_too_large: {} bytes", line.len()),
                None,
                Some(line),
            );
            continue;
        }

        match serde_json::from_str::<PendingEmailRecord>(&line) {
            Ok(r) => records.push(r),
            Err(e) => {
                append_dead_letter(
                    storage,
                    format!("invalid_record_json: {}", e),
                    None,
                    Some(line),
                );
            },
        }
    }

    records
}

fn save_pending_records_atomic(storage: &RetryStorageConfig, records: &[PendingEmailRecord]) {
    ensure_parent_dir(&storage.pending_file_path);
    ensure_parent_dir(&storage.temp_file_path);

    let mut tmp = match fs::File::create(&storage.temp_file_path) {
        Ok(f) => f,
        Err(e) => {
            error!("Failed to create temp pending file {}: {}", storage.temp_file_path, e);
            return;
        },
    };

    for record in records {
        match serde_json::to_string(record) {
            Ok(line) => {
                if let Err(e) = writeln!(tmp, "{}", line) {
                    error!("Failed to write pending temp line: {}", e);
                    return;
                }
            },
            Err(e) => {
                error!("Failed to serialize pending record: {}", e);
            },
        }
    }

    if let Err(e) = fs::rename(&storage.temp_file_path, &storage.pending_file_path) {
        error!(
            "Failed to replace pending file {} from {}: {}",
            storage.pending_file_path, storage.temp_file_path, e
        );
    }
}

fn compute_next_retry_at(now: u64, attempt_count: u32, cfg: &RetryConfig) -> u64 {
    let power = attempt_count.saturating_sub(1).min(31);
    let delay = cfg
        .base_delay
        .saturating_mul(2u64.saturating_pow(power))
        .min(cfg.max_delay);
    now.saturating_add(delay)
}

fn enqueue_failed_email(ctx: &Context, mut record: PendingEmailRecord) {
    if !ctx.retry_config.enabled {
        return;
    }

    let mut records = load_pending_records(&ctx.retry_storage);
    if records.len() >= ctx.retry_storage.max_pending_records {
        append_dead_letter(
            &ctx.retry_storage,
            "pending_capacity_exceeded".to_string(),
            Some(record),
            None,
        );
        return;
    }

    if record.next_retry_at == 0 {
        let now = now_ts();
        record.next_retry_at = now.saturating_add(ctx.retry_config.base_delay);
    }

    records.push(record);
    save_pending_records_atomic(&ctx.retry_storage, &records);
}

fn prepare(backend: &mut Backend, ctx: &mut Context, queue_element: &mut Individual, _my_consumer: &Consumer) -> Result<bool, PrepareError> {
    let cmd = get_cmd(queue_element);
    if cmd.is_none() {
        error!("skip queue message: cmd is none");
        return Ok(true);
    }

    let op_id = queue_element.get_first_integer("op_id").unwrap_or_default();

    let mut prev_state = Individual::default();
    get_inner_binobj_as_individual(queue_element, "prev_state", &mut prev_state);

    let mut new_state = Individual::default();
    get_inner_binobj_as_individual(queue_element, "new_state", &mut new_state);

    if let Err(e) = ctx.module_info.put_info(op_id, op_id) {
        error!("failed to write module_info, op_id = {}, err = {:?}", op_id, e)
    }

    if let Some(types) = new_state.get_literals("rdf:type") {
        let is_version = types.iter().any(|t: &String| t == "v-s:Version");
        if is_version {
            info!("individual {} is version, ignore", new_state.get_id());
            return Ok(true);
        }

        for itype in types.iter() {
            if ctx.onto.is_some_entered(itype, &["v-s:Deliverable"]) {
                let uri = new_state.get_id().to_string();
                let result = catch_unwind(AssertUnwindSafe(|| {
                    prepare_deliverable(&mut new_state, backend, ctx)
                }));
                if let Err(e) = result {
                    error!("panic while processing deliverable, uri = {}, op_id = {}, queue_element = {}: {:?}", uri, op_id, queue_element.get_id(), e);
                }
                break;
            }
        }
    }

    Ok(true)
}

struct IndvAddreses {
    has_message_type: Option<String>,
    from: String,
    to: Vec<String>,
    //reply_to: Option<Vec<String>>,
    sender_mailbox: String,
    recipient_mailbox: Option<Vec<String>>,
}

impl IndvAddreses {
    // Создаем метод для заполнения структуры из объекта `prepared_indv`
    fn from_prepared_individual(prepared_indv: &mut Individual, ctx: &mut Context) -> Self {
        let mut from = prepared_indv.get_first_literal("v-wf:from").unwrap_or_default();
        let sender_mailbox = prepared_indv.get_first_literal("v-s:senderMailbox").unwrap_or_default();
        if from.is_empty() && sender_mailbox.is_empty() && !ctx.default_mail_sender.is_empty() {
            from = ctx.default_mail_sender.to_string();
        }

        IndvAddreses {
            has_message_type: prepared_indv.get_first_literal("v-s:hasMessageType"),
            from,
            to: prepared_indv.get_literals("v-wf:to").unwrap_or_default(),
            //reply_to: prepared_indv.get_literals("v-wf:replyTo"),
            sender_mailbox,
            recipient_mailbox: prepared_indv.get_literals("v-s:recipientMailbox"),
        }
    }
}

struct MailAddreses {
    email_from: Mailbox,
    rr_email_to_hash: HashMap<String, Mailbox>,
    //rr_reply_to_hash: HashMap<String, Mailbox>
}

impl MailAddreses {
    fn from_indv_addreses(message_info: IndvAddreses, backend: &mut Backend, ctx: &mut Context) -> Option<Self> {
        // Пробуем получить корректный email отправителя
        let email_from = if ctx.always_use_mail_sender && !ctx.default_mail_sender.is_empty() && ctx.default_mail_sender.len() > 5 {
            info!("use default mail sender: {}", ctx.default_mail_sender);
            if !ctx.default_mail_sender.contains('@') {
                extract_email(&None, &ctx.default_mail_sender.to_string(), ctx, backend).pop()
            } else {
                Address::from_str(&ctx.default_mail_sender).ok().map(|address| Mailbox::new(Some("Veda System".to_string()), address))
            }
        } else {
            let mut result = None;

            // Пробуем from
            if !message_info.from.is_empty() {
                info!("extract from: {}", message_info.from);
                result = extract_email(&None, &message_info.from, ctx, backend).pop();
            }

            // Если не получилось, пробуем default_mail_sender
            if result.is_none() && !ctx.default_mail_sender.is_empty() {
                result = extract_email(&None, &ctx.default_mail_sender.to_string(), ctx, backend).pop();
            }

            // Если всё ещё нет, пробуем sender_mailbox
            if result.is_none() && !message_info.sender_mailbox.is_empty() {
                result = Address::from_str(&message_info.sender_mailbox).ok().map(|address| Mailbox::new(Some("Veda System".to_string()), address));
            }

            result
        };

        // Если не удалось получить корректный адрес отправителя - возвращаем None
        let email_from = match email_from {
            Some(from) => from,
            None => {
                error!("No valid sender email address found. Message cannot be sent.");
                return None;
            },
        };

        let mut rr_email_to_hash = HashMap::new();
        for elt in message_info.to {
            for r in extract_email(&message_info.has_message_type, &elt, ctx, backend) {
                rr_email_to_hash.insert(r.email.to_string(), r);
            }
        }

        for el in message_info.recipient_mailbox.unwrap_or_default() {
            if let Ok(address) = Address::from_str(&el) {
                rr_email_to_hash.insert(el.to_string(), Mailbox::new(Some("Recipient".to_string()), address));
            }
        }

        Some(MailAddreses {
            email_from,
            rr_email_to_hash,
        })
    }
}

fn prepare_deliverable(msg_indv: &mut Individual, backend: &mut Backend, ctx: &mut Context) -> ResultCode {
    // Проверяем нужно ли подробное логирование
    let debug_logging = msg_indv.is_exists("v-s:debugEmail");

    if debug_logging {
        info!("Starting to prepare deliverable for {}", msg_indv.get_id());
    }

    let is_deleted = msg_indv.is_exists("v-s:deleted");
    if is_deleted {
        if debug_logging {
            info!("Individual {} is deleted, ignore", msg_indv.get_id());
        }
        return ResultCode::Ok;
    }

    let is_draft_of = msg_indv.get_first_literal("v-s:is_draft_of");
    if is_draft_of.is_some() {
        if debug_logging {
            info!("Individual {} is draft, ignore", msg_indv.get_id());
        }
        return ResultCode::Ok;
    }

    let subject = msg_indv.get_first_literal("v-s:subject");
    let message_body = msg_indv.get_first_literal("v-s:messageBody");
    let attachments = msg_indv.get_literals("v-s:attachment");

    if debug_logging {
        info!(
            "Email details for {}: subject={:?}, has_body={}, attachment_count={:?}",
            msg_indv.get_id(),
            subject,
            message_body.is_some(),
            attachments.as_ref().map(|a: &Vec<String>| a.len())
        );
    }

    let message_info = IndvAddreses::from_prepared_individual(msg_indv, ctx);

    if debug_logging {
        info!(
            "Address details for {}: from={}, to_count={}, has_sender_mailbox={}, recipient_mailbox_count={:?}",
            msg_indv.get_id(),
            message_info.from,
            message_info.to.len(),
            !message_info.sender_mailbox.is_empty(),
            message_info.recipient_mailbox.as_ref().map(|r| r.len())
        );
    }

    let mail_addreses = match MailAddreses::from_indv_addreses(message_info, backend, ctx) {
        Some(addresses) => {
            if debug_logging {
                info!("Resolved email addresses for {}: from={}, to_count={}", msg_indv.get_id(), addresses.email_from.email, addresses.rr_email_to_hash.len());
            }
            addresses
        },
        None => {
            error!("Failed to create email - no valid sender address found, uri = {}", msg_indv.get_id());
            return ResultCode::InvalidIdentifier;
        },
    };

    if !mail_addreses.rr_email_to_hash.is_empty() {
        let mut message_builder = Message::builder().from(mail_addreses.email_from.clone()).header(header::ContentTransferEncoding::QuotedPrintable);

        for el in mail_addreses.rr_email_to_hash.values() {
            message_builder = message_builder.to(el.clone());
            if debug_logging {
                info!("Adding recipient for {}: {}", msg_indv.get_id(), el.email);
            }
        }

        if let Some(s) = subject.clone() {
            message_builder = message_builder.subject(s);
        }

        // Собираем сообщение в зависимости от наличия вложений
        let email = if attachments.is_some() {
            if debug_logging {
                info!("Processing attachments for {}", msg_indv.get_id());
            }
            let mut builder = MultiPart::mixed().build();

            // Добавляем тело письма
            if let Some(body) = &message_body {
                let is_html = body.to_lowercase().contains("<html>");
                if debug_logging {
                    info!(
                        "Adding message body for {}, content type: {}",
                        msg_indv.get_id(),
                        if is_html {
                            "text/html"
                        } else {
                            "text/plain"
                        }
                    );
                }

                let body_part = if is_html {
                    SinglePart::builder()
                        .header(header::ContentType::parse("text/html; charset=utf-8").unwrap())
                        .header(header::ContentTransferEncoding::QuotedPrintable)
                        .body(body.to_string())
                } else {
                    SinglePart::builder()
                        .header(header::ContentType::parse("text/plain; charset=utf-8").unwrap())
                        .header(header::ContentTransferEncoding::QuotedPrintable)
                        .body(body.to_string())
                };
                builder = builder.singlepart(body_part);
            }

            // Добавляем вложения
            for id in attachments.clone().unwrap().iter() {
                if debug_logging {
                    info!("Processing attachment {} for {}", id, msg_indv.get_id());
                }
                if let Some(file_info) = backend.get_individual(id, &mut Individual::default()) {
                    if let (Some(path), Some(file_uri), Some(file_name)) =
                        (file_info.get_first_literal("v-s:filePath"), file_info.get_first_literal("v-s:fileUri"), file_info.get_first_literal("v-s:fileName"))
                    {
                        if !path.is_empty() {
                            let full_path = format!("{}/{}/{}", ATTACHMENTS_DB_PATH, path, file_uri);
                            if debug_logging {
                                info!("Reading attachment from {}", full_path);
                            }

                            match std::fs::read(&full_path) {
                                Ok(content) => {
                                    let mime = mime_guess::from_path(&file_name).first_or_octet_stream();
                                    let mime_str = mime.essence_str().to_string();
                                    if debug_logging {
                                        info!("Attachment {} mime type: {}", file_name, mime_str);
                                    }

                                    let content_type = header::ContentType::parse(&mime_str).unwrap_or_else(|_| {
                                        if debug_logging {
                                            warn!("Failed to parse mime type {}, using octet-stream", mime_str);
                                        }
                                        header::ContentType::parse("application/octet-stream").unwrap()
                                    });

                                    let attachment = SinglePart::builder()
                                        .header(content_type)
                                        .header(header::ContentDisposition::attachment(&file_name))
                                        .header(header::ContentTransferEncoding::Base64)
                                        .body(content);

                                    builder = builder.singlepart(attachment);
                                    if debug_logging {
                                        info!("Successfully added attachment {}", file_name);
                                    }
                                },
                                Err(e) => {
                                    error!("Failed to read attachment {} for email {}, err = {:?}", &full_path, msg_indv.get_id(), e);
                                },
                            }
                        }
                    }
                }
            }

            message_builder.multipart(builder)
        } else if let Some(body) = &message_body {
            let is_html = body.to_lowercase().contains("<html>");
            if debug_logging {
                info!(
                    "Creating single part message for {}, content type: {}",
                    msg_indv.get_id(),
                    if is_html {
                        "text/html"
                    } else {
                        "text/plain"
                    }
                );
            }

            if is_html {
                message_builder
                    .header(header::ContentType::parse("text/html; charset=utf-8").unwrap())
                    .header(header::ContentTransferEncoding::QuotedPrintable)
                    .body(body.to_string())
            } else {
                message_builder
                    .header(header::ContentType::parse("text/plain; charset=utf-8").unwrap())
                    .header(header::ContentTransferEncoding::QuotedPrintable)
                    .body(body.to_string())
            }
        } else {
            if debug_logging {
                info!("Creating empty message for {}", msg_indv.get_id());
            }
            message_builder
                .header(header::ContentType::parse("text/plain; charset=utf-8").unwrap())
                .header(header::ContentTransferEncoding::QuotedPrintable)
                .body(String::new())
        };

        match email {
            Ok(email) => {
                if let Some(mailer) = &ctx.smtp_client {
                    if debug_logging {
                        info!("Attempting to send email for {}", msg_indv.get_id());
                    }
                    match mailer.send(&email) {
                        Ok(response) => {
                            // Извлекаем message_id из ответа SMTP сервера
                            let message_id = response.message().collect::<Vec<_>>().first().and_then(|msg| {
                                if let Some(start) = msg.find('<') {
                                    if let Some(end) = msg[start..].find('>') {
                                        Some(&msg[start..start + end + 1])
                                    } else {
                                        None
                                    }
                                } else {
                                    None
                                }
                            });

                            // Базовый лог для любого случая
                            info!(
                                "Email sent: msg={}, from={}, to={:?}, smtp_code={}, message_id={}",
                                msg_indv.get_id(),
                                mail_addreses.email_from.email,
                                mail_addreses.rr_email_to_hash.values().map(|m| m.email.to_string()).collect::<Vec<_>>(),
                                response.code(),
                                message_id.unwrap_or("unknown")
                            );

                            if debug_logging {
                                info!("Full SMTP server response: {:?}", response);
                            }
                            return ResultCode::Ok;
                        },
                        Err(e) => {
                            error!("Failed to send email, uri = {}, error details: {:?}", msg_indv.get_id(), e);
                            let is_transient = e.is_transient();

                            // Определяем тип ошибки и выводим соответствующую информацию
                            if e.is_permanent() {
                                if let Some(code) = e.status() {
                                    error!("Permanent SMTP error with code {}: {}", code, e);
                                } else {
                                    error!("Permanent SMTP error without code: {}", e);
                                }
                                error!("This error cannot be resolved by retrying");
                            } else if e.is_transient() {
                                if let Some(code) = e.status() {
                                    error!("Transient SMTP error with code {}: {}", code, e);
                                } else {
                                    error!("Transient SMTP error without code: {}", e);
                                }
                                error!("This error might be resolved by retrying later");
                            } else if e.is_client() {
                                error!("SMTP client error: {}", e);
                            } else if e.is_response() {
                                error!("SMTP response error: {}", e);
                            } else if e.is_timeout() {
                                error!("SMTP timeout error: {}", e);
                            }

                            // Выводим информацию об исходной ошибке, если она есть
                            if let Some(source) = e.source() {
                                error!("Underlying error: {}", source);
                                if let Some(io_err) = source.downcast_ref::<std::io::Error>() {
                                    error!("IO error kind: {:?}", io_err.kind());
                                    if let Some(os_err) = io_err.raw_os_error() {
                                        error!("OS error code: {}", os_err);
                                    }
                                }
                            }

                            // Логируем контекст отправки
                            error!(
                                "Email context: from={}, to={:?}, subject={:?}",
                                mail_addreses.email_from.email,
                                mail_addreses.rr_email_to_hash.values().map(|m| m.email.to_string()).collect::<Vec<_>>(),
                                subject
                            );

                            if ctx.retry_config.enabled
                                && (!ctx.retry_config.retry_on_transient_only || is_transient)
                            {
                                let now = now_ts();
                                let failed_record = PendingEmailRecord {
                                    msg_uri: msg_indv.get_id().to_string(),
                                    from: mail_addreses.email_from.email.to_string(),
                                    to: mail_addreses
                                        .rr_email_to_hash
                                        .values()
                                        .map(|m| m.email.to_string())
                                        .collect(),
                                    subject: subject.clone(),
                                    body: message_body.clone(),
                                    attachments: attachments.clone().unwrap_or_default(),
                                    attempt_count: 1,
                                    first_failed_at: now,
                                    next_retry_at: now.saturating_add(ctx.retry_config.base_delay),
                                    last_error: e.to_string(),
                                };
                                enqueue_failed_email(ctx, failed_record);
                            }
                        },
                    }
                } else {
                    error!("Failed to send email, mailer not found, uri = {}", msg_indv.get_id());
                }
            },
            Err(e) => {
                error!("Failed to build email, id = {}, error: {}", msg_indv.get_id(), e);
                error!(
                    "Email configuration: from={}, to_count={}, has_subject={}, has_body={}, has_attachments={}",
                    mail_addreses.email_from.email,
                    mail_addreses.rr_email_to_hash.len(),
                    subject.is_some(),
                    message_body.is_some(),
                    attachments.is_some()
                );
            },
        }
    } else {
        error!("No valid recipients found for {}", msg_indv.get_id());
    }

    ResultCode::InternalServerError
}

fn build_email_message(
    mail_from: &str,
    recipients: &[String],
    subject: &Option<String>,
    message_body: &Option<String>,
    attachments: &[String],
    backend: &mut Backend,
    log_id: &str,
) -> Result<Message, String> {
    let from_address = Address::from_str(mail_from)
        .map_err(|e| format!("invalid sender email {}: {}", mail_from, e))?;
    let from_mailbox = Mailbox::new(Some("Veda System".to_string()), from_address);

    let mut message_builder = Message::builder()
        .from(from_mailbox)
        .header(header::ContentTransferEncoding::QuotedPrintable);

    let mut valid_recipients = 0usize;
    for recipient in recipients {
        match Address::from_str(recipient) {
            Ok(address) => {
                message_builder = message_builder.to(Mailbox::new(Some("Recipient".to_string()), address));
                valid_recipients += 1;
            },
            Err(e) => {
                error!("Invalid recipient {} for {}: {}", recipient, log_id, e);
            },
        }
    }

    if valid_recipients == 0 {
        return Err("no valid recipients".to_string());
    }

    if let Some(s) = subject {
        message_builder = message_builder.subject(s.clone());
    }

    if !attachments.is_empty() {
        let mut builder = MultiPart::mixed().build();

        if let Some(body) = message_body {
            let is_html = body.to_lowercase().contains("<html>");
            let body_part = if is_html {
                SinglePart::builder()
                    .header(header::ContentType::parse("text/html; charset=utf-8").map_err(|e| e.to_string())?)
                    .header(header::ContentTransferEncoding::QuotedPrintable)
                    .body(body.to_string())
            } else {
                SinglePart::builder()
                    .header(header::ContentType::parse("text/plain; charset=utf-8").map_err(|e| e.to_string())?)
                    .header(header::ContentTransferEncoding::QuotedPrintable)
                    .body(body.to_string())
            };
            builder = builder.singlepart(body_part);
        }

        for id in attachments {
            if let Some(file_info) = backend.get_individual(id, &mut Individual::default()) {
                if let (Some(path), Some(file_uri), Some(file_name)) = (
                    file_info.get_first_literal("v-s:filePath"),
                    file_info.get_first_literal("v-s:fileUri"),
                    file_info.get_first_literal("v-s:fileName"),
                ) {
                    if !path.is_empty() {
                        let full_path = format!("{}/{}/{}", ATTACHMENTS_DB_PATH, path, file_uri);
                        match std::fs::read(&full_path) {
                            Ok(content) => {
                                let mime = mime_guess::from_path(&file_name).first_or_octet_stream();
                                let content_type = header::ContentType::parse(mime.essence_str())
                                    .unwrap_or_else(|_| header::ContentType::parse("application/octet-stream").unwrap());

                                let attachment = SinglePart::builder()
                                    .header(content_type)
                                    .header(header::ContentDisposition::attachment(&file_name))
                                    .header(header::ContentTransferEncoding::Base64)
                                    .body(content);
                                builder = builder.singlepart(attachment);
                            },
                            Err(e) => {
                                error!("Failed to read attachment {} for {}: {}", full_path, log_id, e);
                            },
                        }
                    }
                }
            }
        }

        message_builder.multipart(builder).map_err(|e| e.to_string())
    } else if let Some(body) = message_body {
        let is_html = body.to_lowercase().contains("<html>");
        if is_html {
            message_builder
                .header(header::ContentType::parse("text/html; charset=utf-8").map_err(|e| e.to_string())?)
                .header(header::ContentTransferEncoding::QuotedPrintable)
                .body(body.to_string())
                .map_err(|e| e.to_string())
        } else {
            message_builder
                .header(header::ContentType::parse("text/plain; charset=utf-8").map_err(|e| e.to_string())?)
                .header(header::ContentTransferEncoding::QuotedPrintable)
                .body(body.to_string())
                .map_err(|e| e.to_string())
        }
    } else {
        message_builder
            .header(header::ContentType::parse("text/plain; charset=utf-8").map_err(|e| e.to_string())?)
            .header(header::ContentTransferEncoding::QuotedPrintable)
            .body(String::new())
            .map_err(|e| e.to_string())
    }
}

fn process_retry_queue(backend: &mut Backend, ctx: &mut Context) {
    if !ctx.retry_config.enabled {
        return;
    }

    let now = now_ts();
    if ctx.last_retry_check_ts > 0
        && now.saturating_sub(ctx.last_retry_check_ts) < ctx.retry_config.heartbeat_check
    {
        return;
    }
    ctx.last_retry_check_ts = now;

    let mut records = load_pending_records(&ctx.retry_storage);
    if records.is_empty() {
        return;
    }

    let mut kept_records = Vec::new();
    for mut record in records.drain(..) {
        if record.next_retry_at > now {
            kept_records.push(record);
            continue;
        }

        if record.attempt_count >= ctx.retry_config.max_attempts {
            append_dead_letter(
                &ctx.retry_storage,
                "max_attempts_reached".to_string(),
                Some(record),
                None,
            );
            continue;
        }

        if now.saturating_sub(record.first_failed_at) >= ctx.retry_config.max_total {
            append_dead_letter(
                &ctx.retry_storage,
                "max_total_time_reached".to_string(),
                Some(record),
                None,
            );
            continue;
        }

        let msg = match build_email_message(
            &record.from,
            &record.to,
            &record.subject,
            &record.body,
            &record.attachments,
            backend,
            &record.msg_uri,
        ) {
            Ok(m) => m,
            Err(e) => {
                append_dead_letter(
                    &ctx.retry_storage,
                    format!("build_failed: {}", e),
                    Some(record),
                    None,
                );
                continue;
            },
        };

        let mailer = match &ctx.smtp_client {
            Some(m) => m,
            None => {
                record.attempt_count = record.attempt_count.saturating_add(1);
                record.last_error = "mailer_not_found".to_string();
                record.next_retry_at = compute_next_retry_at(now, record.attempt_count, &ctx.retry_config);
                kept_records.push(record);
                continue;
            },
        };

        match mailer.send(&msg) {
            Ok(_) => {
                info!(
                    "Retry email sent successfully: msg_uri={}, attempt_count={}",
                    record.msg_uri, record.attempt_count
                );
            },
            Err(e) => {
                record.attempt_count = record.attempt_count.saturating_add(1);
                record.last_error = e.to_string();
                if record.attempt_count >= ctx.retry_config.max_attempts
                    || now.saturating_sub(record.first_failed_at) >= ctx.retry_config.max_total
                {
                    append_dead_letter(
                        &ctx.retry_storage,
                        format!("retry_failed: {}", record.last_error),
                        Some(record),
                        None,
                    );
                } else {
                    record.next_retry_at = compute_next_retry_at(now, record.attempt_count, &ctx.retry_config);
                    kept_records.push(record);
                }
            },
        }
    }

    save_pending_records_atomic(&ctx.retry_storage, &kept_records);
}

fn get_emails_from_appointment(has_message_type: &Option<String>, ap: &mut Individual, backend: &mut Backend) -> Vec<Mailbox> {
    if ap.any_exists("v-s:hasDelegationPurpose", &["d:delegate_Control"]) {
        return vec![];
    }

    let p_uri = ap.get_first_literal("v-s:employee").unwrap_or_default();
    if p_uri.is_empty() {
        return vec![];
    }

    let mut prs = Individual::default();
    if backend.get_individual(&p_uri, &mut prs).is_none() {
        return vec![];
    }

    if let Some(v) = prs.get_first_bool("v-s:deleted") {
        if v {
            return vec![];
        }
    }

    let label = ap.get_first_literal("rdfs:label").unwrap_or_default();

    if let Some(has_message_type) = has_message_type {
        if let Some(preference_uri) = prs.get_first_literal("v-ui:hasPreferences") {
            let preference_uri_str: &str = &preference_uri;
            if let Some(preference) = backend.get_individual(preference_uri_str, &mut Individual::default()) {
                info!("found preferences, uri = {}, has message type = {}", p_uri, has_message_type);

                let mut need_send = true;
                if let Some(receive_message_types) = preference.get_literals("v-ui:rejectMessageType") {
                    for msg_type in receive_message_types.iter() {
                        info!("check preferences {}", msg_type);
                        if !has_message_type.is_empty() && msg_type == has_message_type {
                            need_send = false;
                            break;
                        }

                        if !has_message_type.is_empty() && msg_type == "v-s:OtherNotification" {
                            need_send = false;
                            break;
                        }
                    }
                }

                if !need_send {
                    info!("declined to send message");
                    return vec![];
                }
            }
        }
    }

    let ac_uri = prs.get_first_literal("v-s:hasAccount");
    if ac_uri.is_none() {
        return vec![];
    }

    if let Some(ac) = backend.get_individual(&ac_uri.unwrap(), &mut Individual::default()) {
        if ac.is_exists_bool("v-s:deleted", true) {
            return vec![];
        }

        let mut res = vec![];
        for el in ac.get_literals("v-s:mailbox").unwrap_or_default() {
            if let Ok(address) = Address::from_str(&el) {
                res.push(Mailbox::new(Some(label.clone()), address));
            } else if let Some((user, domain)) = split_email_address(&el) {
                if let Ok(address) = Address::new(user, domain) {
                    res.push(Mailbox::new(Some(label.clone()), address));
                } else {
                    error!("failed to create email address: {}", el);
                }
            } else {
                error!("invalid email format: {}", el);
            }
        }
        res
    } else {
        vec![]
    }
}

fn extract_email(has_message_type: &Option<String>, ap_id: &str, ctx: &mut Context, backend: &mut Backend) -> Vec<Mailbox> {
    let mut res = Vec::new();

    if ap_id.is_empty() {
        return vec![];
    }

    if let Some(indv) = backend.get_individual(ap_id, &mut Individual::default()) {
        let label = indv.get_first_literal("rdfs:label").unwrap_or_default();

        if indv.any_exists("rdf:type", &["v-s:Appointment"]) {
            return get_emails_from_appointment(has_message_type, indv, backend);
        } else if indv.any_exists("rdf:type", &["v-s:Position"]) {
            let l_individuals = backend
                .fts
                .query(FTQuery::new_with_ticket(&ctx.sys_ticket, &("'rdf:type' == 'v-s:Appointment' && 'v-s:occupation' == '".to_string() + indv.get_id() + "'")));

            for id in l_individuals.result {
                if let Some(individual) = backend.get_individual(&id, &mut Individual::default()) {
                    if !individual.is_exists_bool("v-s:deleted", true) {
                        res.append(&mut get_emails_from_appointment(has_message_type, individual, backend));
                    }
                }
            }
        } else if indv.any_exists("rdf:type", &["v-s:Person"]) {
            for ac_uri in indv.get_literals("v-s:hasAccount").unwrap_or_default() {
                if ac_uri.is_empty() {
                    return vec![];
                }

                if let Some(ac) = backend.get_individual(&ac_uri, &mut Individual::default()) {
                    if !ac.is_exists_bool("v-s:delete", true) {
                        for el in ac.get_literals("v-s:mailbox").unwrap_or_default() {
                            if let Ok(address) = Address::from_str(&el) {
                                res.push(Mailbox::new(Some(label.clone()), address));
                            } else if let Some((user, domain)) = split_email_address(&el) {
                                if let Ok(address) = Address::new(user, domain) {
                                    res.push(Mailbox::new(Some(label.clone()), address));
                                } else {
                                    error!("failed to create email address: {}", el);
                                }
                            } else {
                                error!("invalid email format: {}", el);
                            }
                        }
                        return res;
                    }
                }
            }
        } else {
            error!("failed to extract email from {}, this is not an appointment, nor a position", ap_id);
        }
    }
    res
}

fn split_email_address(email: &str) -> Option<(String, String)> {
    let parts: Vec<&str> = email.split('@').collect();
    if parts.len() == 2 {
        Some((parts[0].to_string(), parts[1].to_string()))
    } else {
        None
    }
}

fn connect_to_smtp(ctx: &mut Context, module: &mut Backend) -> bool {
    if let Some(node) = module.get_individual("cfg:standart_node", &mut Individual::default()) {
        if let Some(v) = node.get_literals("v-s:send_an_email_individual_by_event") {
            for el in v {
                let mut connection = Individual::default();

                if module.storage.get_individual(&el, &mut connection).is_ok() && !connection.is_exists_bool("v-s:delete", true) {
                    if let Some(transport) = connection.get_first_literal("v-s:transport") {
                        if transport == "smtp" {
                            info!("found connection configuration for smtp server, uri = {}", connection.get_id());

                            let host = connection.get_first_literal("v-s:host").unwrap_or_default();
                            let port = connection.get_first_integer("v-s:port").unwrap_or(25);

                            if host.is_empty() {
                                error!("parameter [host] is empty");
                                return false;
                            }

                            let login = connection.get_first_literal("v-s:login");
                            let pass = connection.get_first_literal("v-s:password");
                            let use_smtp_utf8 = connection.get_first_bool("cfg:use_smtp_utf8").unwrap_or(true);

                            ctx.default_mail_sender = connection.get_first_literal("v-s:mailSender").unwrap_or_default();
                            ctx.always_use_mail_sender = connection.get_first_bool("v-s:alwaysUseMailSender").unwrap_or_default();

                            // Формируем URL для SMTP
                            let mut url = String::new();

                            // Определяем протокол и порт по умолчанию
                            let (protocol, default_port) = match port {
                                465 => ("smtps://", 465),
                                587 => ("smtp://", 587),
                                _ => ("smtp://", 25),
                            };

                            url.push_str(protocol);

                            // Добавляем credentials и механизм аутентификации если есть
                            if let (Some(login), Some(pass)) = (&login, &pass) {
                                let auth_mechanism = match connection.get_first_literal("v-s:authType").unwrap_or_default().as_str() {
                                    "PLAIN" => "AUTH=PLAIN",
                                    "LOGIN" => "AUTH=LOGIN",
                                    _ => "AUTH=PLAIN",
                                };

                                url.push_str(&urlencoding::encode(login));
                                url.push(':');
                                url.push_str(&urlencoding::encode(pass));
                                url.push('@');

                                if port != default_port {
                                    url.push_str(&host);
                                    url.push(':');
                                    url.push_str(&port.to_string());
                                } else {
                                    url.push_str(&host);
                                }

                                // Добавляем параметры
                                url.push('?');
                                url.push_str(auth_mechanism);

                                if port == 587 {
                                    url.push_str("&tls=required");
                                }

                                if use_smtp_utf8 {
                                    url.push_str("&utf8=yes");
                                }
                            } else {
                                url.push_str(&host);
                                if port != default_port {
                                    url.push(':');
                                    url.push_str(&port.to_string());
                                }

                                if port == 587 {
                                    url.push_str("?tls=required");
                                    if use_smtp_utf8 {
                                        url.push_str("&utf8=yes");
                                    }
                                } else if use_smtp_utf8 {
                                    url.push_str("?utf8=yes");
                                }
                            }

                            info!("Connecting to SMTP server with URL: {}", url);

                            // Создаем конфигурацию пула с разумными значениями по умолчанию
                            let pool_config = PoolConfig::new()
                                .max_size(20) // Максимум 20 соединений в пуле
                                .idle_timeout(Duration::from_secs(300)); // Таймаут простоя 5 минут

                            // Создаем транспорт
                            match SmtpTransport::from_url(&url) {
                                Ok(builder) => {
                                    let transport = builder
                                        .pool_config(pool_config)
                                        .timeout(Some(Duration::from_secs(10))) // Таймаут подключения 10 секунд
                                        .build();

                                    ctx.smtp_client = Some(transport);

                                    if ctx.always_use_mail_sender {
                                        info!("use always_use_mail_sender parameter");
                                    }
                                    if !ctx.default_mail_sender.is_empty() {
                                        info!("default mail sender = {:?}", ctx.default_mail_sender);
                                    }

                                    return true;
                                },
                                Err(e) => {
                                    error!("Failed to create SMTP transport from URL {}: {:?}", url, e);
                                    return false;
                                },
                            }
                        }
                    }
                }
            }
        }
    }

    error!("failed to find connection configuration for smtp server");
    false
}
