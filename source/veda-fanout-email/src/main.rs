#[macro_use]
extern crate log;

use lettre::smtp::authentication::{Credentials, Mechanism};
use lettre::smtp::ConnectionReuseParameters;
use lettre::{ClientSecurity, SmtpClient, SmtpTransport};
use v_api::ResultCode;
use v_module::info::ModuleInfo;
use v_module::module::*;
use v_module::onto::load_onto;
use v_onto::individual::*;
use v_onto::onto::Onto;
use v_queue::consumer::*;
use v_search::{FTQuery};

pub struct Context {
    onto: Onto,
    smtp_client: Option<SmtpTransport>,
    default_mail_sender: String,
    always_use_mail_sender: bool,
    sys_ticket: String,
}

fn main() -> Result<(), i32> {
    init_log();

    let mut queue_consumer = Consumer::new("./data/queue", "fanout-email2", "individuals-flow").expect("!!!!!!!!! FAIL QUEUE");

    let module_info = ModuleInfo::new("./data", "fanout_email2", true);
    if module_info.is_err() {
        error!("{:?}", module_info.err());
        return Err(-1);
    }

    let mut module = Module::default();
    let systicket = module.get_sys_ticket_id();

    let mut ctx = Context {
        onto: Onto::default(),
        smtp_client: None,
        default_mail_sender: String::default(),
        always_use_mail_sender: false,
        sys_ticket: systicket.unwrap_or_default(),
    };

    connect_to_smtp(&mut ctx, &mut module);

    info!("load onto start");
    load_onto(&mut module.fts, &mut module.storage, &mut ctx.onto);
    info!("load onto end");

    module.listen_queue(
        &mut queue_consumer,
        &mut module_info.unwrap(),
        &mut ctx,
        &mut (before_bath as fn(&mut Module, &mut Context)),
        &mut (prepare as fn(&mut Module, &mut ModuleInfo, &mut Context, &mut Individual)),
        &mut (after_bath as fn(&mut Module, &mut Context)),
    );
    Ok(())
}

fn before_bath(_module: &mut Module, _ctx: &mut Context) {}

fn after_bath(_module: &mut Module, _ctx: &mut Context) {}

fn prepare(module: &mut Module, module_info: &mut ModuleInfo, ctx: &mut Context, queue_element: &mut Individual) {
    let cmd = get_cmd(queue_element);
    if cmd.is_none() {
        error!("cmd is none");
        return;
    }

    let op_id = queue_element.get_first_integer("op_id").unwrap_or_default();

    let mut prev_state = Individual::default();
    get_inner_binobj_as_individual(queue_element, "prev_state", &mut prev_state);

    let mut new_state = Individual::default();
    get_inner_binobj_as_individual(queue_element, "new_state", &mut new_state);

    if let Err(e) = module_info.put_info(op_id, op_id) {
        error!("fail write module_info, op_id={}, err={:?}", op_id, e)
    }

    if let Some(types) = new_state.get_literals("rdf:type") {
        for itype in types {
            if ctx.onto.is_some_entered(&itype, &["v-s:Deliverable"]) {
                prepare_deliverable(&mut new_state, module, ctx);
                break;
            }
        }
    }
}

fn prepare_deliverable(new_indv: &mut Individual, module: &mut Module, ctx: &mut Context) -> ResultCode {
    let is_deleted = new_indv.is_exists("v-s:deleted");

    if is_deleted {
        info!("new_indv {} is deleted, ignore it", new_indv.get_id());
        return ResultCode::Ok;
    }

    let is_draft_of = new_indv.get_first_literal("v-s:is_draft_of");
    let actual_version = new_indv.get_first_literal("v-s:actual_version").unwrap_or_default();

    if is_draft_of.is_some() {
        info!("new_indv {} is draft, ignore it", new_indv.get_id());
        return ResultCode::Ok;
    }

    if actual_version.is_empty() && actual_version != new_indv.get_id() {
        info!("new{}.v-s:actual_version{} != {}, ignore", new_indv.get_id(), &actual_version, new_indv.get_id());
        return ResultCode::Ok;
    }

    let hasMessageType = new_indv.get_first_literal("v-s:hasMessageType");

    let mut from = new_indv.get_first_literal("v-wf:from").unwrap_or_default();
    let to = new_indv.get_literals("v-wf:to");
    let subject = new_indv.get_first_literal("v-s:subject");
    let reply_to = new_indv.get_literals("v-wf:replyTo");
    let message_body = new_indv.get_first_literal("v-s:messageBody");

    let senderMailbox = new_indv.get_first_literal("v-s:senderMailbox").unwrap_or_default();
    let recipientMailbox = new_indv.get_literals("v-s:recipientMailbox");
    let attachments = new_indv.get_literals("v-s:attachment");

    if ctx.always_use_mail_sender {
        info!("use always_use_mail_sender");
    }

    if !ctx.default_mail_sender.is_empty() {
        info!("default mail sender: {:?}", ctx.default_mail_sender);
    }

    if from.is_empty() && senderMailbox.is_empty() && !ctx.default_mail_sender.is_empty() {
        from = ctx.default_mail_sender.to_string();
    }

    if (!from.is_empty() || senderMailbox.is_empty() || !ctx.default_mail_sender.is_empty()) && (to.is_some() || recipientMailbox.is_some()) {
        //let from_label;
        let mut email_from;

        if ctx.always_use_mail_sender == true && !ctx.default_mail_sender.is_empty() && ctx.default_mail_sender.len() > 5 {
            info!("use default mail sender: {}", ctx.default_mail_sender);
            email_from = ctx.default_mail_sender.to_string();
        } else {
            info!("extract [from], {}", from);
            email_from = "extract_email(sticket, null, from, from_label).getFirstString()".to_owned();

            if (email_from.is_empty() || email_from.len() < 5) && !ctx.default_mail_sender.is_empty() {
                let emails_from = extract_email(None, &ctx.default_mail_sender.to_string(), ctx, module);
            }

            if (email_from.is_empty() || email_from.len() < 5) && !senderMailbox.is_empty() {
                email_from = senderMailbox;
            }
        }
    }

    /*
        let email = Email::builder()
            // Addresses can be specified by the tuple (email, alias)
            .to(("from@cc.com", "Firstname Lastname"))
            // ... or by an address only
            .from("to@cc.com")
            .subject("Hi, Hello world, Тест заголовок")
            .text("Hello world. Тест Текст")
            .attachment_from_file(Path::new("sdfgg.jpg"), None, &IMAGE_JPEG)
            .unwrap()
            .build()
            .unwrap();

        if let Some(mailer) = &mut ctx.smtp_client {
            if let Err(e) = &mailer.send(email.into()) {
                error!("fail send, err={}", e);
            }
        }
    */
    ResultCode::InternalServerError
}

fn get_email_from_appointment(has_message_type: Option<&str>, ap: &mut Individual, module: &mut Module) -> Vec<String> {
    let p_uri = ap.get_first_literal("v-s:employee").unwrap_or_default();
    if p_uri.is_empty() {
        return vec![];
    }

    let mut prs = Individual::default();
    if module.get_individual(&p_uri, &mut prs).is_none() {
        return vec![];
    }

    if let Some(v) = prs.get_first_bool("v-s:deleted") {
        if v {
            return vec![];
        }
    }

    if let Some(has_message_type) = has_message_type {
        if let Some(preference_uri) = prs.get_first_literal("v-ui:hasPreferences") {
            if let Some(preference) = module.get_individual(&preference_uri, &mut Individual::default()) {
                info!("for {} found preferences, has message type {}", p_uri, has_message_type);

                let mut need_send = true;
                if let Some(receive_message_types) = preference.get_literals("v-ui:rejectMessageType") {
                    for msg_type in receive_message_types {
                        info!("check preference {}", msg_type);
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
                    info!("decline send message");
                    return vec![];
                }
            }
        }
    }

    let ac_uri = prs.get_first_literal("v-s:hasAccount");
    if ac_uri.is_none() {
        return vec![];
    }

    if let Some(ac) = module.get_individual(&ac_uri.unwrap(), &mut Individual::default()) {
        if ac.is_exists_bool("v-s:deleted", true) {
            return vec![];
        }
        return ac.get_literals("v-s:mailbox").unwrap_or_default();
    } else {
        return vec![];
    }
}

fn extract_email(has_message_type: Option<&str>, ap_id: &str, ctx: &mut Context, module: &mut Module) -> (Vec<String>, String) {
    let mut res = Vec::new();
    let mut label = String::default();
    if ap_id.is_empty() || ap_id.len() < 1 {
        return (vec![], String::default());
    }

    if let Some(indv) = module.get_individual(ap_id, &mut Individual::default()) {
        label = indv.get_first_literal("rdfs:label").unwrap_or_default();

        if indv.any_exists("rdf:type", &["v-s:Appointment"]) {
            return (get_email_from_appointment(has_message_type, indv, module), label);
        } else if indv.any_exists("rdf:type", &["v-s:Position"]) {
            let l_individuals = module
                .fts
                .query(FTQuery::new_with_ticket(&ctx.sys_ticket, &("'rdf:type' == 'v-s:Appointment' && 'v-s:occupation' == '".to_string() + indv.get_id() + "'")));

            for id in l_individuals.result {
                if let Some(individual) = module.get_individual(&id, &mut Individual::default()) {
                    if !individual.is_exists_bool("v-s:deleted", true) {
                        res.append(&mut get_email_from_appointment(has_message_type, individual, module));
                    }
                }
            }
        } else if indv.any_exists("rdf:type", &["v-s:Person"]) {
            for ac_uri in indv.get_literals("v-s:hasAccount").unwrap_or_default() {
                if ac_uri.is_empty() {
                    return (vec![], String::default());
                }

                if let Some(ac) = module.get_individual(&ac_uri, &mut Individual::default()) {
                    if !ac.is_exists_bool("v-s:delete", true) {
                        if let Some(m) = ac.get_literals("v-s:mailbox") {
                            return (m, label);
                        }
                    }
                }
            }
        } else {
            error!("extract_email: fail extract email from {}, this not appointment or position", ap_id);
        }
    }
    return (res, label);
}

fn connect_to_smtp(ctx: &mut Context, module: &mut Module) -> bool {
    if let Some(node) = module.get_individual("cfg:standart_node", &mut Individual::default()) {
        if let Some(v) = node.get_literals("v-s:send_an_email_individual_by_event") {
            for el in v {
                let mut connection = Individual::default();

                if module.storage.get_individual(&el, &mut connection) && !connection.is_exists_bool("v-s:delete", true) {
                    if let Some(transport) = connection.get_first_literal("v-s:transport") {
                        if transport == "smtp" {
                            info!("found connect to smtp {}", connection.get_id());

                            let host = connection.get_first_literal("v-s:host").unwrap_or_default();
                            let port = connection.get_first_integer("v-s:port").unwrap_or(25);

                            if host.is_empty() {
                                error!("param [host] is empty");
                                return false;
                            }

                            let login = connection.get_first_literal("v-s:login");
                            let pass = connection.get_first_literal("v-s:password");

                            ctx.default_mail_sender = connection.get_first_literal("v-s:mailSender").unwrap_or_default();
                            ctx.always_use_mail_sender = connection.get_first_bool("v-s:alwaysUseMailSender").unwrap_or_default();

                            let client = SmtpClient::new(host.to_owned() + ":" + &port.to_string(), ClientSecurity::None);
                            if let Err(e) = client {
                                error!("fail connect to {}:{}, err={}", host, port, e);
                                return false;
                            }

                            if login.is_some() && pass.is_some() {
                                let auth_type = match connection.get_first_literal("v-s:authType").unwrap_or_default().as_str() {
                                    "PLAIN" => Mechanism::Plain,
                                    "LOGIN" => Mechanism::Login,
                                    _ => Mechanism::Plain,
                                };

                                // connect with auth
                                ctx.smtp_client = Some(
                                    client
                                        .unwrap()
                                        .credentials(Credentials::new(login.unwrap(), pass.unwrap()))
                                        .smtp_utf8(true)
                                        .authentication_mechanism(auth_type)
                                        .connection_reuse(ConnectionReuseParameters::ReuseUnlimited)
                                        .transport(),
                                );
                                return true;
                            } else {
                                // no security connect
                                ctx.smtp_client = Some(client.unwrap().smtp_utf8(true).transport());
                                return true;
                            }
                        }
                    }
                }
            }
        }
    }

    error!("not found configuration for connection to smtp server");
    false
}
