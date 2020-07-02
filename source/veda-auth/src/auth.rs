use crate::common::{create_new_credential, get_candidate_users_of_login, remove_secret, AuthConf, UserStat, EMPTY_SHA256_HASH};
use chrono::Utc;
use rand::{thread_rng, Rng};
use std::collections::HashMap;
use uuid::Uuid;
use v_api::app::ResultCode;
use v_api::IndvOp;
use v_ft_xapian::xapian_reader::XapianReader;
use v_module::module::{create_new_ticket, Module};
use v_module::ticket::Ticket;
use v_onto::datatype::Lang;
use v_onto::individual::Individual;

pub struct AuthWorkPlace<'a> {
    pub(crate) conf: &'a AuthConf,
    pub(crate) login: &'a str,
    pub(crate) password: &'a str,
    pub(crate) secret: &'a str,
    pub(crate) systicket: &'a str,
    pub(crate) xr: &'a mut XapianReader,
    pub(crate) module: &'a mut Module,
    pub(crate) suspicious: &'a mut HashMap<String, UserStat>,
}

impl<'a> AuthWorkPlace<'a> {
    pub(crate) fn authenticate(&mut self) -> Ticket {
        let mut ticket = Ticket::default();

        info!("authenticate, login={:?} password={:?}, secret={:?}", self.login, self.password, self.secret);

        if self.login.is_empty() || self.login.len() < 3 {
            return ticket;
        }

        if !self.secret.is_empty() && self.secret.len() > 5 && self.password == EMPTY_SHA256_HASH {
            ticket.result = ResultCode::EmptyPassword;
            return ticket;
        }

        if self.secret.is_empty() && self.secret.len() > 5 && (self.password.is_empty() || self.password.len() < 64) {
            ticket.result = ResultCode::InvalidPassword;
            return ticket;
        }

        if !self.secret.is_empty() && self.secret != "?" && self.secret.len() < 6 {
            ticket.result = ResultCode::InvalidSecret;
            return ticket;
        }

        let user_stat = self.suspicious.entry(self.login.to_owned()).or_insert(UserStat::default());
        info!("login={:?}, stat: {:?}", self.login, user_stat);

        if user_stat.wrong_count_login >= self.conf.failed_auth_attempts {
            if Utc::now().timestamp() - user_stat.last_wrong_login_date < self.conf.failed_auth_lock_period {
                ticket.result = ResultCode::TooManyRequests;
                error!("too many attempt of login");
                return ticket;
            } else {
                user_stat.wrong_count_login = 0;
                user_stat.last_wrong_login_date = Utc::now().timestamp();
            }
        }

        let candidate_account_ids = get_candidate_users_of_login(self.login, self.module, self.xr);
        if candidate_account_ids.result_code == ResultCode::Ok && candidate_account_ids.count > 0 {
            for account_id in &candidate_account_ids.result {
                if let Some(account) = self.module.get_individual(&account_id, &mut Individual::default()) {
                    account.parse_all();
                    let user_id = account.get_first_literal("v-s:owner").unwrap_or_default();
                    if user_id.is_empty() {
                        error!("user id is null, user_indv={}", account);
                        continue;
                    }

                    let user_login = account.get_first_literal("v-s:login").unwrap_or_default();
                    if user_login.is_empty() {
                        error!("user login {:?} not equal request login {}", user_login, self.login);
                        continue;
                    }

                    if user_login.to_lowercase() != self.login.to_lowercase() {
                        error!("user login {} not equal request login {}", user_login, self.login);
                        continue;
                    }

                    let mut person = Individual::default();
                    if self.module.get_individual(&user_id, &mut person).is_none() {
                        error!("user {} not found", user_id);
                        continue;
                    }

                    let now = Utc::now().naive_utc().timestamp();
                    let mut exist_password = String::default();
                    let mut edited = 0;
                    let mut credential = Individual::default();

                    match account.get_first_literal("v-s:usesCredential") {
                        Some(uses_credential_uri) => {
                            if let Some(_credential) = self.module.get_individual(&uses_credential_uri, &mut credential) {
                                _credential.parse_all();
                                exist_password = _credential.get_first_literal("v-s:password").unwrap_or_default();
                                edited = _credential.get_first_datetime("v-s:dateFrom").unwrap_or_default();
                            } else {
                                error!("fail read credential: {}", uses_credential_uri);
                                create_new_credential(self.systicket, self.module, &mut credential, account);
                            }
                        }
                        None => {
                            warn!("credential not found, create new");
                            exist_password = account.get_first_literal("v-s:password").unwrap_or_default();

                            create_new_credential(self.systicket, self.module, &mut credential, account);
                        }
                    }

                    //let origin = person.get_first_literal("v-s:origin");
                    let old_secret = credential.get_first_literal("v-s:secret").unwrap_or_default();

                    // PREPARE SECRET CODE
                    if !self.secret.is_empty() && self.secret.len() > 5 {
                        if old_secret.is_empty() {
                            error!("update password: secret not found, user={}", person.get_id());
                            ticket.result = ResultCode::InvalidSecret;
                            remove_secret(&mut credential, person.get_id(), self.module, self.systicket);
                            return ticket;
                        }

                        if self.secret != old_secret {
                            error!("request for update password: send secret not equal request secret {}, user={}", self.secret, person.get_id());
                            ticket.result = ResultCode::InvalidSecret;
                            remove_secret(&mut credential, person.get_id(), self.module, self.systicket);
                            return ticket;
                        }

                        let prev_secret_date = credential.get_first_datetime("v-s:SecretDateFrom").unwrap_or_default();
                        if now - prev_secret_date > self.conf.secret_lifetime {
                            ticket.result = ResultCode::SecretExpired;
                            error!("request new password, secret expired, login={} password={} secret={}", self.login, self.password, self.secret);
                            return ticket;
                        }

                        if exist_password == self.password {
                            error!("update password: now password equal previous password, reject. user={}", person.get_id());
                            ticket.result = ResultCode::NewPasswordIsEqualToOld;
                            remove_secret(&mut credential, person.get_id(), self.module, self.systicket);
                            return ticket;
                        }

                        if self.password == EMPTY_SHA256_HASH {
                            error!("update password: now password is empty, reject. user={}", person.get_id());
                            ticket.result = ResultCode::EmptyPassword;
                            remove_secret(&mut credential, person.get_id(), self.module, self.systicket);
                            return ticket;
                        }

                        if (now - edited > 0) && now - edited < self.conf.success_pass_change_lock_period {
                            ticket.result = ResultCode::Locked;
                            error!("request new password: too many requests, login={} password={} secret={}", self.login, self.password, self.secret);
                            return ticket;
                        }

                        // update password
                        credential.set_string("v-s:password", &self.password, Lang::NONE);
                        credential.set_datetime("v-s:dateFrom", now);
                        credential.remove("v-s:secret");
                        credential.remove("v-s:SecretDateFrom");

                        let res = self.module.api.update(&self.systicket, IndvOp::Put, &credential);
                        if res.result != ResultCode::Ok {
                            ticket.result = ResultCode::AuthenticationFailed;
                            error!("fail store new password {} for user, user={}", self.password, person.get_id());
                        } else {
                            create_new_ticket(self.login, &user_id, self.conf.ticket_lifetime, &mut ticket, &mut self.module.storage);
                            user_stat.attempt_change_pass = 0;
                            info!("update password {} for user, user={}", self.password, person.get_id());
                        }
                        return ticket;
                    } else {
                        // ATTEMPT AUTHENTICATION

                        let mut is_request_new_password = if self.conf.pass_lifetime > 0 && edited > 0 && now - edited > self.conf.pass_lifetime {
                            error!("password is old, lifetime > {} days, user={}", self.conf.pass_lifetime, account.get_id());
                            true
                        } else {
                            false
                        };

                        if self.secret == "?" {
                            warn!("request for new password, user={}", account.get_id());
                            is_request_new_password = true;
                        }

                        if is_request_new_password {
                            warn!("request new password, login={} password={} secret={}", self.login, self.password, self.secret);
                            ticket.result = ResultCode::PasswordExpired;

                            if (now - edited > 0) && now - edited < self.conf.success_pass_change_lock_period {
                                ticket.result = ResultCode::Locked;
                                error!("request new password: too many requests, login={} password={} secret={}", self.login, self.password, self.secret);
                                return ticket;
                            }

                            if user_stat.attempt_change_pass >= self.conf.failed_change_pass_attempts {
                                let prev_secret_date = credential.get_first_datetime("v-s:SecretDateFrom").unwrap_or_default();
                                if now - prev_secret_date < self.conf.failed_pass_change_lock_period {
                                    ticket.result = ResultCode::TooManyRequestsChangePassword;
                                    user_stat.wrong_count_login = self.conf.failed_auth_attempts + 1;
                                    user_stat.last_wrong_login_date = Utc::now().timestamp();
                                    error!("request new password, to many request, login={} password={} secret={}", self.login, self.password, self.secret);
                                    return ticket;
                                }

                                if now - user_stat.last_attempt_change_pass_date < self.conf.failed_pass_change_lock_period {
                                    error!("too many requests of change password");
                                    ticket.result = ResultCode::TooManyRequestsChangePassword;
                                    user_stat.wrong_count_login = self.conf.failed_auth_attempts + 1;
                                    user_stat.last_wrong_login_date = Utc::now().timestamp();
                                    return ticket;
                                } else {
                                    user_stat.attempt_change_pass = 0;
                                }
                            }

                            user_stat.attempt_change_pass += 1;
                            user_stat.last_attempt_change_pass_date = Utc::now().timestamp();

                            let n_secret = thread_rng().gen_range(100_000, 999_999).to_string();

                            credential.set_string("v-s:secret", &n_secret, Lang::NONE);
                            credential.set_datetime("v-s:SecretDateFrom", now);

                            let res = self.module.api.update(&self.systicket, IndvOp::Put, &credential);
                            if res.result != ResultCode::Ok {
                                ticket.result = ResultCode::InternalServerError;
                                error!("fail store new secret, user={}, result={:?}", person.get_id(), res);
                                return ticket;
                            }

                            let mailbox = account.get_first_literal("v-s:mailbox").unwrap_or_default();

                            if !mailbox.is_empty() && mailbox.len() > 3 {
                                let mut mail_with_secret = Individual::default();

                                let uuid1 = "d:mail_".to_owned() + &Uuid::new_v4().to_string();
                                mail_with_secret.set_id(&uuid1);
                                mail_with_secret.add_uri("rdf:type", "v-s:Email");
                                mail_with_secret.add_string("v-s:recipientMailbox", &mailbox, Lang::NONE);
                                mail_with_secret.add_datetime("v-s:created", now);
                                mail_with_secret.add_uri("v-s:creator", "cfg:VedaSystemAppointment");
                                mail_with_secret.add_uri("v-wf:from", "cfg:VedaSystemAppointment");
                                mail_with_secret.add_string("v-s:messageBody", &("your secret code is ".to_owned() + &n_secret), Lang::NONE);

                                let res = self.module.api.update(&self.systicket, IndvOp::Put, &mail_with_secret);
                                if res.result != ResultCode::Ok {
                                    ticket.result = ResultCode::AuthenticationFailed;
                                    error!("fail store email with new secret, user={}", account.get_id());
                                    return ticket;
                                } else {
                                    info!("send {} new secret {} to mailbox {}, user={}", mail_with_secret.get_id(), n_secret, mailbox, account.get_id())
                                }
                            } else {
                                error!("mailbox not found, user={}", account.get_id());
                            }
                            return ticket;
                        }

                        if !exist_password.is_empty() && !self.password.is_empty() && self.password.len() > 63 && exist_password == self.password {
                            create_new_ticket(self.login, &user_id, self.conf.ticket_lifetime, &mut ticket, &mut self.module.storage);
                            user_stat.wrong_count_login = 0;
                            user_stat.last_wrong_login_date = 0;
                            return ticket;
                        } else {
                            user_stat.wrong_count_login += 1;
                            user_stat.last_wrong_login_date = Utc::now().timestamp();
                            warn!("request passw not equal with exist, user={}", account.get_id());
                        }
                    }
                    warn!("user {} not pass", account.get_id());
                } else {
                    error!("fail read, uri={}", &account_id);
                }
            }
        }

        error!("fail authenticate, login={} password={}, candidate users={:?}", self.login, self.password, candidate_account_ids.result);
        ticket.result = ResultCode::AuthenticationFailed;
        ticket
    }
}
