use crate::Context;
use std::error::Error;
use std::fmt;
use v_api::app::generate_unique_uri;
use v_api::IndvOp;
use v_module::module::Module;
use v_onto::individual::Individual;

#[derive(Debug)]
pub struct MyError(pub String);

impl fmt::Display for MyError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "There is an error: {}", self.0)
    }
}

impl Error for MyError {}

pub fn store_work_order_into(uri: &str, work_order_uri: &str, systicket: &str, module: &mut Module) -> Result<(), Box<dyn Error>> {
    let indv = &mut Individual::default();
    indv.set_id(uri);
    indv.add_uri("bpmn:work_order", work_order_uri);

    module.api.update_or_err(systicket, "", "", IndvOp::SetIn, indv)?;
    info!("success update, uri={}", indv.get_id());

    Ok(())
}

pub fn store_is_completed_into(uri: &str, value: bool, systicket: &str, module: &mut Module) -> Result<(), Box<dyn Error>> {
    let indv = &mut Individual::default();
    indv.set_id(uri);
    indv.set_bool("bpmn:is_completed", value);

    module.api.update_or_err(systicket, "", "go-prepare", IndvOp::SetIn, indv)?;
    info!("success update, uri={}", indv.get_id());

    Ok(())
}

pub fn add_right(subj_uri: &str, obj_uri: &str, ctx: &mut Context, module: &mut Module) -> Result<(), Box<dyn Error>> {
    if subj_uri.is_empty() || obj_uri.is_empty() {
        return Err(Box::new(MyError("empty subj_uri or obj_uri".into())));
    }

    let mut right = Individual::default();
    right.set_id(&generate_unique_uri("wd:r_", ""));
    right.add_uri("rdf:type", "v-s:PermissionStatement");
    right.add_uri("v-s:permissionObject", subj_uri);
    right.add_uri("v-s:permissionSubject", obj_uri);

    module.api.update_or_err(&ctx.sys_ticket, "", "", IndvOp::Put, &right)?;
    info!("success update, uri={}", right.get_id());

    Ok(())
}
