use crate::v8_script::OutValue;
use camunda_client::models::{CompleteExternalTaskDto, VariableValueDto};
use std::collections::HashMap;
use v_module::module::Module;

pub fn get_storage_init_param() -> String {
    let tarantool_addr = if let Some(p) = Module::get_property("tarantool_url") {
        p
    } else {
        warn!("param [tarantool_url] not found in veda.properties");
        "".to_owned()
    };

    if !tarantool_addr.is_empty() {
        info!("tarantool addr={}", &tarantool_addr);
    }
    tarantool_addr
}

pub fn out_value_2_complete_external_task(worker_id: &str, res: OutValue) -> CompleteExternalTaskDto {
    let mut out_data = CompleteExternalTaskDto::new();
    out_data.worker_id = Some(worker_id.to_owned());
    let mut vars = HashMap::new();
    if let OutValue::Json(j) = res {
        if let Some(mj) = j.as_object() {
            for (n, v) in mj {
                if let Some(ij) = v.as_object() {
                    let mut out_el = VariableValueDto::new();
                    if let Some(value) = ij.get("value") {
                        out_el.value = Some(value.to_owned());
                    }
                    if let Some(_type) = ij.get("type") {
                        if let Some(t) = _type.as_str() {
                            out_el._type = Some(t.to_owned());
                        }
                    }
                    vars.insert(n.to_owned(), out_el);
                }

                info!("out var [{}] = {:?}", n, v);
            }
            out_data.variables = Some(vars);
        }
    }
    //info!("@out_data={:?}", out_data);
    out_data
}
