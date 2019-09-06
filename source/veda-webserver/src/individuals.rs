use actix_web::{web, Error, HttpRequest, HttpResponse};
use futures::future;
use futures::future::Future;
use rusty_tarantool::tarantool::Client;
use serde_json::value::Value as JSONValue;

#[derive(Debug, Deserialize, PartialEq, Serialize)]
struct GetIndividualResponse(JSONValue);

#[derive(Debug, Deserialize, PartialEq, Serialize)]
pub struct GetIndividualRequest {
    ticket: Option<String>,
    uri: Option<String>,
}

pub fn get_individual_handler(req: HttpRequest, params: web::Query<GetIndividualRequest>) -> Box<dyn Future<Item = HttpResponse, Error = Error>> {
    let tarantool = req.app_data::<Client>().unwrap();
    //println!("call tarantool! {:?}", params);
    let response = tarantool
        .call_fn2("libtarantool_veda.get_individual", &params.ticket, &params.uri)
        .and_then(move |response| Ok(GetIndividualResponse(response.decode_single()?)))
        .map(|result| HttpResponse::Ok().json(result))
        .or_else(move |err| {
            let body = format!("Internal error: {}", err.to_string());
            future::ok::<_, Error>(HttpResponse::InternalServerError().body(body))
        });
    Box::new(response)
}
