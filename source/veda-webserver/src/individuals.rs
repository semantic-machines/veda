use actix_web::{web, Error, HttpRequest, HttpResponse};
use futures::future;
use futures::future::Future;

use rusty_tarantool::tarantool::Client;

#[derive(Debug, Deserialize, PartialEq, Serialize)]
struct GetIndividualResponse {
    acl: Vec<u8>,
}

#[derive(Debug, Deserialize, PartialEq, Serialize)]
pub struct GetIndividualRequest {
    user: Option<String>,
    uri: Option<String>,
    crud: Option<u8>,
}

pub fn get_individual_handler(req: HttpRequest, params: web::Query<GetIndividualRequest>) -> Box<dyn Future<Item = HttpResponse, Error = Error>> {
    let tarantool = req.app_data::<Client>().unwrap();
    println!("call tarantool! {:?}", params);
    let response = tarantool
        .call_fn3("libtarantool_authorization.authorization", &params.uri, &params.user, &params.crud)
        .and_then(move |response| {
            Ok(GetIndividualResponse {
                acl: response.decode_single()?,
            })
        })
        .map(|result| HttpResponse::Ok().json(result))
        .or_else(move |err| {
            let body = format!("Internal error: {}", err.to_string());
            future::ok::<_, Error>(HttpResponse::InternalServerError().body(body))
        });
    Box::new(response)
}
