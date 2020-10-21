/*
 * Camunda BPM REST API
 *
 * OpenApi Spec for Camunda BPM REST API.
 *
 * The version of the OpenAPI document: 7.14.0
 * 
 * Generated by: https://openapi-generator.tech
 */




#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct AuthorizationExceptionDtoAllOf {
    /// The id of the user that does not have expected permissions
    #[serde(rename = "userId", skip_serializing_if = "Option::is_none")]
    pub user_id: Option<String>,
    #[serde(rename = "missingAuthorizations", skip_serializing_if = "Option::is_none")]
    pub missing_authorizations: Option<Vec<crate::models::MissingAuthorizationDto>>,
}

impl AuthorizationExceptionDtoAllOf {
    pub fn new() -> AuthorizationExceptionDtoAllOf {
        AuthorizationExceptionDtoAllOf {
            user_id: None,
            missing_authorizations: None,
        }
    }
}

