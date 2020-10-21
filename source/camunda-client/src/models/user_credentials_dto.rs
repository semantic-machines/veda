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
pub struct UserCredentialsDto {
    /// The users new password.
    #[serde(rename = "password", skip_serializing_if = "Option::is_none")]
    pub password: Option<String>,
    /// The password of the authenticated user who changes the password of the user (i.e., the user with passed id as path parameter).
    #[serde(rename = "authenticatedUserPassword", skip_serializing_if = "Option::is_none")]
    pub authenticated_user_password: Option<String>,
}

impl UserCredentialsDto {
    pub fn new() -> UserCredentialsDto {
        UserCredentialsDto {
            password: None,
            authenticated_user_password: None,
        }
    }
}

