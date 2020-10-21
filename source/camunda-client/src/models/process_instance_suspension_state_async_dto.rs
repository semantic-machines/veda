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
pub struct ProcessInstanceSuspensionStateAsyncDto {
    /// A Boolean value which indicates whether to activate or suspend a given process instance. When the value is set to true, the given process instance will be suspended and when the value is set to false, the given process instance will be activated.
    #[serde(rename = "suspended", skip_serializing_if = "Option::is_none")]
    pub suspended: Option<bool>,
    /// A list of process instance ids which defines a group of process instances which will be activated or suspended by the operation.
    #[serde(rename = "processInstanceIds", skip_serializing_if = "Option::is_none")]
    pub process_instance_ids: Option<Vec<String>>,
    #[serde(rename = "processInstanceQuery", skip_serializing_if = "Option::is_none")]
    pub process_instance_query: Option<crate::models::ProcessInstanceQueryDto>,
    #[serde(rename = "historicProcessInstanceQuery", skip_serializing_if = "Option::is_none")]
    pub historic_process_instance_query: Option<crate::models::HistoricProcessInstanceQueryDto>,
}

impl ProcessInstanceSuspensionStateAsyncDto {
    pub fn new() -> ProcessInstanceSuspensionStateAsyncDto {
        ProcessInstanceSuspensionStateAsyncDto {
            suspended: None,
            process_instance_ids: None,
            process_instance_query: None,
            historic_process_instance_query: None,
        }
    }
}

