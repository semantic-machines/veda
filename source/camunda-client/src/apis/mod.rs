use reqwest;
use serde_json;

#[derive(Debug)]
pub enum Error {
    Reqwest(reqwest::Error),
    Serde(serde_json::Error),
    Io(std::io::Error),
}

impl From<reqwest::Error> for Error {
    fn from(e: reqwest::Error) -> Self {
        Error::Reqwest(e)
    }
}

impl From<serde_json::Error> for Error {
    fn from(e: serde_json::Error) -> Self {
        Error::Serde(e)
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self {
        Error::Io(e)
    }
}

pub fn urlencode<T: AsRef<str>>(s: T) -> String {
    ::url::form_urlencoded::byte_serialize(s.as_ref().as_bytes()).collect()
}

mod condition_api;
pub use self::condition_api::{ ConditionApi, ConditionApiClient };
mod deployment_api;
pub use self::deployment_api::{ DeploymentApi, DeploymentApiClient };
mod engine_api;
pub use self::engine_api::{ EngineApi, EngineApiClient };
mod event_subscription_api;
pub use self::event_subscription_api::{ EventSubscriptionApi, EventSubscriptionApiClient };
mod external_task_api;
pub use self::external_task_api::{ ExternalTaskApi, ExternalTaskApiClient };
mod historic_activity_instance_api;
pub use self::historic_activity_instance_api::{ HistoricActivityInstanceApi, HistoricActivityInstanceApiClient };
mod historic_process_instance_api;
pub use self::historic_process_instance_api::{ HistoricProcessInstanceApi, HistoricProcessInstanceApiClient };
mod incident_api;
pub use self::incident_api::{ IncidentApi, IncidentApiClient };
mod message_api;
pub use self::message_api::{ MessageApi, MessageApiClient };
mod metrics_api;
pub use self::metrics_api::{ MetricsApi, MetricsApiClient };
mod process_definition_api;
pub use self::process_definition_api::{ ProcessDefinitionApi, ProcessDefinitionApiClient };
mod process_instance_api;
pub use self::process_instance_api::{ ProcessInstanceApi, ProcessInstanceApiClient };
mod schema_log_api;
pub use self::schema_log_api::{ SchemaLogApi, SchemaLogApiClient };
mod signal_api;
pub use self::signal_api::{ SignalApi, SignalApiClient };
mod task_api;
pub use self::task_api::{ TaskApi, TaskApiClient };
mod task_attachment_api;
pub use self::task_attachment_api::{ TaskAttachmentApi, TaskAttachmentApiClient };
mod task_comment_api;
pub use self::task_comment_api::{ TaskCommentApi, TaskCommentApiClient };
mod task_identity_link_api;
pub use self::task_identity_link_api::{ TaskIdentityLinkApi, TaskIdentityLinkApiClient };
mod task_local_variable_api;
pub use self::task_local_variable_api::{ TaskLocalVariableApi, TaskLocalVariableApiClient };
mod task_variable_api;
pub use self::task_variable_api::{ TaskVariableApi, TaskVariableApiClient };
mod telemetry_api;
pub use self::telemetry_api::{ TelemetryApi, TelemetryApiClient };
mod user_api;
pub use self::user_api::{ UserApi, UserApiClient };
mod version_api;
pub use self::version_api::{ VersionApi, VersionApiClient };

pub mod configuration;
pub mod client;
