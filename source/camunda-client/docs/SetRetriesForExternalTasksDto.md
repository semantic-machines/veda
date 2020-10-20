# SetRetriesForExternalTasksDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**retries** | Option<**i32**> | The number of retries to set for the external task.  Must be >= 0. If this is 0, an incident is created and the task cannot be fetched anymore unless the retries are increased again. Can not be null. | [optional]
**external_task_ids** | Option<**Vec<String>**> | The ids of the external tasks to set the number of retries for. | [optional]
**process_instance_ids** | Option<**Vec<String>**> | The ids of process instances containing the tasks to set the number of retries for. | [optional]
**external_task_query** | Option<[**crate::models::ExternalTaskQueryDto**](ExternalTaskQueryDto.md)> |  | [optional]
**process_instance_query** | Option<[**crate::models::ProcessInstanceQueryDto**](ProcessInstanceQueryDto.md)> |  | [optional]
**historic_process_instance_query** | Option<[**crate::models::HistoricProcessInstanceQueryDto**](HistoricProcessInstanceQueryDto.md)> |  | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


