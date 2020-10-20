# SetJobRetriesByProcessDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**process_instances** | Option<**Vec<String>**> | A list of process instance ids to fetch jobs, for which retries will be set. | [optional]
**retries** | Option<**i32**> | An integer representing the number of retries. Please note that the value cannot be negative or null. | [optional]
**process_instance_query** | Option<[**crate::models::ProcessInstanceQueryDto**](ProcessInstanceQueryDto.md)> |  | [optional]
**historic_process_instance_query** | Option<[**crate::models::HistoricProcessInstanceQueryDto**](HistoricProcessInstanceQueryDto.md)> |  | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


