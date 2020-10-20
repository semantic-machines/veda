# DeleteProcessInstancesDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**process_instance_ids** | Option<**Vec<String>**> | A list process instance ids to delete. | [optional]
**delete_reason** | Option<**String**> | A string with delete reason. | [optional]
**skip_custom_listeners** | Option<**bool**> | Skip execution listener invocation for activities that are started or ended as part of this request. | [optional]
**skip_subprocesses** | Option<**bool**> | Skip deletion of the subprocesses related to deleted processes as part of this request. | [optional]
**process_instance_query** | Option<[**crate::models::ProcessInstanceQueryDto**](ProcessInstanceQueryDto.md)> |  | [optional]
**historic_process_instance_query** | Option<[**crate::models::HistoricProcessInstanceQueryDto**](HistoricProcessInstanceQueryDto.md)> |  | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


