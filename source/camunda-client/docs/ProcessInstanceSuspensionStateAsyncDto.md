# ProcessInstanceSuspensionStateAsyncDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**suspended** | Option<**bool**> | A Boolean value which indicates whether to activate or suspend a given process instance. When the value is set to true, the given process instance will be suspended and when the value is set to false, the given process instance will be activated. | [optional]
**process_instance_ids** | Option<**Vec<String>**> | A list of process instance ids which defines a group of process instances which will be activated or suspended by the operation. | [optional]
**process_instance_query** | Option<[**crate::models::ProcessInstanceQueryDto**](ProcessInstanceQueryDto.md)> |  | [optional]
**historic_process_instance_query** | Option<[**crate::models::HistoricProcessInstanceQueryDto**](HistoricProcessInstanceQueryDto.md)> |  | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


