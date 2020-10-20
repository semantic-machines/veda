# SetVariablesAsyncDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**process_instance_ids** | Option<**Vec<String>**> | A list of process instance ids that define a group of process instances to which the operation will set variables.  Please note that if `processInstanceIds`, `processInstanceQuery` and `historicProcessInstanceQuery` are defined, the resulting operation will be performed on the union of these sets. | [optional]
**process_instance_query** | Option<[**crate::models::ProcessInstanceQueryDto**](ProcessInstanceQueryDto.md)> |  | [optional]
**historic_process_instance_query** | Option<[**crate::models::HistoricProcessInstanceQueryDto**](HistoricProcessInstanceQueryDto.md)> |  | [optional]
**variables** | Option<[**::std::collections::HashMap<String, crate::models::VariableValueDto>**](VariableValueDto.md)> | A variables the operation will set in the root scope of the process instances. | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


