# MessageCorrelationResultWithVariableDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**result_type** | Option<**String**> | Indicates if the message was correlated to a message start event or an  intermediate message catching event. In the first case, the resultType is  `ProcessDefinition` and otherwise `Execution`. | [optional]
**process_instance** | Option<[**crate::models::ProcessInstanceDto**](ProcessInstanceDto.md)> |  | [optional]
**execution** | Option<[**crate::models::ExecutionDto**](ExecutionDto.md)> |  | [optional]
**variables** | Option<[**::std::collections::HashMap<String, crate::models::VariableValueDto>**](VariableValueDto.md)> | This property is returned if the `variablesInResultEnabled` is set to `true`. Contains a list of the process variables.  | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


