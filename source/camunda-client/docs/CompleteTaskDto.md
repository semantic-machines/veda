# CompleteTaskDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**variables** | Option<[**::std::collections::HashMap<String, crate::models::VariableValueDto>**](VariableValueDto.md)> | A JSON object containing variable key-value pairs. | [optional]
**with_variables_in_return** | Option<**bool**> | Indicates whether the response should contain the process variables or not. The default is `false` with a response code of `204`. If set to `true` the response contains the process variables and has a response code of `200`. If the task is not associated with a process instance (e.g. if it's part of a case instance) no variables will be returned. | [optional][default to false]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


