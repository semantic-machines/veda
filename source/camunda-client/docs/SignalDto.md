# SignalDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**name** | Option<**String**> | The name of the signal to deliver.  **Note**: This property is mandatory. | [optional]
**execution_id** | Option<**String**> | Optionally specifies a single execution which is notified by the signal.  **Note**: If no execution id is defined the signal is broadcasted to all subscribed handlers.  | [optional]
**variables** | Option<[**::std::collections::HashMap<String, crate::models::VariableValueDto>**](VariableValueDto.md)> | A JSON object containing variable key-value pairs. Each key is a variable name and each value a JSON variable value object. | [optional]
**tenant_id** | Option<**String**> | Specifies a tenant to deliver the signal. The signal can only be received on executions or process definitions which belongs to the given tenant.  **Note**: Cannot be used in combination with executionId. | [optional]
**without_tenant_id** | Option<**bool**> | If true the signal can only be received on executions or process definitions which belongs to no tenant. Value may not be false as this is the default behavior.  **Note**: Cannot be used in combination with `executionId`. | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


