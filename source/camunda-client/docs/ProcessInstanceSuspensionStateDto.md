# ProcessInstanceSuspensionStateDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**suspended** | Option<**bool**> | A `Boolean` value which indicates whether to activate or suspend a given process instance. When the value is set to `true`, the given process instance will be suspended and when the value is set to `false`, the given process instance will be activated. | [optional]
**process_definition_id** | Option<**String**> | The process definition id of the process instances to activate or suspend.  **Note**: This parameter can be used only with combination of `suspended`. | [optional]
**process_definition_key** | Option<**String**> | The process definition key of the process instances to activate or suspend.  **Note**: This parameter can be used only with combination of `suspended`, `processDefinitionTenantId`, and `processDefinitionWithoutTenantId`. | [optional]
**process_definition_tenant_id** | Option<**String**> | Only activate or suspend process instances of a process definition which belongs to a tenant with the given id.  **Note**: This parameter can be used only with combination of `suspended`, `processDefinitionKey`, and `processDefinitionWithoutTenantId`. | [optional]
**process_definition_without_tenant_id** | Option<**bool**> | Only activate or suspend process instances of a process definition which belongs to no tenant. Value may only be true, as false is the default behavior.  **Note**: This parameter can be used only with combination of `suspended`, `processDefinitionKey`, and `processDefinitionTenantId`. | [optional]
**process_instance_ids** | Option<**Vec<String>**> | A list of process instance ids which defines a group of process instances which will be activated or suspended by the operation.  **Note**: This parameter can be used only with combination of `suspended`, `processInstanceQuery`, and `historicProcessInstanceQuery`. | [optional]
**process_instance_query** | Option<[**crate::models::ProcessInstanceQueryDto**](ProcessInstanceQueryDto.md)> |  | [optional]
**historic_process_instance_query** | Option<[**crate::models::HistoricProcessInstanceQueryDto**](HistoricProcessInstanceQueryDto.md)> |  | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


