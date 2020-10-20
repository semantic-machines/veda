# ProcessDefinitionSuspensionStateDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**suspended** | Option<**bool**> | A `Boolean` value which indicates whether to activate or suspend all process definitions with the given key. When the value is set to `true`, all process definitions with the given key will be suspended and when the value is set to `false`, all process definitions with the given key will be activated. | [optional]
**process_definition_id** | Option<**String**> | The id of the process definitions to activate or suspend. | [optional]
**process_definition_key** | Option<**String**> |  The key of the process definitions to activate or suspend. | [optional]
**include_process_instances** | Option<**bool**> | A `Boolean` value which indicates whether to activate or suspend also all process instances of  the process definitions with the given key. When the value is set to `true`, all process instances of the process definitions with the given key will be activated or suspended and when the value is set to `false`, the suspension state of  all process instances of the process definitions with the given key will not be updated. | [optional]
**execution_date** | Option<**String**> | The date on which all process definitions with the given key will be activated or suspended. If `null`, the suspension state of all process definitions with the given key is updated immediately. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


