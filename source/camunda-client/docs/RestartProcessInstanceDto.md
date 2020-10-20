# RestartProcessInstanceDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**process_instance_ids** | Option<**Vec<String>**> | A list of process instance ids to restart. | [optional]
**historic_process_instance_query** | Option<[**crate::models::HistoricProcessInstanceQueryDto**](HistoricProcessInstanceQueryDto.md)> |  | [optional]
**skip_custom_listeners** | Option<**bool**> | Skip execution listener invocation for activities that are started as part of this request. | [optional]
**skip_io_mappings** | Option<**bool**> | Skip execution of [input/output variable mappings](https://docs.camunda.org/manual/7.14/user-guide/process-engine/variables/#input-output-variable-mapping) for activities that are started as part of this request. | [optional]
**initial_variables** | Option<**bool**> | Set the initial set of variables during restart. By default, the last set of variables is used. | [optional]
**without_business_key** | Option<**bool**> | Do not take over the business key of the historic process instance. | [optional]
**instructions** | Option<[**Vec<crate::models::RestartProcessInstanceModificationInstructionDto>**](RestartProcessInstanceModificationInstructionDto.md)> | **Optional**. A JSON array of instructions that specify which activities to start the process instance at. If this property is omitted, the process instance starts at its default blank start event. | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


