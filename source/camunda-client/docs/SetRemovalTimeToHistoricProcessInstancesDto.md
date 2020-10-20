# SetRemovalTimeToHistoricProcessInstancesDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**historic_process_instance_ids** | Option<**Vec<String>**> | The id of the process instance. | [optional]
**historic_process_instance_query** | Option<[**crate::models::HistoricProcessInstanceQueryDto**](HistoricProcessInstanceQueryDto.md)> |  | [optional]
**hierarchical** | Option<**bool**> | Sets the removal time to all historic process instances in the hierarchy. Value may only be `true`, as `false` is the default behavior. | [optional]
**absolute_removal_time** | Option<**String**> | The date for which the instances shall be removed. Value may not be `null`.  **Note:** Cannot be set in conjunction with `clearedRemovalTime` or `calculatedRemovalTime`. | [optional]
**cleared_removal_time** | Option<**bool**> | Sets the removal time to `null`. Value may only be `true`, as `false` is the default behavior.  **Note:** Cannot be set in conjunction with `absoluteRemovalTime` or `calculatedRemovalTime`. | [optional]
**calculated_removal_time** | Option<**bool**> | The removal time is calculated based on the engine's configuration settings. Value may only be `true`, as `false` is the default behavior.  **Note:** Cannot be set in conjunction with `absoluteRemovalTime` or `clearedRemovalTime`. | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


