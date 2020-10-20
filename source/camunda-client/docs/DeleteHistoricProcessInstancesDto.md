# DeleteHistoricProcessInstancesDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**historic_process_instance_ids** | Option<**Vec<String>**> | A list historic process instance ids to delete. | [optional]
**historic_process_instance_query** | Option<[**crate::models::HistoricProcessInstanceQueryDto**](HistoricProcessInstanceQueryDto.md)> |  | [optional]
**delete_reason** | Option<**String**> | A string with delete reason. | [optional]
**fail_if_not_exists** | Option<**bool**> | If set to `false`, the request will still be successful if one ore more of the process ids are not found. | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


