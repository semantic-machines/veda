# TransitionInstanceDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | Option<**String**> | The id of the transition instance. | [optional]
**parent_activity_instance_id** | Option<**String**> | The id of the parent activity instance, for example a sub process instance. | [optional]
**activity_id** | Option<**String**> | The id of the activity that this instance enters (asyncBefore job) or leaves (asyncAfter job) | [optional]
**activity_name** | Option<**String**> | The name of the activity that this instance enters (asyncBefore job) or leaves (asyncAfter job) | [optional]
**activity_type** | Option<**String**> | The type of the activity that this instance enters (asyncBefore job) or leaves (asyncAfter job) | [optional]
**process_instance_id** | Option<**String**> | The id of the process instance this instance is part of. | [optional]
**process_definition_id** | Option<**String**> | The id of the process definition. | [optional]
**execution_id** | Option<**String**> | The execution id. | [optional]
**incident_ids** | Option<**Vec<String>**> | A list of incident ids. | [optional]
**incidents** | Option<[**Vec<crate::models::ActivityInstanceIncidentDto>**](ActivityInstanceIncidentDto.md)> | A list of JSON objects containing incident specific properties: * `id`: the id of the incident * `activityId`: the activity id in which the incident occurred | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


