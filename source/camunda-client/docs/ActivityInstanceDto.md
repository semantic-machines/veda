# ActivityInstanceDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | Option<**String**> | The id of the activity instance. | [optional]
**parent_activity_instance_id** | Option<**String**> | The id of the parent activity instance, for example a sub process instance. | [optional]
**activity_id** | Option<**String**> | The id of the activity. | [optional]
**activity_name** | Option<**String**> | The name of the activity | [optional]
**activity_type** | Option<**String**> | The type of activity (corresponds to the XML element name in the BPMN 2.0, e.g., 'userTask') | [optional]
**process_instance_id** | Option<**String**> | The id of the process instance this activity instance is part of. | [optional]
**process_definition_id** | Option<**String**> | The id of the process definition. | [optional]
**child_activity_instances** | Option<[**Vec<crate::models::ActivityInstanceDto>**](ActivityInstanceDto.md)> | A list of child activity instances. | [optional]
**child_transition_instances** | Option<[**Vec<crate::models::TransitionInstanceDto>**](TransitionInstanceDto.md)> | A list of child transition instances. A transition instance represents an execution waiting in an asynchronous continuation. | [optional]
**execution_ids** | Option<**Vec<String>**> | A list of execution ids. | [optional]
**incident_ids** | Option<**Vec<String>**> | A list of incident ids. | [optional]
**incidents** | Option<[**Vec<crate::models::ActivityInstanceIncidentDto>**](ActivityInstanceIncidentDto.md)> | A list of JSON objects containing incident specific properties: * `id`: the id of the incident * `activityId`: the activity id in which the incident occurred | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


