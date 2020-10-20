# HistoricActivityInstanceDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | Option<**String**> | The id of the activity instance. | [optional]
**parent_activity_instance_id** | Option<**String**> | The id of the parent activity instance, for example a sub process instance. | [optional]
**activity_id** | Option<**String**> | The id of the activity that this object is an instance of. | [optional]
**activity_name** | Option<**String**> | The name of the activity that this object is an instance of. | [optional]
**activity_type** | Option<**String**> | The type of the activity that this object is an instance of. | [optional]
**process_definition_key** | Option<**String**> | The key of the process definition that this activity instance belongs to. | [optional]
**process_definition_id** | Option<**String**> | The id of the process definition that this activity instance belongs to. | [optional]
**process_instance_id** | Option<**String**> | The id of the process instance that this activity instance belongs to. | [optional]
**execution_id** | Option<**String**> | The id of the execution that executed this activity instance. | [optional]
**task_id** | Option<**String**> | The id of the task that is associated to this activity instance. Is only set if the activity is a user task. | [optional]
**assignee** | Option<**String**> | The assignee of the task that is associated to this activity instance. Is only set if the activity is a user task. | [optional]
**called_process_instance_id** | Option<**String**> | The id of the called process instance. Is only set if the activity is a call activity and the called instance a process instance. | [optional]
**called_case_instance_id** | Option<**String**> | The id of the called case instance. Is only set if the activity is a call activity and the called instance a case instance. | [optional]
**start_time** | Option<**String**> | The time the instance was started. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. | [optional]
**end_time** | Option<**String**> | The time the instance ended. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. | [optional]
**duration_in_millis** | Option<**i32**> | The time the instance took to finish (in milliseconds). | [optional]
**canceled** | Option<**bool**> | If `true`, this activity instance is canceled. | [optional]
**complete_scope** | Option<**bool**> | If `true`, this activity instance did complete a BPMN 2.0 scope. | [optional]
**tenant_id** | Option<**String**> | The tenant id of the activity instance. | [optional]
**removal_time** | Option<**String**> | The time after which the activity instance should be removed by the History Cleanup job. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. | [optional]
**root_process_instance_id** | Option<**String**> | The process instance id of the root process instance that initiated the process containing this activity instance. | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


