# ExternalTaskDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**activity_id** | Option<**String**> | The id of the activity that this external task belongs to. | [optional]
**activity_instance_id** | Option<**String**> | The id of the activity instance that the external task belongs to. | [optional]
**error_message** | Option<**String**> | The full error message submitted with the latest reported failure executing this task; `null` if no failure was reported previously or if no error message was submitted | [optional]
**execution_id** | Option<**String**> | The id of the execution that the external task belongs to. | [optional]
**id** | Option<**String**> | The id of the external task. | [optional]
**lock_expiration_time** | Option<**String**> | The date that the task's most recent lock expires or has expired. | [optional]
**process_definition_id** | Option<**String**> | The id of the process definition the external task is defined in. | [optional]
**process_definition_key** | Option<**String**> | The key of the process definition the external task is defined in. | [optional]
**process_definition_version_tag** | Option<**String**> | The version tag of the process definition the external task is defined in. | [optional]
**process_instance_id** | Option<**String**> | The id of the process instance the external task belongs to. | [optional]
**tenant_id** | Option<**String**> | The id of the tenant the external task belongs to. | [optional]
**retries** | Option<**i32**> | The number of retries the task currently has left. | [optional]
**suspended** | Option<**bool**> | A flag indicating whether the external task is suspended or not. | [optional]
**worker_id** | Option<**String**> | The id of the worker that posesses or posessed the most recent lock. | [optional]
**topic_name** | Option<**String**> | The topic name of the external task. | [optional]
**priority** | Option<**i64**> | The priority of the external task. | [optional]
**business_key** | Option<**String**> | The business key of the process instance the external task belongs to. | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


