# TaskDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | Option<**String**> | The task id. | [optional]
**name** | Option<**String**> | The task name. | [optional]
**assignee** | Option<**String**> | The assignee's id. | [optional]
**owner** | Option<**String**> | The owner's id. | [optional]
**created** | Option<**String**> | The date the task was created on. [Default format](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/) `yyyy-MM-dd'T'HH:mm:ss.SSSZ`. | [optional]
**due** | Option<**String**> | The task's due date. [Default format](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/) `yyyy-MM-dd'T'HH:mm:ss.SSSZ`. | [optional]
**follow_up** | Option<**String**> | The follow-up date for the task. [Default format](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/) `yyyy-MM-dd'T'HH:mm:ss.SSSZ`. | [optional]
**delegation_state** | Option<**String**> | The task's delegation state. Possible values are `PENDING` and `RESOLVED`. | [optional]
**description** | Option<**String**> | The task's description. | [optional]
**execution_id** | Option<**String**> | The id of the execution the task belongs to. | [optional]
**parent_task_id** | Option<**String**> | The id the parent task, if this task is a subtask. | [optional]
**priority** | Option<**i32**> | The task's priority. | [optional]
**process_definition_id** | Option<**String**> | The id of the process definition the task belongs to. | [optional]
**process_instance_id** | Option<**String**> | The id of the process instance the task belongs to. | [optional]
**case_execution_id** | Option<**String**> | The id of the case execution the task belongs to. | [optional]
**case_definition_id** | Option<**String**> | The id of the case definition the task belongs to. | [optional]
**case_instance_id** | Option<**String**> | The id of the case instance the task belongs to. | [optional]
**task_definition_key** | Option<**String**> | The task's key. | [optional]
**suspended** | Option<**bool**> | Whether the task belongs to a process instance that is suspended. | [optional]
**form_key** | Option<**String**> | If not `null`, the form key for the task. | [optional]
**tenant_id** | Option<**String**> | If not `null`, the tenant id of the task. | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


