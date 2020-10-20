# HistoricProcessInstanceDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | Option<**String**> | The id of the process instance. | [optional]
**root_process_instance_id** | Option<**String**> | The process instance id of the root process instance that initiated the process. | [optional]
**super_process_instance_id** | Option<**String**> | The id of the parent process instance, if it exists. | [optional]
**super_case_instance_id** | Option<**String**> | The id of the parent case instance, if it exists. | [optional]
**case_instance_id** | Option<**String**> | The id of the parent case instance, if it exists. | [optional]
**process_definition_name** | Option<**String**> | The name of the process definition that this process instance belongs to. | [optional]
**process_definition_key** | Option<**String**> | The key of the process definition that this process instance belongs to. | [optional]
**process_definition_version** | Option<**i32**> | The version of the process definition that this process instance belongs to. | [optional]
**process_definition_id** | Option<**String**> | The id of the process definition that this process instance belongs to. | [optional]
**business_key** | Option<**String**> | The business key of the process instance. | [optional]
**start_time** | Option<**String**> | The time the instance was started. Default [format](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/) `yyyy-MM-dd'T'HH:mm:ss.SSSZ`. | [optional]
**end_time** | Option<**String**> | The time the instance ended. Default [format](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/) `yyyy-MM-dd'T'HH:mm:ss.SSSZ`. | [optional]
**removal_time** | Option<**String**> | The time after which the instance should be removed by the History Cleanup job. Default [format](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/) `yyyy-MM-dd'T'HH:mm:ss.SSSZ`. | [optional]
**duration_in_millis** | Option<**i32**> | The time the instance took to finish (in milliseconds). | [optional]
**start_user_id** | Option<**String**> | The id of the user who started the process instance. | [optional]
**start_activity_id** | Option<**String**> | The id of the initial activity that was executed (e.g., a start event). | [optional]
**delete_reason** | Option<**String**> | The provided delete reason in case the process instance was canceled during execution. | [optional]
**tenant_id** | Option<**String**> | The tenant id of the process instance. | [optional]
**state** | Option<**String**> | Last state of the process instance, possible values are:  `ACTIVE` - running process instance  `SUSPENDED` - suspended process instances  `COMPLETED` - completed through normal end event  `EXTERNALLY_TERMINATED` - terminated externally, for instance through REST API  `INTERNALLY_TERMINATED` - terminated internally, for instance by terminating boundary event | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


