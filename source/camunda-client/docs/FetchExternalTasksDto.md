# FetchExternalTasksDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**worker_id** | **String** | **Mandatory.** The id of the worker on which behalf tasks are fetched. The returned tasks are locked for that worker and can only be completed when providing the same worker id. | 
**max_tasks** | Option<**i32**> | **Mandatory.** The maximum number of tasks to return. | 
**use_priority** | Option<**bool**> | A `boolean` value, which indicates whether the task should be fetched based on its priority or arbitrarily. | [optional]
**async_response_timeout** | Option<**i64**> | The [Long Polling](https://docs.camunda.org/manual/7.14/user-guide/process-engine/external-tasks/#long-polling-to-fetch-and-lock-external-tasks) timeout in milliseconds.  **Note:** The value cannot be set larger than 1.800.000 milliseconds (corresponds to 30 minutes). | [optional]
**topics** | Option<[**Vec<crate::models::FetchExternalTaskTopicDto>**](FetchExternalTaskTopicDto.md)> | A JSON array of topic objects for which external tasks should be fetched. The returned tasks may be arbitrarily distributed among these topics. Each topic object has the following properties: | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


