# \ExternalTaskApi

All URIs are relative to *http://localhost:8080/engine-rest*

Method | HTTP request | Description
------------- | ------------- | -------------
[**complete_external_task_resource**](ExternalTaskApi.md#complete_external_task_resource) | **post** /external-task/{id}/complete | 
[**extend_lock**](ExternalTaskApi.md#extend_lock) | **post** /external-task/{id}/extendLock | 
[**fetch_and_lock**](ExternalTaskApi.md#fetch_and_lock) | **post** /external-task/fetchAndLock | 
[**get_external_task**](ExternalTaskApi.md#get_external_task) | **get** /external-task/{id} | 
[**get_external_task_error_details**](ExternalTaskApi.md#get_external_task_error_details) | **get** /external-task/{id}/errorDetails | 
[**get_external_tasks**](ExternalTaskApi.md#get_external_tasks) | **get** /external-task | 
[**get_external_tasks_count**](ExternalTaskApi.md#get_external_tasks_count) | **get** /external-task/count | 
[**get_topic_names**](ExternalTaskApi.md#get_topic_names) | **get** /external-task/topic-names | 
[**handle_external_task_bpmn_error**](ExternalTaskApi.md#handle_external_task_bpmn_error) | **post** /external-task/{id}/bpmnError | 
[**handle_failure**](ExternalTaskApi.md#handle_failure) | **post** /external-task/{id}/failure | 
[**query_external_tasks**](ExternalTaskApi.md#query_external_tasks) | **post** /external-task | 
[**query_external_tasks_count**](ExternalTaskApi.md#query_external_tasks_count) | **post** /external-task/count | 
[**set_external_task_resource_priority**](ExternalTaskApi.md#set_external_task_resource_priority) | **put** /external-task/{id}/priority | 
[**set_external_task_resource_retries**](ExternalTaskApi.md#set_external_task_resource_retries) | **put** /external-task/{id}/retries | 
[**set_external_task_retries**](ExternalTaskApi.md#set_external_task_retries) | **put** /external-task/retries | 
[**set_external_task_retries_async_operation**](ExternalTaskApi.md#set_external_task_retries_async_operation) | **post** /external-task/retries-async | 
[**unlock**](ExternalTaskApi.md#unlock) | **post** /external-task/{id}/unlock | 



## complete_external_task_resource

> complete_external_task_resource(id, complete_external_task_dto)


Completes an external task by id and updates process variables.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task to complete. | [required] |
**complete_external_task_dto** | Option<[**CompleteExternalTaskDto**](CompleteExternalTaskDto.md)> |  |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## extend_lock

> extend_lock(id, extend_lock_on_external_task_dto)


Extends the timeout of the lock by a given amount of time.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the external task. | [required] |
**extend_lock_on_external_task_dto** | Option<[**ExtendLockOnExternalTaskDto**](ExtendLockOnExternalTaskDto.md)> |  |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## fetch_and_lock

> Vec<crate::models::LockedExternalTaskDto> fetch_and_lock(fetch_external_tasks_dto)


Fetches and locks a specific number of external tasks for execution by a worker. Query can be restricted to specific task topics and for each task topic an individual lock time can be provided.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**fetch_external_tasks_dto** | Option<[**FetchExternalTasksDto**](FetchExternalTasksDto.md)> |  |  |

### Return type

[**Vec<crate::models::LockedExternalTaskDto>**](LockedExternalTaskDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_external_task

> crate::models::ExternalTaskDto get_external_task(id)


Retrieves an external task by id, corresponding to the `ExternalTask` interface in the engine.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the external task to be retrieved. | [required] |

### Return type

[**crate::models::ExternalTaskDto**](ExternalTaskDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_external_task_error_details

> String get_external_task_error_details(id)


Retrieves the error details in the context of a running external task by id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the external task for which the error details should be retrieved. | [required] |

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/plain, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_external_tasks

> Vec<crate::models::ExternalTaskDto> get_external_tasks(external_task_id, external_task_id_in, topic_name, worker_id, locked, not_locked, with_retries_left, no_retries_left, lock_expiration_after, lock_expiration_before, activity_id, activity_id_in, execution_id, process_instance_id, process_instance_id_in, process_definition_id, tenant_id_in, active, suspended, priority_higher_than_or_equals, priority_lower_than_or_equals, sort_by, sort_order, first_result, max_results)


Queries for the external tasks that fulfill given parameters. Parameters may be static as well as dynamic runtime properties of executions. The size of the result set can be retrieved by using the [Get External Task Count](https://docs.camunda.org/manual/7.14/reference/rest/external-task/get-query-count/) method.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**external_task_id** | Option<**String**> | Filter by an external task's id. |  |
**external_task_id_in** | Option<**String**> | Filter by the comma-separated list of external task ids. |  |
**topic_name** | Option<**String**> | Filter by an external task topic. |  |
**worker_id** | Option<**String**> | Filter by the id of the worker that the task was most recently locked by. |  |
**locked** | Option<**bool**> | Only include external tasks that are currently locked (i.e., they have a lock time and it has not expired). Value may only be `true`, as `false` matches any external task. |  |
**not_locked** | Option<**bool**> | Only include external tasks that are currently not locked (i.e., they have no lock or it has expired). Value may only be `true`, as `false` matches any external task. |  |
**with_retries_left** | Option<**bool**> | Only include external tasks that have a positive (&gt; 0) number of retries (or `null`). Value may only be `true`, as `false` matches any external task. |  |
**no_retries_left** | Option<**bool**> | Only include external tasks that have 0 retries. Value may only be `true`, as `false` matches any external task. |  |
**lock_expiration_after** | Option<**String**> | Restrict to external tasks that have a lock that expires after a given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**lock_expiration_before** | Option<**String**> | Restrict to external tasks that have a lock that expires before a given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**activity_id** | Option<**String**> | Filter by the id of the activity that an external task is created for. |  |
**activity_id_in** | Option<**String**> | Filter by the comma-separated list of ids of the activities that an external task is created for. |  |
**execution_id** | Option<**String**> | Filter by the id of the execution that an external task belongs to. |  |
**process_instance_id** | Option<**String**> | Filter by the id of the process instance that an external task belongs to. |  |
**process_instance_id_in** | Option<**String**> | Filter by a comma-separated list of process instance ids that an external task may belong to. |  |
**process_definition_id** | Option<**String**> | Filter by the id of the process definition that an external task belongs to. |  |
**tenant_id_in** | Option<**String**> | Filter by a comma-separated list of tenant ids. An external task must have one of the given tenant ids. |  |
**active** | Option<**bool**> | Only include active tasks. Value may only be `true`, as `false` matches any external task. |  |
**suspended** | Option<**bool**> | Only include suspended tasks. Value may only be `true`, as `false` matches any external task. |  |
**priority_higher_than_or_equals** | Option<**i64**> | Only include jobs with a priority higher than or equal to the given value. Value must be a valid `long` value. |  |
**priority_lower_than_or_equals** | Option<**i64**> | Only include jobs with a priority lower than or equal to the given value. Value must be a valid `long` value. |  |
**sort_by** | Option<**String**> | Sort the results lexicographically by a given criterion. Must be used in conjunction with the sortOrder parameter. |  |
**sort_order** | Option<**String**> | Sort the results in a given order. Values may be asc for ascending order or desc for descending order. Must be used in conjunction with the sortBy parameter. |  |
**first_result** | Option<**i32**> | Pagination of results. Specifies the index of the first result to return. |  |
**max_results** | Option<**i32**> | Pagination of results. Specifies the maximum number of results to return. Will return less results if there are no more results left. |  |

### Return type

[**Vec<crate::models::ExternalTaskDto>**](ExternalTaskDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_external_tasks_count

> crate::models::CountResultDto get_external_tasks_count(external_task_id, external_task_id_in, topic_name, worker_id, locked, not_locked, with_retries_left, no_retries_left, lock_expiration_after, lock_expiration_before, activity_id, activity_id_in, execution_id, process_instance_id, process_instance_id_in, process_definition_id, tenant_id_in, active, suspended, priority_higher_than_or_equals, priority_lower_than_or_equals)


Queries for the number of external tasks that fulfill given parameters. Takes the same parameters as the [Get External Tasks](https://docs.camunda.org/manual/7.14/reference/rest/external-task/get-query/) method.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**external_task_id** | Option<**String**> | Filter by an external task's id. |  |
**external_task_id_in** | Option<**String**> | Filter by the comma-separated list of external task ids. |  |
**topic_name** | Option<**String**> | Filter by an external task topic. |  |
**worker_id** | Option<**String**> | Filter by the id of the worker that the task was most recently locked by. |  |
**locked** | Option<**bool**> | Only include external tasks that are currently locked (i.e., they have a lock time and it has not expired). Value may only be `true`, as `false` matches any external task. |  |
**not_locked** | Option<**bool**> | Only include external tasks that are currently not locked (i.e., they have no lock or it has expired). Value may only be `true`, as `false` matches any external task. |  |
**with_retries_left** | Option<**bool**> | Only include external tasks that have a positive (&gt; 0) number of retries (or `null`). Value may only be `true`, as `false` matches any external task. |  |
**no_retries_left** | Option<**bool**> | Only include external tasks that have 0 retries. Value may only be `true`, as `false` matches any external task. |  |
**lock_expiration_after** | Option<**String**> | Restrict to external tasks that have a lock that expires after a given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**lock_expiration_before** | Option<**String**> | Restrict to external tasks that have a lock that expires before a given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**activity_id** | Option<**String**> | Filter by the id of the activity that an external task is created for. |  |
**activity_id_in** | Option<**String**> | Filter by the comma-separated list of ids of the activities that an external task is created for. |  |
**execution_id** | Option<**String**> | Filter by the id of the execution that an external task belongs to. |  |
**process_instance_id** | Option<**String**> | Filter by the id of the process instance that an external task belongs to. |  |
**process_instance_id_in** | Option<**String**> | Filter by a comma-separated list of process instance ids that an external task may belong to. |  |
**process_definition_id** | Option<**String**> | Filter by the id of the process definition that an external task belongs to. |  |
**tenant_id_in** | Option<**String**> | Filter by a comma-separated list of tenant ids. An external task must have one of the given tenant ids. |  |
**active** | Option<**bool**> | Only include active tasks. Value may only be `true`, as `false` matches any external task. |  |
**suspended** | Option<**bool**> | Only include suspended tasks. Value may only be `true`, as `false` matches any external task. |  |
**priority_higher_than_or_equals** | Option<**i64**> | Only include jobs with a priority higher than or equal to the given value. Value must be a valid `long` value. |  |
**priority_lower_than_or_equals** | Option<**i64**> | Only include jobs with a priority lower than or equal to the given value. Value must be a valid `long` value. |  |

### Return type

[**crate::models::CountResultDto**](CountResultDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_topic_names

> Vec<String> get_topic_names(with_locked_tasks, with_unlocked_tasks, with_retries_left)


Queries for distinct topic names of external tasks that fulfill given parameters. Query can be restricted to only tasks with retries left, tasks that are locked, or tasks that are unlocked. The parameters withLockedTasks and withUnlockedTasks are exclusive. Setting them both to true will return an empty list. Providing no parameters will return a list of all distinct topic names with external tasks.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**with_locked_tasks** | Option<**bool**> | Only include external tasks that are currently locked (i.e., they have a lock time and it has not expired). Value may only be `true`, as `false` matches any external task. |  |
**with_unlocked_tasks** | Option<**bool**> | Only include external tasks that are currently not locked (i.e., they have no lock or it has expired). Value may only be `true`, as `false` matches any external task. |  |
**with_retries_left** | Option<**bool**> | Only include external tasks that have a positive (&gt; 0) number of retries (or `null`). Value may only be `true`, as `false` matches any external task. |  |

### Return type

**Vec<String>**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## handle_external_task_bpmn_error

> handle_external_task_bpmn_error(id, external_task_bpmn_error)


Reports a business error in the context of a running external task by id. The error code must be specified to identify the BPMN error handler.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the external task in which context a BPMN error is reported. | [required] |
**external_task_bpmn_error** | Option<[**ExternalTaskBpmnError**](ExternalTaskBpmnError.md)> |  |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## handle_failure

> handle_failure(id, external_task_failure_dto)


Reports a failure to execute an external task by id. A number of retries and a timeout until the task can be retried can be specified. If retries are set to 0, an incident for this task is created.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the external task to report a failure for. | [required] |
**external_task_failure_dto** | Option<[**ExternalTaskFailureDto**](ExternalTaskFailureDto.md)> |  |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## query_external_tasks

> Vec<crate::models::ExternalTaskDto> query_external_tasks(first_result, max_results, external_task_query_dto)


Queries for external tasks that fulfill given parameters in the form of a JSON object.  This method is slightly more powerful than the [Get External Tasks](https://docs.camunda.org/manual/7.14/reference/rest/external-task/get-query/) method because it allows to specify a hierarchical result sorting.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**first_result** | Option<**i32**> | Pagination of results. Specifies the index of the first result to return. |  |
**max_results** | Option<**i32**> | Pagination of results. Specifies the maximum number of results to return. Will return less results if there are no more results left. |  |
**external_task_query_dto** | Option<[**ExternalTaskQueryDto**](ExternalTaskQueryDto.md)> |  |  |

### Return type

[**Vec<crate::models::ExternalTaskDto>**](ExternalTaskDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## query_external_tasks_count

> crate::models::CountResultDto query_external_tasks_count(external_task_query_dto)


Queries for the number of external tasks that fulfill given parameters. This method takes the same message body as the [Get External Tasks (POST)](https://docs.camunda.org/manual/7.14/reference/rest/external-task/post-query/) method.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**external_task_query_dto** | Option<[**ExternalTaskQueryDto**](ExternalTaskQueryDto.md)> |  |  |

### Return type

[**crate::models::CountResultDto**](CountResultDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## set_external_task_resource_priority

> set_external_task_resource_priority(id, priority_dto)


Sets the priority of an existing external task by id. The default value of a priority is 0.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the external task to set the priority for. | [required] |
**priority_dto** | Option<[**PriorityDto**](PriorityDto.md)> |  |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## set_external_task_resource_retries

> set_external_task_resource_retries(id, retries_dto)


Sets the number of retries left to execute an external task by id. If retries are set to 0, an  incident is created.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the external task to set the number of retries for. | [required] |
**retries_dto** | Option<[**RetriesDto**](RetriesDto.md)> |  |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## set_external_task_retries

> set_external_task_retries(set_retries_for_external_tasks_dto)


Sets the number of retries left to execute external tasks by id synchronously. If retries are set to 0,  an incident is created.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**set_retries_for_external_tasks_dto** | Option<[**SetRetriesForExternalTasksDto**](SetRetriesForExternalTasksDto.md)> |  |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## set_external_task_retries_async_operation

> crate::models::BatchDto set_external_task_retries_async_operation(set_retries_for_external_tasks_dto)


Sets the number of retries left to execute external tasks by id asynchronously. If retries are set to 0, an incident is created.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**set_retries_for_external_tasks_dto** | Option<[**SetRetriesForExternalTasksDto**](SetRetriesForExternalTasksDto.md)> |  |  |

### Return type

[**crate::models::BatchDto**](BatchDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## unlock

> unlock(id)


Unlocks an external task by id. Clears the task's lock expiration time and worker id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the external task to unlock. | [required] |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

