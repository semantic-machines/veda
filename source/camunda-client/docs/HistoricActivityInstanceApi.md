# \HistoricActivityInstanceApi

All URIs are relative to *http://localhost:8080/engine-rest*

Method | HTTP request | Description
------------- | ------------- | -------------
[**get_historic_activity_instance**](HistoricActivityInstanceApi.md#get_historic_activity_instance) | **get** /history/activity-instance/{id} | Get
[**get_historic_activity_instances**](HistoricActivityInstanceApi.md#get_historic_activity_instances) | **get** /history/activity-instance | Get List
[**get_historic_activity_instances_count**](HistoricActivityInstanceApi.md#get_historic_activity_instances_count) | **get** /history/activity-instance/count | Get List Count
[**query_historic_activity_instances**](HistoricActivityInstanceApi.md#query_historic_activity_instances) | **post** /history/activity-instance | Get List (POST)
[**query_historic_activity_instances_count**](HistoricActivityInstanceApi.md#query_historic_activity_instances_count) | **post** /history/activity-instance/count | Get List Count (POST)



## get_historic_activity_instance

> crate::models::HistoricActivityInstanceDto get_historic_activity_instance(id)
Get

Retrieves a historic activity instance by id, according to the `HistoricActivityInstance` interface in the engine.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the historic activity instance to be retrieved. | [required] |

### Return type

[**crate::models::HistoricActivityInstanceDto**](HistoricActivityInstanceDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_historic_activity_instances

> Vec<crate::models::HistoricActivityInstanceDto> get_historic_activity_instances(sort_by, sort_order, first_result, max_results, activity_instance_id, process_instance_id, process_definition_id, execution_id, activity_id, activity_name, activity_type, task_assignee, finished, unfinished, canceled, complete_scope, started_before, started_after, finished_before, finished_after, tenant_id_in, without_tenant_id)
Get List

Queries for historic activity instances that fulfill the given parameters. The size of the result set can be retrieved by using the [Get Historic Activity Instance Count](https://docs.camunda.org/manual/7.14/reference/rest/history/activity-instance/get-activity-instance-query-count/) method.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**sort_by** | Option<**String**> | Sort the results lexicographically by a given criterion. Must be used in conjunction with the sortOrder parameter. |  |
**sort_order** | Option<**String**> | Sort the results in a given order. Values may be asc for ascending order or desc for descending order. Must be used in conjunction with the sortBy parameter. |  |
**first_result** | Option<**i32**> | Pagination of results. Specifies the index of the first result to return. |  |
**max_results** | Option<**i32**> | Pagination of results. Specifies the maximum number of results to return. Will return less results if there are no more results left. |  |
**activity_instance_id** | Option<**String**> | Filter by activity instance id. |  |
**process_instance_id** | Option<**String**> | Filter by process instance id. |  |
**process_definition_id** | Option<**String**> | Filter by process definition id. |  |
**execution_id** | Option<**String**> | Filter by the id of the execution that executed the activity instance. |  |
**activity_id** | Option<**String**> | Filter by the activity id (according to BPMN 2.0 XML). |  |
**activity_name** | Option<**String**> | Filter by the activity name (according to BPMN 2.0 XML). |  |
**activity_type** | Option<**String**> | Filter by activity type. |  |
**task_assignee** | Option<**String**> | Only include activity instances that are user tasks and assigned to a given user. |  |
**finished** | Option<**bool**> | Only include finished activity instances. Value may only be `true`, as `false` behaves the same as when the property is not set. |  |
**unfinished** | Option<**bool**> | Only include unfinished activity instances. Value may only be `true`, as `false` behaves the same as when the property is not set. |  |
**canceled** | Option<**bool**> | Only include canceled activity instances. Value may only be `true`, as `false` behaves the same as when the property is not set. |  |
**complete_scope** | Option<**bool**> | Only include activity instances which completed a scope. Value may only be `true`, as `false` behaves the same as when the property is not set. |  |
**started_before** | Option<**String**> | Restrict to instances that were started before the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**started_after** | Option<**String**> | Restrict to instances that were started after the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**finished_before** | Option<**String**> | Restrict to instances that were finished before the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**finished_after** | Option<**String**> | Restrict to instances that were finished after the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**tenant_id_in** | Option<**String**> | Filter by a comma-separated list of ids. An activity instance must have one of the given tenant ids. |  |
**without_tenant_id** | Option<**bool**> | Only include historic activity instances that belong to no tenant. Value may only be `true`, as `false` is the default behavior. |  |

### Return type

[**Vec<crate::models::HistoricActivityInstanceDto>**](HistoricActivityInstanceDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_historic_activity_instances_count

> crate::models::CountResultDto get_historic_activity_instances_count(activity_instance_id, process_instance_id, process_definition_id, execution_id, activity_id, activity_name, activity_type, task_assignee, finished, unfinished, canceled, complete_scope, started_before, started_after, finished_before, finished_after, tenant_id_in, without_tenant_id)
Get List Count

Queries for the number of historic activity instances that fulfill the given parameters. Takes the same parameters as the [Get Historic Activity Instance](https://docs.camunda.org/manual/7.14/reference/rest/history/activity-instance/get-activity-instance-query/)  method.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**activity_instance_id** | Option<**String**> | Filter by activity instance id. |  |
**process_instance_id** | Option<**String**> | Filter by process instance id. |  |
**process_definition_id** | Option<**String**> | Filter by process definition id. |  |
**execution_id** | Option<**String**> | Filter by the id of the execution that executed the activity instance. |  |
**activity_id** | Option<**String**> | Filter by the activity id (according to BPMN 2.0 XML). |  |
**activity_name** | Option<**String**> | Filter by the activity name (according to BPMN 2.0 XML). |  |
**activity_type** | Option<**String**> | Filter by activity type. |  |
**task_assignee** | Option<**String**> | Only include activity instances that are user tasks and assigned to a given user. |  |
**finished** | Option<**bool**> | Only include finished activity instances. Value may only be `true`, as `false` behaves the same as when the property is not set. |  |
**unfinished** | Option<**bool**> | Only include unfinished activity instances. Value may only be `true`, as `false` behaves the same as when the property is not set. |  |
**canceled** | Option<**bool**> | Only include canceled activity instances. Value may only be `true`, as `false` behaves the same as when the property is not set. |  |
**complete_scope** | Option<**bool**> | Only include activity instances which completed a scope. Value may only be `true`, as `false` behaves the same as when the property is not set. |  |
**started_before** | Option<**String**> | Restrict to instances that were started before the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**started_after** | Option<**String**> | Restrict to instances that were started after the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**finished_before** | Option<**String**> | Restrict to instances that were finished before the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**finished_after** | Option<**String**> | Restrict to instances that were finished after the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**tenant_id_in** | Option<**String**> | Filter by a comma-separated list of ids. An activity instance must have one of the given tenant ids. |  |
**without_tenant_id** | Option<**bool**> | Only include historic activity instances that belong to no tenant. Value may only be `true`, as `false` is the default behavior. |  |

### Return type

[**crate::models::CountResultDto**](CountResultDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## query_historic_activity_instances

> Vec<crate::models::HistoricActivityInstanceDto> query_historic_activity_instances(first_result, max_results, historic_activity_instance_query_dto)
Get List (POST)

Queries for historic activity instances that fulfill the given parameters. The size of the result set can be retrieved by using the [Get Historic Activity Instance Count](https://docs.camunda.org/manual/7.14/reference/rest/history/activity-instance/get-activity-instance-query-count/) method.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**first_result** | Option<**i32**> | Pagination of results. Specifies the index of the first result to return. |  |
**max_results** | Option<**i32**> | Pagination of results. Specifies the maximum number of results to return. Will return less results if there are no more results left. |  |
**historic_activity_instance_query_dto** | Option<[**HistoricActivityInstanceQueryDto**](HistoricActivityInstanceQueryDto.md)> |  |  |

### Return type

[**Vec<crate::models::HistoricActivityInstanceDto>**](HistoricActivityInstanceDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## query_historic_activity_instances_count

> crate::models::CountResultDto query_historic_activity_instances_count(historic_activity_instance_query_dto)
Get List Count (POST)

Queries for the number of historic activity instances that fulfill the given parameters.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**historic_activity_instance_query_dto** | Option<[**HistoricActivityInstanceQueryDto**](HistoricActivityInstanceQueryDto.md)> |  |  |

### Return type

[**crate::models::CountResultDto**](CountResultDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

