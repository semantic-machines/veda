# \HistoricProcessInstanceApi

All URIs are relative to *http://localhost:8080/engine-rest*

Method | HTTP request | Description
------------- | ------------- | -------------
[**delete_historic_process_instance**](HistoricProcessInstanceApi.md#delete_historic_process_instance) | **delete** /history/process-instance/{id} | Delete
[**delete_historic_process_instances_async**](HistoricProcessInstanceApi.md#delete_historic_process_instances_async) | **post** /history/process-instance/delete | Delete Async (POST)
[**delete_historic_variable_instances_of_historic_process_instance**](HistoricProcessInstanceApi.md#delete_historic_variable_instances_of_historic_process_instance) | **delete** /history/process-instance/{id}/variable-instances | Delete Variable Instances
[**get_historic_process_instance**](HistoricProcessInstanceApi.md#get_historic_process_instance) | **get** /history/process-instance/{id} | Get
[**get_historic_process_instance_duration_report**](HistoricProcessInstanceApi.md#get_historic_process_instance_duration_report) | **get** /history/process-instance/report | Get Duration Report
[**get_historic_process_instances**](HistoricProcessInstanceApi.md#get_historic_process_instances) | **get** /history/process-instance | Get List
[**get_historic_process_instances_count**](HistoricProcessInstanceApi.md#get_historic_process_instances_count) | **get** /history/process-instance/count | Get List Count
[**query_historic_process_instances**](HistoricProcessInstanceApi.md#query_historic_process_instances) | **post** /history/process-instance | Get List (POST)
[**query_historic_process_instances_count**](HistoricProcessInstanceApi.md#query_historic_process_instances_count) | **post** /history/process-instance/count | Get List Count (POST)
[**set_removal_time_async**](HistoricProcessInstanceApi.md#set_removal_time_async) | **post** /history/process-instance/set-removal-time | Set Removal Time Async (POST)



## delete_historic_process_instance

> delete_historic_process_instance(id, fail_if_not_exists)
Delete

Deletes a process instance from the history by id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the historic process instance to be deleted. | [required] |
**fail_if_not_exists** | Option<**bool**> | If set to `false`, the request will still be successful if the process id is not found. |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## delete_historic_process_instances_async

> crate::models::BatchDto delete_historic_process_instances_async(delete_historic_process_instances_dto)
Delete Async (POST)

Delete multiple historic process instances asynchronously (batch). At least `historicProcessInstanceIds` or `historicProcessInstanceQuery` has to be provided. If both are provided then all instances matching query criterion and instances from the list will be deleted.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**delete_historic_process_instances_dto** | Option<[**DeleteHistoricProcessInstancesDto**](DeleteHistoricProcessInstancesDto.md)> |  |  |

### Return type

[**crate::models::BatchDto**](BatchDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## delete_historic_variable_instances_of_historic_process_instance

> delete_historic_variable_instances_of_historic_process_instance(id)
Delete Variable Instances

Deletes all variables of a process instance from the history by id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the process instance for which all historic variables are to be deleted. | [required] |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_historic_process_instance

> crate::models::HistoricProcessInstanceDto get_historic_process_instance(id)
Get

Retrieves a historic process instance by id, according to the `HistoricProcessInstance` interface in the engine.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the historic process instance to be retrieved. | [required] |

### Return type

[**crate::models::HistoricProcessInstanceDto**](HistoricProcessInstanceDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_historic_process_instance_duration_report

> crate::models::AnyType get_historic_process_instance_duration_report(report_type, period_unit, process_definition_id_in, process_definition_key_in, started_before, started_after)
Get Duration Report

Retrieves a report about the duration of completed process instances, grouped by a period. These reports include the maximum, minimum and average duration of all completed process instances which were started in a given period.  **Note:** This only includes historic data.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**report_type** | **String** | **Mandatory.** Specifies the type of the report to retrieve. To retrieve a report about the duration of process instances, the value must be set to `duration`. | [required] |
**period_unit** | **String** | **Mandatory.** Specifies the granularity of the report. Valid values are `month` and `quarter`. | [required] |
**process_definition_id_in** | Option<**String**> | Filter by process definition ids. Must be a comma-separated list of process definition ids. |  |
**process_definition_key_in** | Option<**String**> | Filter by process definition keys. Must be a comma-separated list of process definition keys. |  |
**started_before** | Option<**String**> | Restrict to instances that were started before the given date. By [default](), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2016-01-23T14:42:45.000+0200`. |  |
**started_after** | Option<**String**> | Restrict to instances that were started after the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2016-01-23T14:42:45.000+0200`. |  |

### Return type

[**crate::models::AnyType**](AnyType.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: text/csv, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_historic_process_instances

> Vec<crate::models::HistoricProcessInstanceDto> get_historic_process_instances(sort_by, sort_order, first_result, max_results, process_instance_id, process_instance_ids, process_definition_id, process_definition_key, process_definition_key_in, process_definition_name, process_definition_name_like, process_definition_key_not_in, process_instance_business_key, process_instance_business_key_like, root_process_instances, finished, unfinished, with_incidents, with_root_incidents, incident_type, incident_status, incident_message, incident_message_like, started_before, started_after, finished_before, finished_after, executed_activity_after, executed_activity_before, executed_job_after, executed_job_before, started_by, super_process_instance_id, sub_process_instance_id, super_case_instance_id, sub_case_instance_id, case_instance_id, tenant_id_in, without_tenant_id, executed_activity_id_in, active_activity_id_in, active, suspended, completed, externally_terminated, internally_terminated, variables, variable_names_ignore_case, variable_values_ignore_case)
Get List

Queries for historic process instances that fulfill the given parameters. The size of the result set can be retrieved by using the [Get Process Instance Count](https://docs.camunda.org/manual/7.14/reference/rest/history/process-instance/get-process-instance-query-count/) method.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**sort_by** | Option<**String**> | Sort the results lexicographically by a given criterion. Must be used in conjunction with the sortOrder parameter. |  |
**sort_order** | Option<**String**> | Sort the results in a given order. Values may be asc for ascending order or desc for descending order. Must be used in conjunction with the sortBy parameter. |  |
**first_result** | Option<**i32**> | Pagination of results. Specifies the index of the first result to return. |  |
**max_results** | Option<**i32**> | Pagination of results. Specifies the maximum number of results to return. Will return less results if there are no more results left. |  |
**process_instance_id** | Option<**String**> | Filter by process instance id. |  |
**process_instance_ids** | Option<**String**> | Filter by process instance ids. Filter by a comma-separated list of `Strings`. |  |
**process_definition_id** | Option<**String**> | Filter by the process definition the instances run on. |  |
**process_definition_key** | Option<**String**> | Filter by the key of the process definition the instances run on. |  |
**process_definition_key_in** | Option<**String**> | Filter by a list of process definition keys. A process instance must have one of the given process definition keys. Filter by a comma-separated list of `Strings`. |  |
**process_definition_name** | Option<**String**> | Filter by the name of the process definition the instances run on. |  |
**process_definition_name_like** | Option<**String**> | Filter by process definition names that the parameter is a substring of. |  |
**process_definition_key_not_in** | Option<**String**> | Exclude instances that belong to a set of process definitions. Filter by a comma-separated list of `Strings`. |  |
**process_instance_business_key** | Option<**String**> | Filter by process instance business key. |  |
**process_instance_business_key_like** | Option<**String**> | Filter by process instance business key that the parameter is a substring of. |  |
**root_process_instances** | Option<**bool**> | Restrict the query to all process instances that are top level process instances. |  |
**finished** | Option<**bool**> | Only include finished process instances. Value may only be `true`, as `false` is the default behavior. |  |
**unfinished** | Option<**bool**> | Only include unfinished process instances. Value may only be `true`, as `false` is the default behavior. |  |
**with_incidents** | Option<**bool**> | Only include process instances which have an incident. Value may only be `true`, as `false` is the default behavior. |  |
**with_root_incidents** | Option<**bool**> | Only include process instances which have a root incident. Value may only be `true`, as `false` is the default behavior. |  |
**incident_type** | Option<**String**> | Filter by the incident type. See the [User Guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/incidents/#incident-types) for a list of incident types. |  |
**incident_status** | Option<**String**> | Only include process instances which have an incident in status either open or resolved. To get all process instances, use the query parameter withIncidents. |  |
**incident_message** | Option<**String**> | Filter by the incident message. Exact match. |  |
**incident_message_like** | Option<**String**> | Filter by the incident message that the parameter is a substring of. |  |
**started_before** | Option<**String**> | Restrict to instances that were started before the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**started_after** | Option<**String**> | Restrict to instances that were started after the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**finished_before** | Option<**String**> | Restrict to instances that were finished before the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**finished_after** | Option<**String**> | Restrict to instances that were finished after the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**executed_activity_after** | Option<**String**> | Restrict to instances that executed an activity after the given date (inclusive). By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**executed_activity_before** | Option<**String**> | Restrict to instances that executed an activity before the given date (inclusive). By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**executed_job_after** | Option<**String**> | Restrict to instances that executed an job after the given date (inclusive). By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**executed_job_before** | Option<**String**> | Restrict to instances that executed an job before the given date (inclusive). By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**started_by** | Option<**String**> | Only include process instances that were started by the given user. |  |
**super_process_instance_id** | Option<**String**> | Restrict query to all process instances that are sub process instances of the given process instance. Takes a process instance id. |  |
**sub_process_instance_id** | Option<**String**> | Restrict query to one process instance that has a sub process instance with the given id. |  |
**super_case_instance_id** | Option<**String**> | Restrict query to all process instances that are sub process instances of the given case instance. Takes a case instance id. |  |
**sub_case_instance_id** | Option<**String**> | Restrict query to one process instance that has a sub case instance with the given id. |  |
**case_instance_id** | Option<**String**> | Restrict query to all process instances that are sub process instances of the given case instance. Takes a case instance id. |  |
**tenant_id_in** | Option<**String**> | Filter by a list of tenant ids. A process instance must have one of the given tenant ids. Filter by a comma-separated list of `Strings` |  |
**without_tenant_id** | Option<**bool**> | Only include historic process instances which belong to no tenant. Value may only be `true`, as `false` is the default behavior. |  |
**executed_activity_id_in** | Option<**String**> | Restrict to instances that executed an activity with one of given ids. Filter by a comma-separated list of `Strings` |  |
**active_activity_id_in** | Option<**String**> | Restrict to instances that have an active activity with one of given ids. Filter by a comma-separated list of `Strings` |  |
**active** | Option<**bool**> | Restrict to instances that are active. |  |
**suspended** | Option<**bool**> | Restrict to instances that are suspended. |  |
**completed** | Option<**bool**> | Restrict to instances that are completed. |  |
**externally_terminated** | Option<**bool**> | Restrict to instances that are externallyTerminated. |  |
**internally_terminated** | Option<**bool**> | Restrict to instances that are internallyTerminated. |  |
**variables** | Option<**String**> | Only include process instances that have/had variables with certain values. Variable filtering expressions are comma-separated and are structured as follows: A valid parameter value has the form `key_operator_value`. `key` is the variable name, `operator` is the comparison operator to be used and `value` the variable value.  **Note:** Values are always treated as String objects on server side.  Valid operator values are: `eq` - equal to; `neq` - not equal to; `gt` - greater than; `gteq` - greater than or equal to; `lt` - lower than; `lteq` - lower than or equal to; `like`.  Key and value may not contain underscore or comma characters.  |  |
**variable_names_ignore_case** | Option<**bool**> | Match all variable names provided in variables case-insensitively. If set to `true` variableName and variablename are treated as equal. |  |
**variable_values_ignore_case** | Option<**bool**> | Match all variable values provided in variables case-insensitively. If set to `true` variableValue and variablevalue are treated as equal. |  |

### Return type

[**Vec<crate::models::HistoricProcessInstanceDto>**](HistoricProcessInstanceDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_historic_process_instances_count

> crate::models::CountResultDto get_historic_process_instances_count(process_instance_id, process_instance_ids, process_definition_id, process_definition_key, process_definition_key_in, process_definition_name, process_definition_name_like, process_definition_key_not_in, process_instance_business_key, process_instance_business_key_like, root_process_instances, finished, unfinished, with_incidents, with_root_incidents, incident_type, incident_status, incident_message, incident_message_like, started_before, started_after, finished_before, finished_after, executed_activity_after, executed_activity_before, executed_job_after, executed_job_before, started_by, super_process_instance_id, sub_process_instance_id, super_case_instance_id, sub_case_instance_id, case_instance_id, tenant_id_in, without_tenant_id, executed_activity_id_in, active_activity_id_in, active, suspended, completed, externally_terminated, internally_terminated, variables, variable_names_ignore_case, variable_values_ignore_case)
Get List Count

Queries for the number of historic process instances that fulfill the given parameters. Takes the same parameters as the [Get Process Instances](https://docs.camunda.org/manual/7.14/reference/rest/history/process-instance/get-process-instance-query/) method.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**process_instance_id** | Option<**String**> | Filter by process instance id. |  |
**process_instance_ids** | Option<**String**> | Filter by process instance ids. Filter by a comma-separated list of `Strings`. |  |
**process_definition_id** | Option<**String**> | Filter by the process definition the instances run on. |  |
**process_definition_key** | Option<**String**> | Filter by the key of the process definition the instances run on. |  |
**process_definition_key_in** | Option<**String**> | Filter by a list of process definition keys. A process instance must have one of the given process definition keys. Filter by a comma-separated list of `Strings`. |  |
**process_definition_name** | Option<**String**> | Filter by the name of the process definition the instances run on. |  |
**process_definition_name_like** | Option<**String**> | Filter by process definition names that the parameter is a substring of. |  |
**process_definition_key_not_in** | Option<**String**> | Exclude instances that belong to a set of process definitions. Filter by a comma-separated list of `Strings`. |  |
**process_instance_business_key** | Option<**String**> | Filter by process instance business key. |  |
**process_instance_business_key_like** | Option<**String**> | Filter by process instance business key that the parameter is a substring of. |  |
**root_process_instances** | Option<**bool**> | Restrict the query to all process instances that are top level process instances. |  |
**finished** | Option<**bool**> | Only include finished process instances. Value may only be `true`, as `false` is the default behavior. |  |
**unfinished** | Option<**bool**> | Only include unfinished process instances. Value may only be `true`, as `false` is the default behavior. |  |
**with_incidents** | Option<**bool**> | Only include process instances which have an incident. Value may only be `true`, as `false` is the default behavior. |  |
**with_root_incidents** | Option<**bool**> | Only include process instances which have a root incident. Value may only be `true`, as `false` is the default behavior. |  |
**incident_type** | Option<**String**> | Filter by the incident type. See the [User Guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/incidents/#incident-types) for a list of incident types. |  |
**incident_status** | Option<**String**> | Only include process instances which have an incident in status either open or resolved. To get all process instances, use the query parameter withIncidents. |  |
**incident_message** | Option<**String**> | Filter by the incident message. Exact match. |  |
**incident_message_like** | Option<**String**> | Filter by the incident message that the parameter is a substring of. |  |
**started_before** | Option<**String**> | Restrict to instances that were started before the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**started_after** | Option<**String**> | Restrict to instances that were started after the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**finished_before** | Option<**String**> | Restrict to instances that were finished before the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**finished_after** | Option<**String**> | Restrict to instances that were finished after the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**executed_activity_after** | Option<**String**> | Restrict to instances that executed an activity after the given date (inclusive). By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**executed_activity_before** | Option<**String**> | Restrict to instances that executed an activity before the given date (inclusive). By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**executed_job_after** | Option<**String**> | Restrict to instances that executed an job after the given date (inclusive). By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**executed_job_before** | Option<**String**> | Restrict to instances that executed an job before the given date (inclusive). By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**started_by** | Option<**String**> | Only include process instances that were started by the given user. |  |
**super_process_instance_id** | Option<**String**> | Restrict query to all process instances that are sub process instances of the given process instance. Takes a process instance id. |  |
**sub_process_instance_id** | Option<**String**> | Restrict query to one process instance that has a sub process instance with the given id. |  |
**super_case_instance_id** | Option<**String**> | Restrict query to all process instances that are sub process instances of the given case instance. Takes a case instance id. |  |
**sub_case_instance_id** | Option<**String**> | Restrict query to one process instance that has a sub case instance with the given id. |  |
**case_instance_id** | Option<**String**> | Restrict query to all process instances that are sub process instances of the given case instance. Takes a case instance id. |  |
**tenant_id_in** | Option<**String**> | Filter by a list of tenant ids. A process instance must have one of the given tenant ids. Filter by a comma-separated list of `Strings` |  |
**without_tenant_id** | Option<**bool**> | Only include historic process instances which belong to no tenant. Value may only be `true`, as `false` is the default behavior. |  |
**executed_activity_id_in** | Option<**String**> | Restrict to instances that executed an activity with one of given ids. Filter by a comma-separated list of `Strings` |  |
**active_activity_id_in** | Option<**String**> | Restrict to instances that have an active activity with one of given ids. Filter by a comma-separated list of `Strings` |  |
**active** | Option<**bool**> | Restrict to instances that are active. |  |
**suspended** | Option<**bool**> | Restrict to instances that are suspended. |  |
**completed** | Option<**bool**> | Restrict to instances that are completed. |  |
**externally_terminated** | Option<**bool**> | Restrict to instances that are externallyTerminated. |  |
**internally_terminated** | Option<**bool**> | Restrict to instances that are internallyTerminated. |  |
**variables** | Option<**String**> | Only include process instances that have/had variables with certain values. Variable filtering expressions are comma-separated and are structured as follows: A valid parameter value has the form `key_operator_value`. `key` is the variable name, `operator` is the comparison operator to be used and `value` the variable value.  **Note:** Values are always treated as String objects on server side.  Valid operator values are: `eq` - equal to; `neq` - not equal to; `gt` - greater than; `gteq` - greater than or equal to; `lt` - lower than; `lteq` - lower than or equal to; `like`.  Key and value may not contain underscore or comma characters.  |  |
**variable_names_ignore_case** | Option<**bool**> | Match all variable names provided in variables case-insensitively. If set to `true` variableName and variablename are treated as equal. |  |
**variable_values_ignore_case** | Option<**bool**> | Match all variable values provided in variables case-insensitively. If set to `true` variableValue and variablevalue are treated as equal. |  |

### Return type

[**crate::models::CountResultDto**](CountResultDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## query_historic_process_instances

> Vec<crate::models::HistoricProcessInstanceDto> query_historic_process_instances(first_result, max_results, historic_process_instance_query_dto)
Get List (POST)

Queries for historic process instances that fulfill the given parameters. This method is slightly more powerful than the [Get Process Instance](https://docs.camunda.org/manual/7.14/reference/rest/history/process-instance/get-process-instance-query/) because it allows filtering by multiple process variables of types `String`, `Number` or `Boolean`.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**first_result** | Option<**i32**> | Pagination of results. Specifies the index of the first result to return. |  |
**max_results** | Option<**i32**> | Pagination of results. Specifies the maximum number of results to return. Will return less results if there are no more results left. |  |
**historic_process_instance_query_dto** | Option<[**HistoricProcessInstanceQueryDto**](HistoricProcessInstanceQueryDto.md)> |  |  |

### Return type

[**Vec<crate::models::HistoricProcessInstanceDto>**](HistoricProcessInstanceDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## query_historic_process_instances_count

> crate::models::CountResultDto query_historic_process_instances_count(historic_process_instance_query_dto)
Get List Count (POST)

Queries for the number of historic process instances that fulfill the given parameters. This method takes the same message body as the [Get Process Instances (POST)](https://docs.camunda.org/manual/7.14/reference/rest/history/process-instance/get-process-instance-query/) method and therefore it is slightly more powerful than the [Get Process Instance Count](https://docs.camunda.org/manual/7.14/reference/rest/history/process-instance/post-process-instance-query-count/) method.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**historic_process_instance_query_dto** | Option<[**HistoricProcessInstanceQueryDto**](HistoricProcessInstanceQueryDto.md)> |  |  |

### Return type

[**crate::models::CountResultDto**](CountResultDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## set_removal_time_async

> crate::models::BatchDto set_removal_time_async(set_removal_time_to_historic_process_instances_dto)
Set Removal Time Async (POST)

Sets the removal time to multiple historic process instances asynchronously (batch).  At least `historicProcessInstanceIds` or `historicProcessInstanceQuery` has to be provided. If both are provided, all instances matching query criterion and instances from the list will be updated with a removal time.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**set_removal_time_to_historic_process_instances_dto** | Option<[**SetRemovalTimeToHistoricProcessInstancesDto**](SetRemovalTimeToHistoricProcessInstancesDto.md)> |  |  |

### Return type

[**crate::models::BatchDto**](BatchDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

