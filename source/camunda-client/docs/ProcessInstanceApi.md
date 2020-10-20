# \ProcessInstanceApi

All URIs are relative to *http://localhost:8080/engine-rest*

Method | HTTP request | Description
------------- | ------------- | -------------
[**delete_async_historic_query_based**](ProcessInstanceApi.md#delete_async_historic_query_based) | **post** /process-instance/delete-historic-query-based | 
[**delete_process_instance**](ProcessInstanceApi.md#delete_process_instance) | **delete** /process-instance/{id} | 
[**delete_process_instance_variable**](ProcessInstanceApi.md#delete_process_instance_variable) | **delete** /process-instance/{id}/variables/{varName} | 
[**delete_process_instances_async_operation**](ProcessInstanceApi.md#delete_process_instances_async_operation) | **post** /process-instance/delete | 
[**get_activity_instance_tree**](ProcessInstanceApi.md#get_activity_instance_tree) | **get** /process-instance/{id}/activity-instances | 
[**get_process_instance**](ProcessInstanceApi.md#get_process_instance) | **get** /process-instance/{id} | Get Process Instance
[**get_process_instance_variable**](ProcessInstanceApi.md#get_process_instance_variable) | **get** /process-instance/{id}/variables/{varName} | 
[**get_process_instance_variable_binary**](ProcessInstanceApi.md#get_process_instance_variable_binary) | **get** /process-instance/{id}/variables/{varName}/data | 
[**get_process_instance_variables**](ProcessInstanceApi.md#get_process_instance_variables) | **get** /process-instance/{id}/variables | 
[**get_process_instances**](ProcessInstanceApi.md#get_process_instances) | **get** /process-instance | 
[**get_process_instances_count**](ProcessInstanceApi.md#get_process_instances_count) | **get** /process-instance/count | 
[**modify_process_instance**](ProcessInstanceApi.md#modify_process_instance) | **post** /process-instance/{id}/modification | 
[**modify_process_instance_async_operation**](ProcessInstanceApi.md#modify_process_instance_async_operation) | **post** /process-instance/{id}/modification-async | 
[**modify_process_instance_variables**](ProcessInstanceApi.md#modify_process_instance_variables) | **post** /process-instance/{id}/variables | 
[**query_process_instances**](ProcessInstanceApi.md#query_process_instances) | **post** /process-instance | 
[**query_process_instances_count**](ProcessInstanceApi.md#query_process_instances_count) | **post** /process-instance/count | 
[**set_process_instance_variable**](ProcessInstanceApi.md#set_process_instance_variable) | **put** /process-instance/{id}/variables/{varName} | 
[**set_process_instance_variable_binary**](ProcessInstanceApi.md#set_process_instance_variable_binary) | **post** /process-instance/{id}/variables/{varName}/data | 
[**set_retries_by_process**](ProcessInstanceApi.md#set_retries_by_process) | **post** /process-instance/job-retries | 
[**set_retries_by_process_historic_query_based**](ProcessInstanceApi.md#set_retries_by_process_historic_query_based) | **post** /process-instance/job-retries-historic-query-based | 
[**set_variables_async_operation**](ProcessInstanceApi.md#set_variables_async_operation) | **post** /process-instance/variables-async | 
[**update_suspension_state**](ProcessInstanceApi.md#update_suspension_state) | **put** /process-instance/suspended | 
[**update_suspension_state_async_operation**](ProcessInstanceApi.md#update_suspension_state_async_operation) | **post** /process-instance/suspended-async | 
[**update_suspension_state_by_id**](ProcessInstanceApi.md#update_suspension_state_by_id) | **put** /process-instance/{id}/suspended | 



## delete_async_historic_query_based

> crate::models::BatchDto delete_async_historic_query_based(delete_process_instances_dto)


Deletes a set of process instances asynchronously (batch) based on a historic process instance query.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**delete_process_instances_dto** | Option<[**DeleteProcessInstancesDto**](DeleteProcessInstancesDto.md)> | **Unallowed property**: `processInstanceQuery` |  |

### Return type

[**crate::models::BatchDto**](BatchDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## delete_process_instance

> delete_process_instance(id, skip_custom_listeners, skip_io_mappings, skip_subprocesses, fail_if_not_exists)


Deletes a running process instance by id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the process instance to be deleted. | [required] |
**skip_custom_listeners** | Option<**bool**> | If set to true, the custom listeners will be skipped. |  |[default to false]
**skip_io_mappings** | Option<**bool**> | If set to true, the input/output mappings will be skipped. |  |[default to false]
**skip_subprocesses** | Option<**bool**> | If set to true, subprocesses related to deleted processes will be skipped. |  |[default to false]
**fail_if_not_exists** | Option<**bool**> | If set to false, the request will still be successful if the process id is not found. |  |[default to true]

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## delete_process_instance_variable

> delete_process_instance_variable(id, var_name)


Deletes a variable of a process instance by id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the process instance to delete the variable from. | [required] |
**var_name** | **String** | The name of the variable to delete. | [required] |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## delete_process_instances_async_operation

> crate::models::BatchDto delete_process_instances_async_operation(delete_process_instances_dto)


Deletes multiple process instances asynchronously (batch).

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**delete_process_instances_dto** | Option<[**DeleteProcessInstancesDto**](DeleteProcessInstancesDto.md)> | **Unallowed property**: `historicProcessInstanceQuery` |  |

### Return type

[**crate::models::BatchDto**](BatchDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_activity_instance_tree

> crate::models::ActivityInstanceDto get_activity_instance_tree(id)


Retrieves an Activity Instance (Tree) for a given process instance by id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the process instance for which the activity instance should be retrieved. | [required] |

### Return type

[**crate::models::ActivityInstanceDto**](ActivityInstanceDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_process_instance

> crate::models::ProcessInstanceDto get_process_instance(id)
Get Process Instance

Retrieves a process instance by id, according to the `ProcessInstance` interface in the engine.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the process instance to be retrieved. | [required] |

### Return type

[**crate::models::ProcessInstanceDto**](ProcessInstanceDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_process_instance_variable

> crate::models::VariableValueDto get_process_instance_variable(id, var_name, deserialize_value)


Retrieves a variable of a given process instance by id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the process instance to retrieve the variable for. | [required] |
**var_name** | **String** | The name of the variable to retrieve. | [required] |
**deserialize_value** | Option<**bool**> | Determines whether serializable variable values (typically variables that store custom Java objects) should be deserialized on server side (default true).  If set to true, a serializable variable will be deserialized on server side and transformed to JSON using [Jackson's](https://github.com/FasterXML/jackson) POJO/bean property introspection feature. Note that this requires the Java classes of the variable value to be on the REST API's classpath.  If set to false, a serializable variable will be returned in its serialized format. For example, a variable that is serialized as XML will be returned as a JSON string containing XML.  Note: While true is the default value for reasons of backward compatibility, we recommend setting this parameter to false when developing web applications that are independent of the Java process applications deployed to the engine. |  |[default to true]

### Return type

[**crate::models::VariableValueDto**](VariableValueDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_process_instance_variable_binary

> std::path::PathBuf get_process_instance_variable_binary(id, var_name)


Retrieves the content of a Process Variable by the Process Instance id and the Process Variable name. Applicable for byte array or file Process Variables.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the process instance to retrieve the variable for. | [required] |
**var_name** | **String** | The name of the variable to retrieve. | [required] |

### Return type

[**std::path::PathBuf**](std::path::PathBuf.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/octet-stream, text/plain, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_process_instance_variables

> ::std::collections::HashMap<String, crate::models::VariableValueDto> get_process_instance_variables(id, deserialize_value)


Retrieves all variables of a given process instance by id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the process instance to retrieve the variables from. | [required] |
**deserialize_value** | Option<**bool**> | Determines whether serializable variable values (typically variables that store custom Java objects) should be deserialized on server side (default true).  If set to true, a serializable variable will be deserialized on server side and transformed to JSON using [Jackson's](https://github.com/FasterXML/jackson) POJO/bean property introspection feature. Note that this requires the Java classes of the variable value to be on the REST API's classpath.  If set to false, a serializable variable will be returned in its serialized format. For example, a variable that is serialized as XML will be returned as a JSON string containing XML.  Note: While true is the default value for reasons of backward compatibility, we recommend setting this parameter to false when developing web applications that are independent of the Java process applications deployed to the engine. |  |[default to true]

### Return type

[**::std::collections::HashMap<String, crate::models::VariableValueDto>**](VariableValueDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_process_instances

> Vec<crate::models::ProcessInstanceDto> get_process_instances(sort_by, sort_order, first_result, max_results, process_instance_ids, business_key, business_key_like, case_instance_id, process_definition_id, process_definition_key, process_definition_key_in, process_definition_key_not_in, deployment_id, super_process_instance, sub_process_instance, super_case_instance, sub_case_instance, active, suspended, with_incident, incident_id, incident_type, incident_message, incident_message_like, tenant_id_in, without_tenant_id, process_definition_without_tenant_id, activity_id_in, root_process_instances, leaf_process_instances, variables, variable_names_ignore_case, variable_values_ignore_case)


Queries for process instances that fulfill given parameters. Parameters may be static as well as dynamic runtime properties of process instances. The size of the result set can be retrieved by using the Get Instance Count method.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**sort_by** | Option<**String**> | Sort the results lexicographically by a given criterion. Must be used in conjunction with the sortOrder parameter. |  |
**sort_order** | Option<**String**> | Sort the results in a given order. Values may be asc for ascending order or desc for descending order. Must be used in conjunction with the sortBy parameter. |  |
**first_result** | Option<**i32**> | Pagination of results. Specifies the index of the first result to return. |  |
**max_results** | Option<**i32**> | Pagination of results. Specifies the maximum number of results to return. Will return less results if there are no more results left. |  |
**process_instance_ids** | Option<**String**> | Filter by a comma-separated list of process instance ids. |  |
**business_key** | Option<**String**> | Filter by process instance business key. |  |
**business_key_like** | Option<**String**> | Filter by process instance business key that the parameter is a substring of. |  |
**case_instance_id** | Option<**String**> | Filter by case instance id. |  |
**process_definition_id** | Option<**String**> | Filter by the deployment the id belongs to. |  |
**process_definition_key** | Option<**String**> | Filter by the key of the process definition the instances run on. |  |
**process_definition_key_in** | Option<**String**> | Filter by a comma-separated list of process definition keys. A process instance must have one of the given process definition keys. |  |
**process_definition_key_not_in** | Option<**String**> | Exclude instances by a comma-separated list of process definition keys. A process instance must not have one of the given process definition keys. |  |
**deployment_id** | Option<**String**> | Filter by the deployment the id belongs to. |  |
**super_process_instance** | Option<**String**> | Restrict query to all process instances that are sub process instances of the given process instance. Takes a process instance id. |  |
**sub_process_instance** | Option<**String**> | Restrict query to all process instances that have the given process instance as a sub process instance. Takes a process instance id. |  |
**super_case_instance** | Option<**String**> | Restrict query to all process instances that are sub process instances of the given case instance. Takes a case instance id. |  |
**sub_case_instance** | Option<**String**> | Restrict query to all process instances that have the given case instance as a sub case instance. Takes a case instance id. |  |
**active** | Option<**bool**> | Only include active process instances. Value may only be true, as false is the default behavior. |  |[default to false]
**suspended** | Option<**bool**> | Only include suspended process instances. Value may only be true, as false is the default behavior. |  |[default to false]
**with_incident** | Option<**bool**> | Filter by presence of incidents. Selects only process instances that have an incident. |  |[default to false]
**incident_id** | Option<**String**> | Filter by the incident id. |  |
**incident_type** | Option<**String**> | Filter by the incident type. See the [User Guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/incidents/#incident-types) for a list of incident types. |  |
**incident_message** | Option<**String**> | Filter by the incident message. Exact match. |  |
**incident_message_like** | Option<**String**> | Filter by the incident message that the parameter is a substring of. |  |
**tenant_id_in** | Option<**String**> | Filter by a comma-separated list of tenant ids. A process instance must have one of the given tenant ids. |  |
**without_tenant_id** | Option<**bool**> | Only include process instances which belong to no tenant. |  |[default to false]
**process_definition_without_tenant_id** | Option<**bool**> | Only include process instances which process definition has no tenant id. |  |[default to false]
**activity_id_in** | Option<**String**> | Filter by a comma-separated list of activity ids. A process instance must currently wait in a leaf activity with one of the given activity ids. |  |
**root_process_instances** | Option<**bool**> | Restrict the query to all process instances that are top level process instances. |  |[default to false]
**leaf_process_instances** | Option<**bool**> | Restrict the query to all process instances that are leaf instances. (i.e. don't have any sub instances). |  |[default to false]
**variables** | Option<**String**> | Only include process instances that have variables with certain values. Variable filtering expressions are comma-separated and are structured as follows:  A valid parameter value has the form `key_operator_value`. `key` is the variable name, `operator` is the comparison operator to be used and `value` the variable value.  **Note**: Values are always treated as String objects on server side.  Valid `operator` values are: `eq` - equal to; `neq` - not equal to; `gt` - greater than; `gteq` - greater than or equal to; `lt` - lower than; `lteq` - lower than or equal to; `like`. `key` and `value` may not contain underscore or comma characters. |  |
**variable_names_ignore_case** | Option<**bool**> | Match all variable names in this query case-insensitively. If set to true variableName and variablename are treated as equal. |  |[default to false]
**variable_values_ignore_case** | Option<**bool**> | Match all variable values in this query case-insensitively. If set to true variableValue and variablevalue are treated as equal. |  |[default to false]

### Return type

[**Vec<crate::models::ProcessInstanceDto>**](ProcessInstanceDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_process_instances_count

> crate::models::CountResultDto get_process_instances_count(process_instance_ids, business_key, business_key_like, case_instance_id, process_definition_id, process_definition_key, process_definition_key_in, process_definition_key_not_in, deployment_id, super_process_instance, sub_process_instance, super_case_instance, sub_case_instance, active, suspended, with_incident, incident_id, incident_type, incident_message, incident_message_like, tenant_id_in, without_tenant_id, process_definition_without_tenant_id, activity_id_in, root_process_instances, leaf_process_instances, variables, variable_names_ignore_case, variable_values_ignore_case)


Queries for the number of process instances that fulfill given parameters.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**process_instance_ids** | Option<**String**> | Filter by a comma-separated list of process instance ids. |  |
**business_key** | Option<**String**> | Filter by process instance business key. |  |
**business_key_like** | Option<**String**> | Filter by process instance business key that the parameter is a substring of. |  |
**case_instance_id** | Option<**String**> | Filter by case instance id. |  |
**process_definition_id** | Option<**String**> | Filter by the deployment the id belongs to. |  |
**process_definition_key** | Option<**String**> | Filter by the key of the process definition the instances run on. |  |
**process_definition_key_in** | Option<**String**> | Filter by a comma-separated list of process definition keys. A process instance must have one of the given process definition keys. |  |
**process_definition_key_not_in** | Option<**String**> | Exclude instances by a comma-separated list of process definition keys. A process instance must not have one of the given process definition keys. |  |
**deployment_id** | Option<**String**> | Filter by the deployment the id belongs to. |  |
**super_process_instance** | Option<**String**> | Restrict query to all process instances that are sub process instances of the given process instance. Takes a process instance id. |  |
**sub_process_instance** | Option<**String**> | Restrict query to all process instances that have the given process instance as a sub process instance. Takes a process instance id. |  |
**super_case_instance** | Option<**String**> | Restrict query to all process instances that are sub process instances of the given case instance. Takes a case instance id. |  |
**sub_case_instance** | Option<**String**> | Restrict query to all process instances that have the given case instance as a sub case instance. Takes a case instance id. |  |
**active** | Option<**bool**> | Only include active process instances. Value may only be true, as false is the default behavior. |  |[default to false]
**suspended** | Option<**bool**> | Only include suspended process instances. Value may only be true, as false is the default behavior. |  |[default to false]
**with_incident** | Option<**bool**> | Filter by presence of incidents. Selects only process instances that have an incident. |  |[default to false]
**incident_id** | Option<**String**> | Filter by the incident id. |  |
**incident_type** | Option<**String**> | Filter by the incident type. See the [User Guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/incidents/#incident-types) for a list of incident types. |  |
**incident_message** | Option<**String**> | Filter by the incident message. Exact match. |  |
**incident_message_like** | Option<**String**> | Filter by the incident message that the parameter is a substring of. |  |
**tenant_id_in** | Option<**String**> | Filter by a comma-separated list of tenant ids. A process instance must have one of the given tenant ids. |  |
**without_tenant_id** | Option<**bool**> | Only include process instances which belong to no tenant. |  |[default to false]
**process_definition_without_tenant_id** | Option<**bool**> | Only include process instances which process definition has no tenant id. |  |[default to false]
**activity_id_in** | Option<**String**> | Filter by a comma-separated list of activity ids. A process instance must currently wait in a leaf activity with one of the given activity ids. |  |
**root_process_instances** | Option<**bool**> | Restrict the query to all process instances that are top level process instances. |  |[default to false]
**leaf_process_instances** | Option<**bool**> | Restrict the query to all process instances that are leaf instances. (i.e. don't have any sub instances). |  |[default to false]
**variables** | Option<**String**> | Only include process instances that have variables with certain values. Variable filtering expressions are comma-separated and are structured as follows:  A valid parameter value has the form `key_operator_value`. `key` is the variable name, `operator` is the comparison operator to be used and `value` the variable value.  **Note**: Values are always treated as String objects on server side.  Valid `operator` values are: `eq` - equal to; `neq` - not equal to; `gt` - greater than; `gteq` - greater than or equal to; `lt` - lower than; `lteq` - lower than or equal to; `like`. `key` and `value` may not contain underscore or comma characters. |  |
**variable_names_ignore_case** | Option<**bool**> | Match all variable names in this query case-insensitively. If set to true variableName and variablename are treated as equal. |  |[default to false]
**variable_values_ignore_case** | Option<**bool**> | Match all variable values in this query case-insensitively. If set to true variableValue and variablevalue are treated as equal. |  |[default to false]

### Return type

[**crate::models::CountResultDto**](CountResultDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## modify_process_instance

> modify_process_instance(id, process_instance_modification_dto)


Submits a list of modification instructions to change a process instance's execution state. A modification instruction is one of the following:  * Starting execution before an activity * Starting execution after an activity on its single outgoing sequence flow * Starting execution on a specific sequence flow * Canceling an activity instance, transition instance, or all instances (activity or transition) for an activity  Instructions are executed immediately and in the order they are provided in this request's body. Variables can be provided with every starting instruction.  The exact semantics of modification can be read about in the [User guide](https://docs.camunda.org/manual/develop/user-guide/process-engine/process-instance-modification/).

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the process instance to modify. | [required] |
**process_instance_modification_dto** | Option<[**ProcessInstanceModificationDto**](ProcessInstanceModificationDto.md)> |  |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## modify_process_instance_async_operation

> crate::models::BatchDto modify_process_instance_async_operation(id, process_instance_modification_dto)


Submits a list of modification instructions to change a process instance's execution state async. A modification instruction is one of the following:  * Starting execution before an activity * Starting execution after an activity on its single outgoing sequence flow * Starting execution on a specific sequence flow * Cancelling an activity instance, transition instance, or all instances (activity or transition) for an activity  Instructions are executed asynchronous and in the order they are provided in this request's body. Variables can be provided with every starting instruction.  The exact semantics of modification can be read about in the [User guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/process-instance-modification/).

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the process instance to modify. | [required] |
**process_instance_modification_dto** | Option<[**ProcessInstanceModificationDto**](ProcessInstanceModificationDto.md)> |  |  |

### Return type

[**crate::models::BatchDto**](BatchDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## modify_process_instance_variables

> modify_process_instance_variables(id, patch_variables_dto)


Updates or deletes the variables of a process instance by id. Updates precede deletions. So, if a variable is updated AND deleted, the deletion overrides the update.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the process instance to set variables for. | [required] |
**patch_variables_dto** | Option<[**PatchVariablesDto**](PatchVariablesDto.md)> |  |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## query_process_instances

> Vec<crate::models::ProcessInstanceDto> query_process_instances(first_result, max_results, process_instance_query_dto)


Queries for process instances that fulfill given parameters through a JSON object. This method is slightly more powerful than the Get Instances method because it allows filtering by multiple process variables of types `string`, `number` or `boolean`.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**first_result** | Option<**i32**> | Pagination of results. Specifies the index of the first result to return. |  |
**max_results** | Option<**i32**> | Pagination of results. Specifies the maximum number of results to return. Will return less results if there are no more results left. |  |
**process_instance_query_dto** | Option<[**ProcessInstanceQueryDto**](ProcessInstanceQueryDto.md)> |  |  |

### Return type

[**Vec<crate::models::ProcessInstanceDto>**](ProcessInstanceDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## query_process_instances_count

> crate::models::CountResultDto query_process_instances_count(process_instance_query_dto)


Queries for the number of process instances that fulfill the given parameters. This method takes the same message body as the Get Instances (POST) method and therefore it is slightly more powerful than the Get Instance Count method.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**process_instance_query_dto** | Option<[**ProcessInstanceQueryDto**](ProcessInstanceQueryDto.md)> |  |  |

### Return type

[**crate::models::CountResultDto**](CountResultDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## set_process_instance_variable

> set_process_instance_variable(id, var_name, variable_value_dto)


Sets a variable of a given process instance by id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the process instance to set the variable for. | [required] |
**var_name** | **String** | The name of the variable to set. | [required] |
**variable_value_dto** | Option<[**VariableValueDto**](VariableValueDto.md)> |  |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## set_process_instance_variable_binary

> set_process_instance_variable_binary(id, var_name, data, value_type)


Sets the serialized value for a binary variable or the binary value for a file variable.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the process instance to retrieve the variable for. | [required] |
**var_name** | **String** | The name of the variable to retrieve. | [required] |
**data** | Option<**std::path::PathBuf**> | The binary data to be set. For File variables, this multipart can contain the filename, binary value and MIME type of the file variable to be set Only the filename is mandatory. |  |
**value_type** | Option<**String**> | The name of the variable type. Either Bytes for a byte array variable or File for a file variable. |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: multipart/form-data
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## set_retries_by_process

> crate::models::BatchDto set_retries_by_process(set_job_retries_by_process_dto)


Create a batch to set retries of jobs associated with given processes asynchronously.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**set_job_retries_by_process_dto** | Option<[**SetJobRetriesByProcessDto**](SetJobRetriesByProcessDto.md)> | Please note that if both processInstances and processInstanceQuery are provided, then the resulting execution will be performed on the union of these sets. **Unallowed property**: `historicProcessInstanceQuery` |  |

### Return type

[**crate::models::BatchDto**](BatchDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## set_retries_by_process_historic_query_based

> crate::models::BatchDto set_retries_by_process_historic_query_based(set_job_retries_by_process_dto)


Create a batch to set retries of jobs asynchronously based on a historic process instance query.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**set_job_retries_by_process_dto** | Option<[**SetJobRetriesByProcessDto**](SetJobRetriesByProcessDto.md)> | Please note that if both processInstances and historicProcessInstanceQuery are provided, then the resulting execution will be performed on the union of these sets. **Unallowed property**: `processInstanceQuery` |  |

### Return type

[**crate::models::BatchDto**](BatchDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## set_variables_async_operation

> crate::models::BatchDto set_variables_async_operation(set_variables_async_dto)


Update or create runtime process variables in the root scope of process instances.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**set_variables_async_dto** | Option<[**SetVariablesAsyncDto**](SetVariablesAsyncDto.md)> |  |  |

### Return type

[**crate::models::BatchDto**](BatchDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## update_suspension_state

> update_suspension_state(process_instance_suspension_state_dto)


Activates or suspends process instances by providing certain criteria:  # Activate/Suspend Process Instance By Process Definition Id * `suspend` * `processDefinitionId`  # Activate/Suspend Process Instance By Process Definition Key  * `suspend` * `processDefinitionKey` * `processDefinitionTenantId` * `processDefinitionWithoutTenantId`  # Activate/Suspend Process Instance In Group * `suspend` * `processInstanceIds` * `processInstanceQuery` * `historicProcessInstanceQuery`

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**process_instance_suspension_state_dto** | Option<[**ProcessInstanceSuspensionStateDto**](ProcessInstanceSuspensionStateDto.md)> |  |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## update_suspension_state_async_operation

> crate::models::BatchDto update_suspension_state_async_operation(process_instance_suspension_state_async_dto)


Activates or suspends process instances asynchronously with a list of process instance ids, a process instance query, and/or a historical process instance query.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**process_instance_suspension_state_async_dto** | Option<[**ProcessInstanceSuspensionStateAsyncDto**](ProcessInstanceSuspensionStateAsyncDto.md)> |  |  |

### Return type

[**crate::models::BatchDto**](BatchDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## update_suspension_state_by_id

> update_suspension_state_by_id(id, suspension_state_dto)


Activates or suspends a given process instance by id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the process instance to activate or suspend. | [required] |
**suspension_state_dto** | Option<[**SuspensionStateDto**](SuspensionStateDto.md)> |  |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

