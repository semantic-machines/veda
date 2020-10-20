# \TaskApi

All URIs are relative to *http://localhost:8080/engine-rest*

Method | HTTP request | Description
------------- | ------------- | -------------
[**claim**](TaskApi.md#claim) | **post** /task/{id}/claim | 
[**complete**](TaskApi.md#complete) | **post** /task/{id}/complete | 
[**create_task**](TaskApi.md#create_task) | **post** /task/create | 
[**delegate_task**](TaskApi.md#delegate_task) | **post** /task/{id}/delegate | 
[**delete_task**](TaskApi.md#delete_task) | **delete** /task/{id} | 
[**get_deployed_form**](TaskApi.md#get_deployed_form) | **get** /task/{id}/deployed-form | 
[**get_form**](TaskApi.md#get_form) | **get** /task/{id}/form | 
[**get_form_variables**](TaskApi.md#get_form_variables) | **get** /task/{id}/form-variables | 
[**get_rendered_form**](TaskApi.md#get_rendered_form) | **get** /task/{id}/rendered-form | 
[**get_task**](TaskApi.md#get_task) | **get** /task/{id} | 
[**get_tasks**](TaskApi.md#get_tasks) | **get** /task | 
[**get_tasks_count**](TaskApi.md#get_tasks_count) | **get** /task/count | 
[**handle_bpmn_error**](TaskApi.md#handle_bpmn_error) | **post** /task/{id}/bpmnError | 
[**handle_escalation**](TaskApi.md#handle_escalation) | **post** /task/{id}/bpmnEscalation | 
[**query_tasks**](TaskApi.md#query_tasks) | **post** /task | 
[**query_tasks_count**](TaskApi.md#query_tasks_count) | **post** /task/count | 
[**resolve**](TaskApi.md#resolve) | **post** /task/{id}/resolve | 
[**set_assignee**](TaskApi.md#set_assignee) | **post** /task/{id}/assignee | 
[**submit**](TaskApi.md#submit) | **post** /task/{id}/submit-form | 
[**unclaim**](TaskApi.md#unclaim) | **post** /task/{id}/unclaim | 
[**update_task**](TaskApi.md#update_task) | **put** /task/{id} | 



## claim

> claim(id, user_id_dto)


Claims a task for a specific user.  **Note:** The difference with the [Set Assignee](https://docs.camunda.org/manual/7.14/reference/rest/task/post-assignee/) method is that here a check is performed to see if the task already has a user assigned to it.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task to claim. | [required] |
**user_id_dto** | Option<[**UserIdDto**](UserIdDto.md)> | Provide the id of the user that claims the task. |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## complete

> ::std::collections::HashMap<String, crate::models::VariableValueDto> complete(id, complete_task_dto)


Completes a task and updates process variables.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task to complete. | [required] |
**complete_task_dto** | Option<[**CompleteTaskDto**](CompleteTaskDto.md)> |  |  |

### Return type

[**::std::collections::HashMap<String, crate::models::VariableValueDto>**](VariableValueDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## create_task

> create_task(task_dto)


Creates a new task.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**task_dto** | Option<[**TaskDto**](TaskDto.md)> |  |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## delegate_task

> delegate_task(id, user_id_dto)


Delegates a task to another user.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task to delegate. | [required] |
**user_id_dto** | Option<[**UserIdDto**](UserIdDto.md)> | Provide the id of the user that the task should be delegated to. |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## delete_task

> delete_task(id)


Removes a task by id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task to be removed. | [required] |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_deployed_form

> std::path::PathBuf get_deployed_form(id)


Retrieves the deployed form that is referenced from a given task. For further information please refer to the [User Guide](https://docs.camunda.org/manual/7.14/user-guide/task-forms/#embedded-task-forms).

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task to get the deployed form for. | [required] |

### Return type

[**std::path::PathBuf**](std::path::PathBuf.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/xhtml+xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_form

> crate::models::FormDto get_form(id)


Retrieves the form key for a task. The form key corresponds to the `FormData#formKey` property in the engine. This key can be used to do task-specific form rendering in client applications. Additionally, the context path of the containing process application is returned.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task to retrieve the form for. | [required] |

### Return type

[**crate::models::FormDto**](FormDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_form_variables

> ::std::collections::HashMap<String, crate::models::VariableValueDto> get_form_variables(id, variable_names, deserialize_values)


Retrieves the form variables for a task. The form variables take form data specified on the task into account. If form fields are defined, the variable types and default values of the form fields are taken into account.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task to retrieve the variables for. | [required] |
**variable_names** | Option<**String**> | A comma-separated list of variable names. Allows restricting the list of requested variables to the variable names in the list. It is best practice to restrict the list of variables to the variables actually required by the form in order to minimize fetching of data. If the query parameter is ommitted all variables are fetched. If the query parameter contains non-existent variable names, the variable names are ignored. |  |
**deserialize_values** | Option<**bool**> | Determines whether serializable variable values (typically variables that store custom Java objects) should be deserialized on server side (default true).  If set to true, a serializable variable will be deserialized on server side and transformed to JSON using [Jackson's](http://jackson.codehaus.org/) POJO/bean property introspection feature. Note that this requires the Java classes of the variable value to be on the REST API's classpath.  If set to false, a serializable variable will be returned in its serialized format. For example, a variable that is serialized as XML will be returned as a JSON string containing XML.  Note: While true is the default value for reasons of backward compatibility, we recommend setting this parameter to false when developing web applications that are independent of the Java process applications deployed to the engine. |  |[default to true]

### Return type

[**::std::collections::HashMap<String, crate::models::VariableValueDto>**](VariableValueDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_rendered_form

> std::path::PathBuf get_rendered_form(id)


Retrieves the rendered form for a task. This method can be used to get the HTML rendering of a [Generated Task Form](https://docs.camunda.org/manual/7.14/user-guide/task-forms/#generated-task-forms).

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task to get the rendered form for. | [required] |

### Return type

[**std::path::PathBuf**](std::path::PathBuf.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/xhtml+xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_task

> crate::models::TaskDto get_task(id)


Retrieves a task by id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task to be retrieved. | [required] |

### Return type

[**crate::models::TaskDto**](TaskDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_tasks

> Vec<crate::models::TaskDto> get_tasks(process_instance_id, process_instance_id_in, process_instance_business_key, process_instance_business_key_expression, process_instance_business_key_in, process_instance_business_key_like, process_instance_business_key_like_expression, process_definition_id, process_definition_key, process_definition_key_in, process_definition_name, process_definition_name_like, execution_id, case_instance_id, case_instance_business_key, case_instance_business_key_like, case_definition_id, case_definition_key, case_definition_name, case_definition_name_like, case_execution_id, activity_instance_id_in, tenant_id_in, without_tenant_id, assignee, assignee_expression, assignee_like, assignee_like_expression, assignee_in, owner, owner_expression, candidate_group, candidate_group_expression, candidate_user, candidate_user_expression, include_assigned_tasks, involved_user, involved_user_expression, assigned, unassigned, task_definition_key, task_definition_key_in, task_definition_key_like, name, name_not_equal, name_like, name_not_like, description, description_like, priority, max_priority, min_priority, due_date, due_date_expression, due_after, due_after_expression, due_before, due_before_expression, follow_up_date, follow_up_date_expression, follow_up_after, follow_up_after_expression, follow_up_before, follow_up_before_expression, follow_up_before_or_not_existent, follow_up_before_or_not_existent_expression, created_on, created_on_expression, created_after, created_after_expression, created_before, created_before_expression, delegation_state, candidate_groups, candidate_groups_expression, with_candidate_groups, without_candidate_groups, with_candidate_users, without_candidate_users, active, suspended, task_variables, process_variables, case_instance_variables, variable_names_ignore_case, variable_values_ignore_case, parent_task_id, sort_by, sort_order, first_result, max_results)


Queries for tasks that fulfill a given filter. The size of the result set can be retrieved by using the Get Task Count method.  **Security Consideration:** There are several query parameters (such as assigneeExpression) for specifying an EL expression. These are disabled by default to prevent remote code execution. See the section on [security considerations](https://docs.camunda.org/manual/7.14/user-guide/process-engine/securing-custom-code/) for custom code in the user guide for details.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**process_instance_id** | Option<**String**> | Restrict to tasks that belong to process instances with the given id. |  |
**process_instance_id_in** | Option<**String**> | Restrict to tasks that belong to process instances with the given ids. |  |
**process_instance_business_key** | Option<**String**> | Restrict to tasks that belong to process instances with the given business key. |  |
**process_instance_business_key_expression** | Option<**String**> | Restrict to tasks that belong to process instances with the given business key which  is described by an expression. See the  [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions) for more information on available functions. |  |
**process_instance_business_key_in** | Option<**String**> | Restrict to tasks that belong to process instances with one of the give business keys.  The keys need to be in a comma-separated list. |  |
**process_instance_business_key_like** | Option<**String**> | Restrict to tasks that have a process instance business key that has the parameter  value as a substring. |  |
**process_instance_business_key_like_expression** | Option<**String**> | Restrict to tasks that have a process instance business key that has the parameter  value as a substring and is described by an expression. See the [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions)  for more information on available functions. |  |
**process_definition_id** | Option<**String**> | Restrict to tasks that belong to a process definition with the given id. |  |
**process_definition_key** | Option<**String**> | Restrict to tasks that belong to a process definition with the given key. |  |
**process_definition_key_in** | Option<**String**> | Restrict to tasks that belong to a process definition with one of the given keys. The  keys need to be in a comma-separated list. |  |
**process_definition_name** | Option<**String**> | Restrict to tasks that belong to a process definition with the given name. |  |
**process_definition_name_like** | Option<**String**> | Restrict to tasks that have a process definition name that has the parameter value as  a substring. |  |
**execution_id** | Option<**String**> | Restrict to tasks that belong to an execution with the given id. |  |
**case_instance_id** | Option<**String**> | Restrict to tasks that belong to case instances with the given id. |  |
**case_instance_business_key** | Option<**String**> | Restrict to tasks that belong to case instances with the given business key. |  |
**case_instance_business_key_like** | Option<**String**> | Restrict to tasks that have a case instance business key that has the parameter value  as a substring. |  |
**case_definition_id** | Option<**String**> | Restrict to tasks that belong to a case definition with the given id. |  |
**case_definition_key** | Option<**String**> | Restrict to tasks that belong to a case definition with the given key. |  |
**case_definition_name** | Option<**String**> | Restrict to tasks that belong to a case definition with the given name. |  |
**case_definition_name_like** | Option<**String**> | Restrict to tasks that have a case definition name that has the parameter value as a  substring. |  |
**case_execution_id** | Option<**String**> | Restrict to tasks that belong to a case execution with the given id. |  |
**activity_instance_id_in** | Option<**String**> | Only include tasks which belong to one of the passed and comma-separated activity  instance ids. |  |
**tenant_id_in** | Option<**String**> | Only include tasks which belong to one of the passed and comma-separated  tenant ids. |  |
**without_tenant_id** | Option<**bool**> | Only include tasks which belong to no tenant. Value may only be `true`,  as `false` is the default behavior. |  |[default to false]
**assignee** | Option<**String**> | Restrict to tasks that the given user is assigned to. |  |
**assignee_expression** | Option<**String**> | Restrict to tasks that the user described by the given expression is assigned to.  See the  [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions)  for more information on available functions. |  |
**assignee_like** | Option<**String**> | Restrict to tasks that have an assignee that has the parameter  value as a substring. |  |
**assignee_like_expression** | Option<**String**> | Restrict to tasks that have an assignee that has the parameter value described by the  given expression as a substring. See the  [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions)  for more information on available functions. |  |
**assignee_in** | Option<**String**> | Only include tasks which are assigned to one of the passed and  comma-separated user ids. |  |
**owner** | Option<**String**> | Restrict to tasks that the given user owns. |  |
**owner_expression** | Option<**String**> | Restrict to tasks that the user described by the given expression owns. See the  [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions)  for more information on available functions. |  |
**candidate_group** | Option<**String**> | Only include tasks that are offered to the given group. |  |
**candidate_group_expression** | Option<**String**> | Only include tasks that are offered to the group described by the given expression.  See the  [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions)  for more information on available functions. |  |
**candidate_user** | Option<**String**> | Only include tasks that are offered to the given user or to one of his groups. |  |
**candidate_user_expression** | Option<**String**> | Only include tasks that are offered to the user described by the given expression.  See the  [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions)  for more information on available functions. |  |
**include_assigned_tasks** | Option<**bool**> | Also include tasks that are assigned to users in candidate queries. Default is to only  include tasks that are not assigned to any user if you query by candidate user or group(s). |  |[default to false]
**involved_user** | Option<**String**> | Only include tasks that the given user is involved in. A user is involved in a task if  an identity link exists between task and user (e.g., the user is the assignee). |  |
**involved_user_expression** | Option<**String**> | Only include tasks that the user described by the given expression is involved in. A user is involved in a task if an identity link exists between task and user (e.g., the user is the assignee). See the [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions) for more information on available functions. |  |
**assigned** | Option<**bool**> | If set to `true`, restricts the query to all tasks that are assigned. |  |[default to false]
**unassigned** | Option<**bool**> | If set to `true`, restricts the query to all tasks that are unassigned. |  |[default to false]
**task_definition_key** | Option<**String**> | Restrict to tasks that have the given key. |  |
**task_definition_key_in** | Option<**String**> | Restrict to tasks that have one of the given keys. The keys need to be in a comma-separated list. |  |
**task_definition_key_like** | Option<**String**> | Restrict to tasks that have a key that has the parameter value as a substring. |  |
**name** | Option<**String**> | Restrict to tasks that have the given name. |  |
**name_not_equal** | Option<**String**> | Restrict to tasks that do not have the given name. |  |
**name_like** | Option<**String**> | Restrict to tasks that have a name with the given parameter value as substring. |  |
**name_not_like** | Option<**String**> | Restrict to tasks that do not have a name with the given parameter value as substring. |  |
**description** | Option<**String**> | Restrict to tasks that have the given description. |  |
**description_like** | Option<**String**> | Restrict to tasks that have a description that has the parameter value as a substring. |  |
**priority** | Option<**i32**> | Restrict to tasks that have the given priority. |  |
**max_priority** | Option<**i32**> | Restrict to tasks that have a lower or equal priority. |  |
**min_priority** | Option<**i32**> | Restrict to tasks that have a higher or equal priority. |  |
**due_date** | Option<**String**> | Restrict to tasks that are due on the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.546+0200`. |  |
**due_date_expression** | Option<**String**> | Restrict to tasks that are due on the date described by the given expression. See the [User Guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions) for more information on available functions. The expression must evaluate to a `java.util.Date` or `org.joda.time.DateTime` object. |  |
**due_after** | Option<**String**> | Restrict to tasks that are due after the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.435+0200`. |  |
**due_after_expression** | Option<**String**> | Restrict to tasks that are due after the date described by the given expression. See the [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions) for more information on available functions. The expression must evaluate to a `java.util.Date` or `org.joda.time.DateTime` object. |  |
**due_before** | Option<**String**> | Restrict to tasks that are due before the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.243+0200`. |  |
**due_before_expression** | Option<**String**> | Restrict to tasks that are due before the date described by the given expression. See the [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions) for more information on available functions. The expression must evaluate to a `java.util.Date` or `org.joda.time.DateTime` object. |  |
**follow_up_date** | Option<**String**> | Restrict to tasks that have a followUp date on the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.342+0200`. |  |
**follow_up_date_expression** | Option<**String**> | Restrict to tasks that have a followUp date on the date described by the given expression. See the [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions) for more information on available functions. The expression must evaluate to a `java.util.Date` or `org.joda.time.DateTime` object. |  |
**follow_up_after** | Option<**String**> | Restrict to tasks that have a followUp date after the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.542+0200`. |  |
**follow_up_after_expression** | Option<**String**> | Restrict to tasks that have a followUp date after the date described by the given expression. See the [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions) for more information on available functions. The expression must evaluate to a `java.util.Date` or `org.joda.time.DateTime` object. |  |
**follow_up_before** | Option<**String**> | Restrict to tasks that have a followUp date before the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.234+0200`. |  |
**follow_up_before_expression** | Option<**String**> | Restrict to tasks that have a followUp date before the date described by the given expression. See the [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions) for more information on available functions. The expression must evaluate to a `java.util.Date` or `org.joda.time.DateTime` object. |  |
**follow_up_before_or_not_existent** | Option<**String**> | Restrict to tasks that have no followUp date or a followUp date before the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.432+0200`. The typical use case is to query all `active` tasks for a user for a given date. |  |
**follow_up_before_or_not_existent_expression** | Option<**String**> | Restrict to tasks that have no followUp date or a followUp date before the date described by the given expression. See the [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions) for more information on available functions. The expression must evaluate to a `java.util.Date` or `org.joda.time.DateTime` object. |  |
**created_on** | Option<**String**> | Restrict to tasks that were created on the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.324+0200`. |  |
**created_on_expression** | Option<**String**> | Restrict to tasks that were created on the date described by the given expression. See the [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions) for more information on available functions. The expression must evaluate to a `java.util.Date` or `org.joda.time.DateTime` object. |  |
**created_after** | Option<**String**> | Restrict to tasks that were created after the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.342+0200`. |  |
**created_after_expression** | Option<**String**> | Restrict to tasks that were created after the date described by the given expression. See the [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions) for more information on available functions. The expression must evaluate to a `java.util.Date` or `org.joda.time.DateTime` object. |  |
**created_before** | Option<**String**> | Restrict to tasks that were created before the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.332+0200`. |  |
**created_before_expression** | Option<**String**> | Restrict to tasks that were created before the date described by the given expression. See the [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions) for more information on available functions. The expression must evaluate to a `java.util.Date` or `org.joda.time.DateTime` object. |  |
**delegation_state** | Option<**String**> | Restrict to tasks that are in the given delegation state. Valid values are `PENDING` and `RESOLVED`. |  |
**candidate_groups** | Option<**String**> | Restrict to tasks that are offered to any of the given candidate groups. Takes a comma-separated list of group names, so for example `developers,support,sales`. |  |
**candidate_groups_expression** | Option<**String**> | Restrict to tasks that are offered to any of the candidate groups described by the given expression. See the [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions) for more information on available functions. The expression must evaluate to `java.util.List` of Strings. |  |
**with_candidate_groups** | Option<**bool**> | Only include tasks which have a candidate group. Value may only be `true`, as `false` is the default behavior. |  |[default to false]
**without_candidate_groups** | Option<**bool**> | Only include tasks which have no candidate group. Value may only be `true`, as `false` is the default behavior. |  |[default to false]
**with_candidate_users** | Option<**bool**> | Only include tasks which have a candidate user. Value may only be `true`, as `false` is the default behavior. |  |[default to false]
**without_candidate_users** | Option<**bool**> | Only include tasks which have no candidate users. Value may only be `true`, as `false` is the default behavior. |  |[default to false]
**active** | Option<**bool**> | Only include active tasks. Value may only be `true`, as `false` is the default behavior. |  |[default to false]
**suspended** | Option<**bool**> | Only include suspended tasks. Value may only be `true`, as `false` is the default behavior. |  |[default to false]
**task_variables** | Option<**String**> | Only include tasks that have variables with certain values. Variable filtering expressions are comma-separated and are structured as follows:  A valid parameter value has the form `key_operator_value`. `key` is the variable name, `operator` is the comparison operator to be used and `value` the variable value.  **Note**: Values are always treated as String objects on server side.  Valid `operator` values are: `eq` - equal to; `neq` - not equal to; `gt` - greater than; `gteq` - greater than or equal to; `lt` - lower than; `lteq` - lower than or equal to; `like`. `key` and `value` may not contain underscore or comma characters. |  |
**process_variables** | Option<**String**> | Only include tasks that belong to process instances that have variables with certain  values. Variable filtering expressions are comma-separated and are structured as follows:  A valid parameter value has the form `key_operator_value`. `key` is the variable name, `operator` is the comparison operator to be used and `value` the variable value.  **Note**: Values are always treated as String objects on server side.  Valid `operator` values are: `eq` - equal to; `neq` - not equal to; `gt` - greater than; `gteq` - greater than or equal to; `lt` - lower than; `lteq` - lower than or equal to; `like`. `key` and `value` may not contain underscore or comma characters. |  |
**case_instance_variables** | Option<**String**> | Only include tasks that belong to case instances that have variables with certain values. Variable filtering expressions are comma-separated and are structured as follows:  A valid parameter value has the form `key_operator_value`. `key` is the variable name, `operator` is the comparison operator to be used and `value` the variable value.  **Note**: Values are always treated as String objects on server side.  Valid `operator` values are: `eq` - equal to; `neq` - not equal to; `gt` - greater than; `gteq` - greater than or equal to; `lt` - lower than; `lteq` - lower than or equal to; `like`. `key` and `value` may not contain underscore or comma characters. |  |
**variable_names_ignore_case** | Option<**bool**> | Match all variable names in this query case-insensitively. If set `variableName` and `variablename` are treated as equal. |  |[default to false]
**variable_values_ignore_case** | Option<**bool**> | Match all variable values in this query case-insensitively. If set `variableValue` and `variablevalue` are treated as equal. |  |[default to false]
**parent_task_id** | Option<**String**> | Restrict query to all tasks that are sub tasks of the given task. Takes a task id. |  |
**sort_by** | Option<**String**> | Sort the results lexicographically by a given criterion. Must be used in conjunction with the sortOrder parameter. |  |
**sort_order** | Option<**String**> | Sort the results in a given order. Values may be asc for ascending order or desc for descending order. Must be used in conjunction with the sortBy parameter. |  |
**first_result** | Option<**i32**> | Pagination of results. Specifies the index of the first result to return. |  |
**max_results** | Option<**i32**> | Pagination of results. Specifies the maximum number of results to return. Will return less results if there are no more results left. |  |

### Return type

[**Vec<crate::models::TaskDto>**](TaskDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_tasks_count

> crate::models::CountResultDto get_tasks_count(process_instance_id, process_instance_id_in, process_instance_business_key, process_instance_business_key_expression, process_instance_business_key_in, process_instance_business_key_like, process_instance_business_key_like_expression, process_definition_id, process_definition_key, process_definition_key_in, process_definition_name, process_definition_name_like, execution_id, case_instance_id, case_instance_business_key, case_instance_business_key_like, case_definition_id, case_definition_key, case_definition_name, case_definition_name_like, case_execution_id, activity_instance_id_in, tenant_id_in, without_tenant_id, assignee, assignee_expression, assignee_like, assignee_like_expression, assignee_in, owner, owner_expression, candidate_group, candidate_group_expression, candidate_user, candidate_user_expression, include_assigned_tasks, involved_user, involved_user_expression, assigned, unassigned, task_definition_key, task_definition_key_in, task_definition_key_like, name, name_not_equal, name_like, name_not_like, description, description_like, priority, max_priority, min_priority, due_date, due_date_expression, due_after, due_after_expression, due_before, due_before_expression, follow_up_date, follow_up_date_expression, follow_up_after, follow_up_after_expression, follow_up_before, follow_up_before_expression, follow_up_before_or_not_existent, follow_up_before_or_not_existent_expression, created_on, created_on_expression, created_after, created_after_expression, created_before, created_before_expression, delegation_state, candidate_groups, candidate_groups_expression, with_candidate_groups, without_candidate_groups, with_candidate_users, without_candidate_users, active, suspended, task_variables, process_variables, case_instance_variables, variable_names_ignore_case, variable_values_ignore_case, parent_task_id)


Retrieves the number of tasks that fulfill a provided filter. Corresponds to the size of the result set when using the [Get Tasks](https://docs.camunda.org/manual/7.14/reference/rest/task/) method.  **Security Consideration:** There are several query parameters (such as assigneeExpression) for specifying an EL expression. These are disabled by default to prevent remote code execution. See the section on [security considerations](https://docs.camunda.org/manual/7.14/user-guide/process-engine/securing-custom-code/) for custom code in the user guide for details.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**process_instance_id** | Option<**String**> | Restrict to tasks that belong to process instances with the given id. |  |
**process_instance_id_in** | Option<**String**> | Restrict to tasks that belong to process instances with the given ids. |  |
**process_instance_business_key** | Option<**String**> | Restrict to tasks that belong to process instances with the given business key. |  |
**process_instance_business_key_expression** | Option<**String**> | Restrict to tasks that belong to process instances with the given business key which  is described by an expression. See the  [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions) for more information on available functions. |  |
**process_instance_business_key_in** | Option<**String**> | Restrict to tasks that belong to process instances with one of the give business keys.  The keys need to be in a comma-separated list. |  |
**process_instance_business_key_like** | Option<**String**> | Restrict to tasks that have a process instance business key that has the parameter  value as a substring. |  |
**process_instance_business_key_like_expression** | Option<**String**> | Restrict to tasks that have a process instance business key that has the parameter  value as a substring and is described by an expression. See the [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions)  for more information on available functions. |  |
**process_definition_id** | Option<**String**> | Restrict to tasks that belong to a process definition with the given id. |  |
**process_definition_key** | Option<**String**> | Restrict to tasks that belong to a process definition with the given key. |  |
**process_definition_key_in** | Option<**String**> | Restrict to tasks that belong to a process definition with one of the given keys. The  keys need to be in a comma-separated list. |  |
**process_definition_name** | Option<**String**> | Restrict to tasks that belong to a process definition with the given name. |  |
**process_definition_name_like** | Option<**String**> | Restrict to tasks that have a process definition name that has the parameter value as  a substring. |  |
**execution_id** | Option<**String**> | Restrict to tasks that belong to an execution with the given id. |  |
**case_instance_id** | Option<**String**> | Restrict to tasks that belong to case instances with the given id. |  |
**case_instance_business_key** | Option<**String**> | Restrict to tasks that belong to case instances with the given business key. |  |
**case_instance_business_key_like** | Option<**String**> | Restrict to tasks that have a case instance business key that has the parameter value  as a substring. |  |
**case_definition_id** | Option<**String**> | Restrict to tasks that belong to a case definition with the given id. |  |
**case_definition_key** | Option<**String**> | Restrict to tasks that belong to a case definition with the given key. |  |
**case_definition_name** | Option<**String**> | Restrict to tasks that belong to a case definition with the given name. |  |
**case_definition_name_like** | Option<**String**> | Restrict to tasks that have a case definition name that has the parameter value as a  substring. |  |
**case_execution_id** | Option<**String**> | Restrict to tasks that belong to a case execution with the given id. |  |
**activity_instance_id_in** | Option<**String**> | Only include tasks which belong to one of the passed and comma-separated activity  instance ids. |  |
**tenant_id_in** | Option<**String**> | Only include tasks which belong to one of the passed and comma-separated  tenant ids. |  |
**without_tenant_id** | Option<**bool**> | Only include tasks which belong to no tenant. Value may only be `true`,  as `false` is the default behavior. |  |[default to false]
**assignee** | Option<**String**> | Restrict to tasks that the given user is assigned to. |  |
**assignee_expression** | Option<**String**> | Restrict to tasks that the user described by the given expression is assigned to.  See the  [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions)  for more information on available functions. |  |
**assignee_like** | Option<**String**> | Restrict to tasks that have an assignee that has the parameter  value as a substring. |  |
**assignee_like_expression** | Option<**String**> | Restrict to tasks that have an assignee that has the parameter value described by the  given expression as a substring. See the  [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions)  for more information on available functions. |  |
**assignee_in** | Option<**String**> | Only include tasks which are assigned to one of the passed and  comma-separated user ids. |  |
**owner** | Option<**String**> | Restrict to tasks that the given user owns. |  |
**owner_expression** | Option<**String**> | Restrict to tasks that the user described by the given expression owns. See the  [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions)  for more information on available functions. |  |
**candidate_group** | Option<**String**> | Only include tasks that are offered to the given group. |  |
**candidate_group_expression** | Option<**String**> | Only include tasks that are offered to the group described by the given expression.  See the  [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions)  for more information on available functions. |  |
**candidate_user** | Option<**String**> | Only include tasks that are offered to the given user or to one of his groups. |  |
**candidate_user_expression** | Option<**String**> | Only include tasks that are offered to the user described by the given expression.  See the  [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions)  for more information on available functions. |  |
**include_assigned_tasks** | Option<**bool**> | Also include tasks that are assigned to users in candidate queries. Default is to only  include tasks that are not assigned to any user if you query by candidate user or group(s). |  |[default to false]
**involved_user** | Option<**String**> | Only include tasks that the given user is involved in. A user is involved in a task if  an identity link exists between task and user (e.g., the user is the assignee). |  |
**involved_user_expression** | Option<**String**> | Only include tasks that the user described by the given expression is involved in. A user is involved in a task if an identity link exists between task and user (e.g., the user is the assignee). See the [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions) for more information on available functions. |  |
**assigned** | Option<**bool**> | If set to `true`, restricts the query to all tasks that are assigned. |  |[default to false]
**unassigned** | Option<**bool**> | If set to `true`, restricts the query to all tasks that are unassigned. |  |[default to false]
**task_definition_key** | Option<**String**> | Restrict to tasks that have the given key. |  |
**task_definition_key_in** | Option<**String**> | Restrict to tasks that have one of the given keys. The keys need to be in a comma-separated list. |  |
**task_definition_key_like** | Option<**String**> | Restrict to tasks that have a key that has the parameter value as a substring. |  |
**name** | Option<**String**> | Restrict to tasks that have the given name. |  |
**name_not_equal** | Option<**String**> | Restrict to tasks that do not have the given name. |  |
**name_like** | Option<**String**> | Restrict to tasks that have a name with the given parameter value as substring. |  |
**name_not_like** | Option<**String**> | Restrict to tasks that do not have a name with the given parameter value as substring. |  |
**description** | Option<**String**> | Restrict to tasks that have the given description. |  |
**description_like** | Option<**String**> | Restrict to tasks that have a description that has the parameter value as a substring. |  |
**priority** | Option<**i32**> | Restrict to tasks that have the given priority. |  |
**max_priority** | Option<**i32**> | Restrict to tasks that have a lower or equal priority. |  |
**min_priority** | Option<**i32**> | Restrict to tasks that have a higher or equal priority. |  |
**due_date** | Option<**String**> | Restrict to tasks that are due on the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.546+0200`. |  |
**due_date_expression** | Option<**String**> | Restrict to tasks that are due on the date described by the given expression. See the [User Guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions) for more information on available functions. The expression must evaluate to a `java.util.Date` or `org.joda.time.DateTime` object. |  |
**due_after** | Option<**String**> | Restrict to tasks that are due after the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.435+0200`. |  |
**due_after_expression** | Option<**String**> | Restrict to tasks that are due after the date described by the given expression. See the [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions) for more information on available functions. The expression must evaluate to a `java.util.Date` or `org.joda.time.DateTime` object. |  |
**due_before** | Option<**String**> | Restrict to tasks that are due before the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.243+0200`. |  |
**due_before_expression** | Option<**String**> | Restrict to tasks that are due before the date described by the given expression. See the [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions) for more information on available functions. The expression must evaluate to a `java.util.Date` or `org.joda.time.DateTime` object. |  |
**follow_up_date** | Option<**String**> | Restrict to tasks that have a followUp date on the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.342+0200`. |  |
**follow_up_date_expression** | Option<**String**> | Restrict to tasks that have a followUp date on the date described by the given expression. See the [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions) for more information on available functions. The expression must evaluate to a `java.util.Date` or `org.joda.time.DateTime` object. |  |
**follow_up_after** | Option<**String**> | Restrict to tasks that have a followUp date after the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.542+0200`. |  |
**follow_up_after_expression** | Option<**String**> | Restrict to tasks that have a followUp date after the date described by the given expression. See the [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions) for more information on available functions. The expression must evaluate to a `java.util.Date` or `org.joda.time.DateTime` object. |  |
**follow_up_before** | Option<**String**> | Restrict to tasks that have a followUp date before the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.234+0200`. |  |
**follow_up_before_expression** | Option<**String**> | Restrict to tasks that have a followUp date before the date described by the given expression. See the [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions) for more information on available functions. The expression must evaluate to a `java.util.Date` or `org.joda.time.DateTime` object. |  |
**follow_up_before_or_not_existent** | Option<**String**> | Restrict to tasks that have no followUp date or a followUp date before the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.432+0200`. The typical use case is to query all `active` tasks for a user for a given date. |  |
**follow_up_before_or_not_existent_expression** | Option<**String**> | Restrict to tasks that have no followUp date or a followUp date before the date described by the given expression. See the [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions) for more information on available functions. The expression must evaluate to a `java.util.Date` or `org.joda.time.DateTime` object. |  |
**created_on** | Option<**String**> | Restrict to tasks that were created on the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.324+0200`. |  |
**created_on_expression** | Option<**String**> | Restrict to tasks that were created on the date described by the given expression. See the [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions) for more information on available functions. The expression must evaluate to a `java.util.Date` or `org.joda.time.DateTime` object. |  |
**created_after** | Option<**String**> | Restrict to tasks that were created after the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.342+0200`. |  |
**created_after_expression** | Option<**String**> | Restrict to tasks that were created after the date described by the given expression. See the [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions) for more information on available functions. The expression must evaluate to a `java.util.Date` or `org.joda.time.DateTime` object. |  |
**created_before** | Option<**String**> | Restrict to tasks that were created before the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.332+0200`. |  |
**created_before_expression** | Option<**String**> | Restrict to tasks that were created before the date described by the given expression. See the [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions) for more information on available functions. The expression must evaluate to a `java.util.Date` or `org.joda.time.DateTime` object. |  |
**delegation_state** | Option<**String**> | Restrict to tasks that are in the given delegation state. Valid values are `PENDING` and `RESOLVED`. |  |
**candidate_groups** | Option<**String**> | Restrict to tasks that are offered to any of the given candidate groups. Takes a comma-separated list of group names, so for example `developers,support,sales`. |  |
**candidate_groups_expression** | Option<**String**> | Restrict to tasks that are offered to any of the candidate groups described by the given expression. See the [user guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/expression-language/#internal-context-functions) for more information on available functions. The expression must evaluate to `java.util.List` of Strings. |  |
**with_candidate_groups** | Option<**bool**> | Only include tasks which have a candidate group. Value may only be `true`, as `false` is the default behavior. |  |[default to false]
**without_candidate_groups** | Option<**bool**> | Only include tasks which have no candidate group. Value may only be `true`, as `false` is the default behavior. |  |[default to false]
**with_candidate_users** | Option<**bool**> | Only include tasks which have a candidate user. Value may only be `true`, as `false` is the default behavior. |  |[default to false]
**without_candidate_users** | Option<**bool**> | Only include tasks which have no candidate users. Value may only be `true`, as `false` is the default behavior. |  |[default to false]
**active** | Option<**bool**> | Only include active tasks. Value may only be `true`, as `false` is the default behavior. |  |[default to false]
**suspended** | Option<**bool**> | Only include suspended tasks. Value may only be `true`, as `false` is the default behavior. |  |[default to false]
**task_variables** | Option<**String**> | Only include tasks that have variables with certain values. Variable filtering expressions are comma-separated and are structured as follows:  A valid parameter value has the form `key_operator_value`. `key` is the variable name, `operator` is the comparison operator to be used and `value` the variable value.  **Note**: Values are always treated as String objects on server side.  Valid `operator` values are: `eq` - equal to; `neq` - not equal to; `gt` - greater than; `gteq` - greater than or equal to; `lt` - lower than; `lteq` - lower than or equal to; `like`. `key` and `value` may not contain underscore or comma characters. |  |
**process_variables** | Option<**String**> | Only include tasks that belong to process instances that have variables with certain  values. Variable filtering expressions are comma-separated and are structured as follows:  A valid parameter value has the form `key_operator_value`. `key` is the variable name, `operator` is the comparison operator to be used and `value` the variable value.  **Note**: Values are always treated as String objects on server side.  Valid `operator` values are: `eq` - equal to; `neq` - not equal to; `gt` - greater than; `gteq` - greater than or equal to; `lt` - lower than; `lteq` - lower than or equal to; `like`. `key` and `value` may not contain underscore or comma characters. |  |
**case_instance_variables** | Option<**String**> | Only include tasks that belong to case instances that have variables with certain values. Variable filtering expressions are comma-separated and are structured as follows:  A valid parameter value has the form `key_operator_value`. `key` is the variable name, `operator` is the comparison operator to be used and `value` the variable value.  **Note**: Values are always treated as String objects on server side.  Valid `operator` values are: `eq` - equal to; `neq` - not equal to; `gt` - greater than; `gteq` - greater than or equal to; `lt` - lower than; `lteq` - lower than or equal to; `like`. `key` and `value` may not contain underscore or comma characters. |  |
**variable_names_ignore_case** | Option<**bool**> | Match all variable names in this query case-insensitively. If set `variableName` and `variablename` are treated as equal. |  |[default to false]
**variable_values_ignore_case** | Option<**bool**> | Match all variable values in this query case-insensitively. If set `variableValue` and `variablevalue` are treated as equal. |  |[default to false]
**parent_task_id** | Option<**String**> | Restrict query to all tasks that are sub tasks of the given task. Takes a task id. |  |

### Return type

[**crate::models::CountResultDto**](CountResultDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## handle_bpmn_error

> handle_bpmn_error(id, task_bpmn_error_dto)


Reports a business error in the context of a running task by id. The error code must be specified to identify the BPMN error handler. See the documentation for [Reporting Bpmn Error](https://docs.camunda.org/manual/7.14/reference/bpmn20/tasks/user-task/#reporting-bpmn-error) in User Tasks.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task a BPMN error is reported for. | [required] |
**task_bpmn_error_dto** | Option<[**TaskBpmnErrorDto**](TaskBpmnErrorDto.md)> |  |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## handle_escalation

> handle_escalation(id, task_escalation_dto)


Reports an escalation in the context of a running task by id. The escalation code must be specified to identify the escalation handler. See the documentation for [Reporting Bpmn Escalation](https://docs.camunda.org/manual/7.14/reference/bpmn20/tasks/user-task/#reporting-bpmn-escalation) in User Tasks.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task in which context a BPMN escalation is reported. | [required] |
**task_escalation_dto** | Option<[**TaskEscalationDto**](TaskEscalationDto.md)> |  |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## query_tasks

> Vec<crate::models::TaskDto> query_tasks(first_result, max_results, task_query_dto)


Queries for tasks that fulfill a given filter. This method is slightly more powerful than the [Get Tasks](https://docs.camunda.org/manual/7.14/reference/rest/task/get-query/) method because it allows filtering by multiple process or task variables of types `String`, `Number` or `Boolean`. The size of the result set can be retrieved by using the [Get Task Count (POST)](https://docs.camunda.org/manual/7.14/reference/rest/task/post-query-count/) method.  **Security Consideration**: There are several parameters (such as `assigneeExpression`) for specifying an EL expression. These are disabled by default to prevent remote code execution. See the section on [security considerations for custom code](https://docs.camunda.org/manual/7.14/user-guide/process-engine/securing-custom-code/) in the user guide for details.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**first_result** | Option<**i32**> | Pagination of results. Specifies the index of the first result to return. |  |
**max_results** | Option<**i32**> | Pagination of results. Specifies the maximum number of results to return. Will return less results if there are no more results left. |  |
**task_query_dto** | Option<[**TaskQueryDto**](TaskQueryDto.md)> |  |  |

### Return type

[**Vec<crate::models::TaskDto>**](TaskDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## query_tasks_count

> crate::models::CountResultDto query_tasks_count(task_query_dto)


Retrieves the number of tasks that fulfill the given filter. Corresponds to the size of the result set of the [Get Tasks (POST)](https://docs.camunda.org/manual/7.14/reference/rest/task/post-query/) method and takes the same parameters.  **Security Consideration**: There are several parameters (such as `assigneeExpression`) for specifying an EL expression. These are disabled by default to prevent remote code execution. See the section on [security considerations for custom code](https://docs.camunda.org/manual/7.14/user-guide/process-engine/securing-custom-code/) in the user guide for details.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**task_query_dto** | Option<[**TaskQueryDto**](TaskQueryDto.md)> |  |  |

### Return type

[**crate::models::CountResultDto**](CountResultDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## resolve

> resolve(id, complete_task_dto)


Resolves a task and updates execution variables.  Resolving a task marks that the assignee is done with the task delegated to them, and that it can be sent back to the owner. Can only be executed when the task has been delegated. The assignee will be set to the owner, who performed the delegation.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task to resolve. | [required] |
**complete_task_dto** | Option<[**CompleteTaskDto**](CompleteTaskDto.md)> |  |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## set_assignee

> set_assignee(id, user_id_dto)


Changes the assignee of a task to a specific user.  **Note:** The difference with the [Claim Task](https://docs.camunda.org/manual/7.14/reference/rest/task/post-claim/) method is that this method does not check if the task already has a user assigned to it.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task to set the assignee for. | [required] |
**user_id_dto** | Option<[**UserIdDto**](UserIdDto.md)> | Provide the id of the user that will be the assignee of the task. |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## submit

> ::std::collections::HashMap<String, crate::models::VariableValueDto> submit(id, complete_task_dto)


Completes a task and updates process variables using a form submit. There are two difference between this method and the `complete` method:  * If the task is in state `PENDING` - i.e., has been delegated before, it is not completed but resolved. Otherwise it will be completed. * If the task has Form Field Metadata defined, the process engine will perform backend validation for any form fields which have validators defined. See the [Generated Task Forms](https://docs.camunda.org/manual/7.14/user-guide/task-forms/_index/#generated-task-forms) section of the [User Guide](https://docs.camunda.org/manual/7.14/user-guide/) for more information.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task to submit the form for. | [required] |
**complete_task_dto** | Option<[**CompleteTaskDto**](CompleteTaskDto.md)> |  |  |

### Return type

[**::std::collections::HashMap<String, crate::models::VariableValueDto>**](VariableValueDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## unclaim

> unclaim(id)


Resets a task's assignee. If successful, the task is not assigned to a user.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task to unclaim. | [required] |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## update_task

> update_task(id, task_dto)


Updates a task.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task to be updated. | [required] |
**task_dto** | Option<[**TaskDto**](TaskDto.md)> |  |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

