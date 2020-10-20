# \TaskLocalVariableApi

All URIs are relative to *http://localhost:8080/engine-rest*

Method | HTTP request | Description
------------- | ------------- | -------------
[**delete_task_local_variable**](TaskLocalVariableApi.md#delete_task_local_variable) | **delete** /task/{id}/localVariables/{varName} | 
[**get_task_local_variable**](TaskLocalVariableApi.md#get_task_local_variable) | **get** /task/{id}/localVariables/{varName} | 
[**get_task_local_variable_binary**](TaskLocalVariableApi.md#get_task_local_variable_binary) | **get** /task/{id}/localVariables/{varName}/data | 
[**get_task_local_variables**](TaskLocalVariableApi.md#get_task_local_variables) | **get** /task/{id}/localVariables | 
[**modify_task_local_variables**](TaskLocalVariableApi.md#modify_task_local_variables) | **post** /task/{id}/localVariables | 
[**put_task_local_variable**](TaskLocalVariableApi.md#put_task_local_variable) | **put** /task/{id}/localVariables/{varName} | 
[**set_binary_task_local_variable**](TaskLocalVariableApi.md#set_binary_task_local_variable) | **post** /task/{id}/localVariables/{varName}/data | 



## delete_task_local_variable

> delete_task_local_variable(id, var_name)


Removes a local variable from a task by id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task to delete the variable from. | [required] |
**var_name** | **String** | The name of the variable to be removed. | [required] |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_task_local_variable

> crate::models::VariableValueDto get_task_local_variable(id, var_name, deserialize_value)


Retrieves a variable from the context of a given task by id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task to retrieve the variable from. | [required] |
**var_name** | **String** | The name of the variable to get | [required] |
**deserialize_value** | Option<**bool**> | Determines whether serializable variable values (typically variables that store custom Java objects) should be deserialized on the server side (default `true`).  If set to `true`, a serializable variable will be deserialized on server side and transformed to JSON using [Jackson's](https://github.com/FasterXML/jackson) POJO/bean property introspection feature. Note that this requires the Java classes of the variable value to be on the REST API's classpath.  If set to `false`, a serializable variable will be returned in its serialized format. For example, a variable that is serialized as XML will be returned as a JSON string containing XML.  Note: While `true` is the default value for reasons of backward compatibility, we recommend setting this parameter to `false` when developing web applications that are independent of the Java process applications deployed to the engine. |  |[default to true]

### Return type

[**crate::models::VariableValueDto**](VariableValueDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_task_local_variable_binary

> std::path::PathBuf get_task_local_variable_binary(id, var_name)


Retrieves a binary variable from the context of a given task by id. Applicable for byte array and file variables.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task to retrieve the variable for. | [required] |
**var_name** | **String** | The name of the variable to retrieve. | [required] |

### Return type

[**std::path::PathBuf**](std::path::PathBuf.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/octet-stream, text/plain, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_task_local_variables

> ::std::collections::HashMap<String, crate::models::VariableValueDto> get_task_local_variables(id, deserialize_values)


Retrieves all variables of a given task by id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task to retrieve the variables from. | [required] |
**deserialize_values** | Option<**bool**> | Determines whether serializable variable values (typically variables that store custom Java objects) should be deserialized on the server side (default `true`).  If set to `true`, a serializable variable will be deserialized on server side and transformed to JSON using [Jackson's](https://github.com/FasterXML/jackson) POJO/bean property introspection feature. Note that this requires the Java classes of the variable value to be on the REST API's classpath.  If set to `false`, a serializable variable will be returned in its serialized format. For example, a variable that is serialized as XML will be returned as a JSON string containing XML.  **Note:** While `true` is the default value for reasons of backward compatibility, we recommend setting this parameter to `false` when developing web applications that are independent of the Java process applications deployed to the engine. |  |[default to true]

### Return type

[**::std::collections::HashMap<String, crate::models::VariableValueDto>**](VariableValueDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## modify_task_local_variables

> modify_task_local_variables(id, patch_variables_dto)


Updates or deletes the variables in the context of a task. Updates precede deletions. So, if a variable is updated AND deleted, the deletion overrides the update.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task to set variables for. | [required] |
**patch_variables_dto** | Option<[**PatchVariablesDto**](PatchVariablesDto.md)> |  |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## put_task_local_variable

> put_task_local_variable(id, var_name, variable_value_dto)


Sets a variable in the context of a given task.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task to set the variable for. | [required] |
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


## set_binary_task_local_variable

> set_binary_task_local_variable(id, var_name, data, value_type)


Sets the serialized value for a binary variable or the binary value for a file variable.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task to retrieve the variable for. | [required] |
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

