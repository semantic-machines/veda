# \ProcessDefinitionApi

All URIs are relative to *http://localhost:8080/engine-rest*

Method | HTTP request | Description
------------- | ------------- | -------------
[**delete_process_definition**](ProcessDefinitionApi.md#delete_process_definition) | **delete** /process-definition/{id} | Delete
[**delete_process_definitions_by_key**](ProcessDefinitionApi.md#delete_process_definitions_by_key) | **delete** /process-definition/key/{key} | Delete By Key
[**delete_process_definitions_by_key_and_tenant_id**](ProcessDefinitionApi.md#delete_process_definitions_by_key_and_tenant_id) | **delete** /process-definition/key/{key}/tenant-id/{tenant-id} | Delete By Key
[**get_activity_statistics**](ProcessDefinitionApi.md#get_activity_statistics) | **get** /process-definition/{id}/statistics | Get Activity Instance Statistics
[**get_activity_statistics_by_process_definition_key**](ProcessDefinitionApi.md#get_activity_statistics_by_process_definition_key) | **get** /process-definition/key/{key}/statistics | Get Activity Instance Statistics
[**get_activity_statistics_by_process_definition_key_and_tenant_id**](ProcessDefinitionApi.md#get_activity_statistics_by_process_definition_key_and_tenant_id) | **get** /process-definition/key/{key}/tenant-id/{tenant-id}/statistics | Get Activity Instance Statistics
[**get_deployed_start_form**](ProcessDefinitionApi.md#get_deployed_start_form) | **get** /process-definition/{id}/deployed-start-form | Get Deployed Start Form
[**get_deployed_start_form_by_key**](ProcessDefinitionApi.md#get_deployed_start_form_by_key) | **get** /process-definition/key/{key}/deployed-start-form | Get Deployed Start Form
[**get_deployed_start_form_by_key_and_tenant_id**](ProcessDefinitionApi.md#get_deployed_start_form_by_key_and_tenant_id) | **get** /process-definition/key/{key}/tenant-id/{tenant-id}/deployed-start-form | Get Deployed Start Form
[**get_latest_process_definition_by_tenant_id**](ProcessDefinitionApi.md#get_latest_process_definition_by_tenant_id) | **get** /process-definition/key/{key}/tenant-id/{tenant-id} | Get
[**get_process_definition**](ProcessDefinitionApi.md#get_process_definition) | **get** /process-definition/{id} | Get
[**get_process_definition_bpmn20_xml**](ProcessDefinitionApi.md#get_process_definition_bpmn20_xml) | **get** /process-definition/{id}/xml | Get XML
[**get_process_definition_bpmn20_xml_by_key**](ProcessDefinitionApi.md#get_process_definition_bpmn20_xml_by_key) | **get** /process-definition/key/{key}/xml | Get XML
[**get_process_definition_bpmn20_xml_by_key_and_tenant_id**](ProcessDefinitionApi.md#get_process_definition_bpmn20_xml_by_key_and_tenant_id) | **get** /process-definition/key/{key}/tenant-id/{tenant-id}/xml | Get XML
[**get_process_definition_by_key**](ProcessDefinitionApi.md#get_process_definition_by_key) | **get** /process-definition/key/{key} | Get
[**get_process_definition_diagram**](ProcessDefinitionApi.md#get_process_definition_diagram) | **get** /process-definition/{id}/diagram | Get Diagram
[**get_process_definition_diagram_by_key**](ProcessDefinitionApi.md#get_process_definition_diagram_by_key) | **get** /process-definition/key/{key}/diagram | Get Diagram
[**get_process_definition_diagram_by_key_and_tenant_id**](ProcessDefinitionApi.md#get_process_definition_diagram_by_key_and_tenant_id) | **get** /process-definition/key/{key}/tenant-id/{tenant-id}/diagram | Get Diagram
[**get_process_definition_statistics**](ProcessDefinitionApi.md#get_process_definition_statistics) | **get** /process-definition/statistics | Get Process Instance Statistics
[**get_process_definitions**](ProcessDefinitionApi.md#get_process_definitions) | **get** /process-definition | Get List
[**get_process_definitions_count**](ProcessDefinitionApi.md#get_process_definitions_count) | **get** /process-definition/count | Get List Count
[**get_rendered_start_form**](ProcessDefinitionApi.md#get_rendered_start_form) | **get** /process-definition/{id}/rendered-form | Get Rendered Start Form
[**get_rendered_start_form_by_key**](ProcessDefinitionApi.md#get_rendered_start_form_by_key) | **get** /process-definition/key/{key}/rendered-form | Get Rendered Start Form
[**get_rendered_start_form_by_key_and_tenant_id**](ProcessDefinitionApi.md#get_rendered_start_form_by_key_and_tenant_id) | **get** /process-definition/key/{key}/tenant-id/{tenant-id}/rendered-form | Get Rendered Start Form
[**get_start_form**](ProcessDefinitionApi.md#get_start_form) | **get** /process-definition/{id}/startForm | Get Start Form Key
[**get_start_form_by_key**](ProcessDefinitionApi.md#get_start_form_by_key) | **get** /process-definition/key/{key}/startForm | Get Start Form Key
[**get_start_form_by_key_and_tenant_id**](ProcessDefinitionApi.md#get_start_form_by_key_and_tenant_id) | **get** /process-definition/key/{key}/tenant-id/{tenant-id}/startForm | Get Start Form Key
[**get_start_form_variables**](ProcessDefinitionApi.md#get_start_form_variables) | **get** /process-definition/{id}/form-variables | Get Start Form Variables
[**get_start_form_variables_by_key**](ProcessDefinitionApi.md#get_start_form_variables_by_key) | **get** /process-definition/key/{key}/form-variables | Get Start Form Variables
[**get_start_form_variables_by_key_and_tenant_id**](ProcessDefinitionApi.md#get_start_form_variables_by_key_and_tenant_id) | **get** /process-definition/key/{key}/tenant-id/{tenant-id}/form-variables | Get Start Form Variables
[**restart_process_instance**](ProcessDefinitionApi.md#restart_process_instance) | **post** /process-definition/{id}/restart | Restart Process Instance
[**restart_process_instance_async_operation**](ProcessDefinitionApi.md#restart_process_instance_async_operation) | **post** /process-definition/{id}/restart-async | Restart Process Instance Async
[**start_process_instance**](ProcessDefinitionApi.md#start_process_instance) | **post** /process-definition/{id}/start | Start Instance
[**start_process_instance_by_key**](ProcessDefinitionApi.md#start_process_instance_by_key) | **post** /process-definition/key/{key}/start | Start Instance
[**start_process_instance_by_key_and_tenant_id**](ProcessDefinitionApi.md#start_process_instance_by_key_and_tenant_id) | **post** /process-definition/key/{key}/tenant-id/{tenant-id}/start | Start Instance
[**submit_form**](ProcessDefinitionApi.md#submit_form) | **post** /process-definition/{id}/submit-form | Submit Start Form
[**submit_form_by_key**](ProcessDefinitionApi.md#submit_form_by_key) | **post** /process-definition/key/{key}/submit-form | Submit Start Form
[**submit_form_by_key_and_tenant_id**](ProcessDefinitionApi.md#submit_form_by_key_and_tenant_id) | **post** /process-definition/key/{key}/tenant-id/{tenant-id}/submit-form | Submit Start Form
[**update_history_time_to_live_by_process_definition_id**](ProcessDefinitionApi.md#update_history_time_to_live_by_process_definition_id) | **put** /process-definition/{id}/history-time-to-live | Update History Time to Live
[**update_history_time_to_live_by_process_definition_key**](ProcessDefinitionApi.md#update_history_time_to_live_by_process_definition_key) | **put** /process-definition/key/{key}/history-time-to-live | Update History Time to Live
[**update_history_time_to_live_by_process_definition_key_and_tenant_id**](ProcessDefinitionApi.md#update_history_time_to_live_by_process_definition_key_and_tenant_id) | **put** /process-definition/key/{key}/tenant-id/{tenant-id}/history-time-to-live | Update History Time to Live
[**update_process_definition_suspension_state**](ProcessDefinitionApi.md#update_process_definition_suspension_state) | **put** /process-definition/suspended | Activate/Suspend By Key
[**update_process_definition_suspension_state_by_id**](ProcessDefinitionApi.md#update_process_definition_suspension_state_by_id) | **put** /process-definition/{id}/suspended | Activate/Suspend By Id
[**update_process_definition_suspension_state_by_key**](ProcessDefinitionApi.md#update_process_definition_suspension_state_by_key) | **put** /process-definition/key/{key}/suspended | Activate/Suspend by Id
[**update_process_definition_suspension_state_by_key_and_tenant_id**](ProcessDefinitionApi.md#update_process_definition_suspension_state_by_key_and_tenant_id) | **put** /process-definition/key/{key}/tenant-id/{tenant-id}/suspended | Activate/Suspend by Id



## delete_process_definition

> delete_process_definition(id, cascade, skip_custom_listeners, skip_io_mappings)
Delete

Deletes a running process instance by id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the process definition to be deleted. | [required] |
**cascade** | Option<**bool**> | `true`, if all process instances, historic process instances and jobs for this process definition should be deleted. |  |
**skip_custom_listeners** | Option<**bool**> | `true`, if only the built-in ExecutionListeners should be notified with the end event. |  |[default to false]
**skip_io_mappings** | Option<**bool**> | A boolean value to control whether input/output mappings should be executed during deletion. `true`, if input/output mappings should not be invoked. |  |[default to false]

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## delete_process_definitions_by_key

> delete_process_definitions_by_key(key, cascade, skip_custom_listeners, skip_io_mappings)
Delete By Key

Deletes process definitions by a given key which belong to no tenant id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**key** | **String** | The key of the process definitions to be deleted. | [required] |
**cascade** | Option<**bool**> | `true`, if all process instances, historic process instances and jobs for this process definition should be deleted. |  |
**skip_custom_listeners** | Option<**bool**> | `true`, if only the built-in ExecutionListeners should be notified with the end event. |  |[default to false]
**skip_io_mappings** | Option<**bool**> | A boolean value to control whether input/output mappings should be executed during deletion. `true`, if input/output mappings should not be invoked. |  |[default to false]

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## delete_process_definitions_by_key_and_tenant_id

> delete_process_definitions_by_key_and_tenant_id(key, tenant_id, cascade, skip_custom_listeners, skip_io_mappings)
Delete By Key

Deletes process definitions by a given key and which belong to a tenant id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**key** | **String** | The key of the process definitions to be deleted. | [required] |
**tenant_id** | **String** | The id of the tenant the process definitions belong to. | [required] |
**cascade** | Option<**bool**> | `true`, if all process instances, historic process instances and jobs for this process definition should be deleted. |  |
**skip_custom_listeners** | Option<**bool**> | `true`, if only the built-in ExecutionListeners should be notified with the end event. |  |[default to false]
**skip_io_mappings** | Option<**bool**> | A boolean value to control whether input/output mappings should be executed during deletion. `true`, if input/output mappings should not be invoked. |  |[default to false]

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_activity_statistics

> Vec<crate::models::ActivityStatisticsResultDto> get_activity_statistics(id, failed_jobs, incidents, incidents_for_type)
Get Activity Instance Statistics

Retrieves runtime statistics of a given process definition, grouped by activities. These statistics include the number of running activity instances, optionally the number of failed jobs and also optionally the number of incidents either grouped by incident types or for a specific incident type. **Note**: This does not include historic data.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the process definition. | [required] |
**failed_jobs** | Option<**bool**> | Whether to include the number of failed jobs in the result or not. Valid values are `true` or `false`. |  |
**incidents** | Option<**bool**> | Valid values for this property are `true` or `false`. If this property has been set to `true` the result will include the corresponding number of incidents for each occurred incident type. If it is set to `false`, the incidents will not be included in the result. Cannot be used in combination with `incidentsForType`. |  |
**incidents_for_type** | Option<**String**> | If this property has been set with any incident type (i.e., a string value) the result will only include the number of incidents for the assigned incident type. Cannot be used in combination with `incidents`. See the [User Guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/incidents/#incident-types) for a list of incident types. |  |

### Return type

[**Vec<crate::models::ActivityStatisticsResultDto>**](ActivityStatisticsResultDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_activity_statistics_by_process_definition_key

> Vec<crate::models::ActivityStatisticsResultDto> get_activity_statistics_by_process_definition_key(key, failed_jobs, incidents, incidents_for_type)
Get Activity Instance Statistics

Retrieves runtime statistics of the latest version of the given process definition which belongs to no tenant, grouped by activities. These statistics include the number of running activity instances, optionally the number of failed jobs and also optionally the number of incidents either grouped by incident types or for a specific incident type. **Note**: This does not include historic data.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**key** | **String** | The key of the process definition (the latest version thereof) to be retrieved. | [required] |
**failed_jobs** | Option<**bool**> | Whether to include the number of failed jobs in the result or not. Valid values are `true` or `false`. |  |
**incidents** | Option<**bool**> | Valid values for this property are `true` or `false`. If this property has been set to `true` the result will include the corresponding number of incidents for each occurred incident type. If it is set to `false`, the incidents will not be included in the result. Cannot be used in combination with `incidentsForType`. |  |
**incidents_for_type** | Option<**String**> | If this property has been set with any incident type (i.e., a string value) the result will only include the number of incidents for the assigned incident type. Cannot be used in combination with `incidents`. See the [User Guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/incidents/#incident-types) for a list of incident types. |  |

### Return type

[**Vec<crate::models::ActivityStatisticsResultDto>**](ActivityStatisticsResultDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_activity_statistics_by_process_definition_key_and_tenant_id

> Vec<crate::models::ActivityStatisticsResultDto> get_activity_statistics_by_process_definition_key_and_tenant_id(key, tenant_id, failed_jobs, incidents, incidents_for_type)
Get Activity Instance Statistics

Retrieves runtime statistics of the latest version of the given process definition for a tenant, grouped by activities. These statistics include the number of running activity instances, optionally the number of failed jobs and also optionally the number of incidents either grouped by incident types or for a specific incident type. **Note**: This does not include historic data.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**key** | **String** | The key of the process definition (the latest version thereof) to be retrieved. | [required] |
**tenant_id** | **String** | The id of the tenant the process definition belongs to. | [required] |
**failed_jobs** | Option<**bool**> | Whether to include the number of failed jobs in the result or not. Valid values are `true` or `false`. |  |
**incidents** | Option<**bool**> | Valid values for this property are `true` or `false`. If this property has been set to `true` the result will include the corresponding number of incidents for each occurred incident type. If it is set to `false`, the incidents will not be included in the result. Cannot be used in combination with `incidentsForType`. |  |
**incidents_for_type** | Option<**String**> | If this property has been set with any incident type (i.e., a string value) the result will only include the number of incidents for the assigned incident type. Cannot be used in combination with `incidents`. See the [User Guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/incidents/#incident-types) for a list of incident types. |  |

### Return type

[**Vec<crate::models::ActivityStatisticsResultDto>**](ActivityStatisticsResultDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_deployed_start_form

> std::path::PathBuf get_deployed_start_form(id)
Get Deployed Start Form

Retrieves the deployed form that can be referenced from a start event. For further information please refer to [User Guide](https://docs.camunda.org/manual/7.14/user-guide/task-forms/#embedded-task-forms).

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the process definition to get the deployed start form for. | [required] |

### Return type

[**std::path::PathBuf**](std::path::PathBuf.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/xhtml+xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_deployed_start_form_by_key

> std::path::PathBuf get_deployed_start_form_by_key(key)
Get Deployed Start Form

Retrieves the deployed form that can be referenced from a start event. For further information please refer to [User Guide](https://docs.camunda.org/manual/7.14/user-guide/task-forms/#embedded-task-forms).

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**key** | **String** | The key of the process definition (the latest version thereof) to be retrieved. | [required] |

### Return type

[**std::path::PathBuf**](std::path::PathBuf.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/xhtml+xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_deployed_start_form_by_key_and_tenant_id

> std::path::PathBuf get_deployed_start_form_by_key_and_tenant_id(key, tenant_id)
Get Deployed Start Form

Retrieves the deployed form that can be referenced from a start event. For further information please refer to [User Guide](https://docs.camunda.org/manual/7.14/user-guide/task-forms/#embedded-task-forms).

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**key** | **String** | The key of the process definition (the latest version thereof) to be retrieved. | [required] |
**tenant_id** | **String** | The id of the tenant the process definitions belong to. | [required] |

### Return type

[**std::path::PathBuf**](std::path::PathBuf.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/xhtml+xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_latest_process_definition_by_tenant_id

> crate::models::ProcessDefinitionDto get_latest_process_definition_by_tenant_id(key, tenant_id)
Get

Retrieves the latest version of the process definition for tenant according to the `ProcessDefinition` interface in the engine.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**key** | **String** | The key of the process definition (the latest version thereof) to be retrieved. | [required] |
**tenant_id** | **String** | The id of the tenant the process definition belongs to. | [required] |

### Return type

[**crate::models::ProcessDefinitionDto**](ProcessDefinitionDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_process_definition

> crate::models::ProcessDefinitionDto get_process_definition(id)
Get

Retrieves a process definition according to the `ProcessDefinition` interface in the engine.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the process definition to be retrieved. | [required] |

### Return type

[**crate::models::ProcessDefinitionDto**](ProcessDefinitionDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_process_definition_bpmn20_xml

> crate::models::ProcessDefinitionDiagramDto get_process_definition_bpmn20_xml(id)
Get XML

Retrieves the BPMN 2.0 XML of a process definition.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the process definition. | [required] |

### Return type

[**crate::models::ProcessDefinitionDiagramDto**](ProcessDefinitionDiagramDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_process_definition_bpmn20_xml_by_key

> crate::models::ProcessDefinitionDiagramDto get_process_definition_bpmn20_xml_by_key(key)
Get XML

Retrieves latest version the BPMN 2.0 XML of a process definition.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**key** | **String** | The key of the process definition (the latest version thereof) whose XML should be retrieved. | [required] |

### Return type

[**crate::models::ProcessDefinitionDiagramDto**](ProcessDefinitionDiagramDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_process_definition_bpmn20_xml_by_key_and_tenant_id

> crate::models::ProcessDefinitionDiagramDto get_process_definition_bpmn20_xml_by_key_and_tenant_id(key, tenant_id)
Get XML

Retrieves latest version the BPMN 2.0 XML of a process definition. Returns the XML for the latest version of the process definition for tenant.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**key** | **String** | The key of the process definition (the latest version thereof) whose XML should be retrieved. | [required] |
**tenant_id** | **String** | The id of the tenant the process definition belongs to. | [required] |

### Return type

[**crate::models::ProcessDefinitionDiagramDto**](ProcessDefinitionDiagramDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_process_definition_by_key

> crate::models::ProcessDefinitionDto get_process_definition_by_key(key)
Get

Retrieves the latest version of the process definition which belongs to no tenant according to the `ProcessDefinition` interface in the engine.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**key** | **String** | The key of the process definition (the latest version thereof) to be retrieved. | [required] |

### Return type

[**crate::models::ProcessDefinitionDto**](ProcessDefinitionDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_process_definition_diagram

> std::path::PathBuf get_process_definition_diagram(id)
Get Diagram

Retrieves the diagram of a process definition.  If the process definition's deployment contains an image resource with the same file name as the process definition, the deployed image will be returned by the Get Diagram endpoint. Example: `someProcess.bpmn` and `someProcess.png`. Supported file extentions for the image are: `svg`, `png`, `jpg`, and `gif`.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the process definition. | [required] |

### Return type

[**std::path::PathBuf**](std::path::PathBuf.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/octet-stream, */*, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_process_definition_diagram_by_key

> std::path::PathBuf get_process_definition_diagram_by_key(key)
Get Diagram

Retrieves the diagram for the latest version of the process definition which belongs to no tenant.  If the process definition's deployment contains an image resource with the same file name as the process definition, the deployed image will be returned by the Get Diagram endpoint. Example: `someProcess.bpmn` and `someProcess.png`. Supported file extentions for the image are: `svg`, `png`, `jpg`, and `gif`.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**key** | **String** | The key of the process definition. | [required] |

### Return type

[**std::path::PathBuf**](std::path::PathBuf.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/octet-stream, */*, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_process_definition_diagram_by_key_and_tenant_id

> std::path::PathBuf get_process_definition_diagram_by_key_and_tenant_id(key, tenant_id)
Get Diagram

Retrieves the diagram for the latest version of the process definition for tenant.  If the process definition's deployment contains an image resource with the same file name as the process definition, the deployed image will be returned by the Get Diagram endpoint. Example: `someProcess.bpmn` and `someProcess.png`. Supported file extentions for the image are: `svg`, `png`, `jpg`, and `gif`.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**key** | **String** | The key of the process definition. | [required] |
**tenant_id** | **String** | The id of the tenant the process definition belongs to. | [required] |

### Return type

[**std::path::PathBuf**](std::path::PathBuf.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/octet-stream, */*, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_process_definition_statistics

> Vec<crate::models::ProcessDefinitionStatisticsResultDto> get_process_definition_statistics(failed_jobs, incidents, incidents_for_type, root_incidents)
Get Process Instance Statistics

Retrieves runtime statistics of the process engine, grouped by process definitions. These statistics include the number of running process instances, optionally the number of failed jobs and also optionally the number of incidents either grouped by incident types or for a specific incident type. **Note**: This does not include historic data.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**failed_jobs** | Option<**bool**> | Whether to include the number of failed jobs in the result or not. Valid values are `true` or `false`. |  |
**incidents** | Option<**bool**> | Valid values for this property are `true` or `false`. If this property has been set to `true` the result will include the corresponding number of incidents for each occurred incident type. If it is set to `false`, the incidents will not be included in the result. Cannot be used in combination with `incidentsForType`. |  |
**incidents_for_type** | Option<**String**> | If this property has been set with any incident type (i.e., a string value) the result will only include the number of incidents for the assigned incident type. Cannot be used in combination with `incidents`. See the [User Guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/incidents/#incident-types) for a list of incident types. |  |
**root_incidents** | Option<**bool**> | Valid values for this property are `true` or `false`. If this property has been set to `true` the result will include the corresponding number of root incidents for each occurred incident type. If it is set to `false`, the incidents will not be included in the result. Cannot be used in combination with `incidentsForType` or `incidents`. |  |

### Return type

[**Vec<crate::models::ProcessDefinitionStatisticsResultDto>**](ProcessDefinitionStatisticsResultDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_process_definitions

> Vec<crate::models::ProcessDefinitionDto> get_process_definitions(process_definition_id, process_definition_id_in, name, name_like, deployment_id, deployed_after, deployed_at, key, keys_in, key_like, category, category_like, version, latest_version, resource_name, resource_name_like, startable_by, active, suspended, incident_id, incident_type, incident_message, incident_message_like, tenant_id_in, without_tenant_id, include_process_definitions_without_tenant_id, version_tag, version_tag_like, without_version_tag, startable_in_tasklist, not_startable_in_tasklist, startable_permission_check, sort_by, sort_order, first_result, max_results)
Get List

Queries for process definitions that fulfill given parameters. Parameters may be the properties of  process definitions, such as the name, key or version. The size of the result set can be retrieved by using the [Get Definition Count](https://docs.camunda.org/manual/7.14/reference/rest/process-definition/get-query-count/) method.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**process_definition_id** | Option<**String**> | Filter by process definition id. |  |
**process_definition_id_in** | Option<**String**> | Filter by a comma-separated list of process definition ids. |  |
**name** | Option<**String**> | Filter by process definition name. |  |
**name_like** | Option<**String**> | Filter by process definition names that the parameter is a substring of. |  |
**deployment_id** | Option<**String**> | Filter by the deployment the id belongs to. |  |
**deployed_after** | Option<**String**> | Filter by the deploy time of the deployment the process definition belongs to. Only selects process definitions that have been deployed after (exclusive) a specific time. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.546+0200`. |  |
**deployed_at** | Option<**String**> | Filter by the deploy time of the deployment the process definition belongs to. Only selects process definitions that have been deployed at a specific time (exact match). By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.546+0200`. |  |
**key** | Option<**String**> | Filter by process definition key, i.e., the id in the BPMN 2.0 XML. Exact match. |  |
**keys_in** | Option<**String**> | Filter by a comma-separated list of process definition keys. |  |
**key_like** | Option<**String**> | Filter by process definition keys that the parameter is a substring of. |  |
**category** | Option<**String**> | Filter by process definition category. Exact match. |  |
**category_like** | Option<**String**> | Filter by process definition categories that the parameter is a substring of. |  |
**version** | Option<**i32**> | Filter by process definition version. |  |
**latest_version** | Option<**bool**> | Only include those process definitions that are latest versions. Value may only be `true`, as `false` is the default behavior. |  |
**resource_name** | Option<**String**> | Filter by the name of the process definition resource. Exact match. |  |
**resource_name_like** | Option<**String**> | Filter by names of those process definition resources that the parameter is a substring of. |  |
**startable_by** | Option<**String**> | Filter by a user name who is allowed to start the process. |  |
**active** | Option<**bool**> | Only include active process definitions. Value may only be `true`, as `false` is the default behavior. |  |
**suspended** | Option<**bool**> | Only include suspended process definitions. Value may only be `true`, as `false` is the default behavior. |  |
**incident_id** | Option<**String**> | Filter by the incident id. |  |
**incident_type** | Option<**String**> | Filter by the incident type. See the [User Guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/incidents/#incident-types) for a list of incident types. |  |
**incident_message** | Option<**String**> | Filter by the incident message. Exact match. |  |
**incident_message_like** | Option<**String**> | Filter by the incident message that the parameter is a substring of. |  |
**tenant_id_in** | Option<**String**> | Filter by a comma-separated list of tenant ids. A process definition must have one of the given tenant ids. |  |
**without_tenant_id** | Option<**bool**> | Only include process definitions which belong to no tenant. Value may only be true, as false is the default behavior. |  |
**include_process_definitions_without_tenant_id** | Option<**bool**> | Include process definitions which belong to no tenant. Can be used in combination with `tenantIdIn`. Value may only be `true`, as `false` is the default behavior. |  |
**version_tag** | Option<**String**> | Filter by the version tag. |  |
**version_tag_like** | Option<**String**> | Filter by the version tag that the parameter is a substring of. |  |
**without_version_tag** | Option<**bool**> | Only include process definitions without a `versionTag`. |  |
**startable_in_tasklist** | Option<**bool**> | Filter by process definitions which are startable in Tasklist.. |  |
**not_startable_in_tasklist** | Option<**bool**> | Filter by process definitions which are not startable in Tasklist. |  |
**startable_permission_check** | Option<**bool**> | Filter by process definitions which the user is allowed to start in Tasklist. If the user doesn't have these permissions the result will be empty list. The permissions are: * `CREATE` permission for all Process instances * `CREATE_INSTANCE` and `READ` permission on Process definition level |  |
**sort_by** | Option<**String**> | Sort the results lexicographically by a given criterion. Must be used in conjunction with the sortOrder parameter. |  |
**sort_order** | Option<**String**> | Sort the results in a given order. Values may be asc for ascending order or desc for descending order. Must be used in conjunction with the sortBy parameter. |  |
**first_result** | Option<**i32**> | Pagination of results. Specifies the index of the first result to return. |  |
**max_results** | Option<**i32**> | Pagination of results. Specifies the maximum number of results to return. Will return less results if there are no more results left. |  |

### Return type

[**Vec<crate::models::ProcessDefinitionDto>**](ProcessDefinitionDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_process_definitions_count

> crate::models::CountResultDto get_process_definitions_count(process_definition_id, process_definition_id_in, name, name_like, deployment_id, deployed_after, deployed_at, key, keys_in, key_like, category, category_like, version, latest_version, resource_name, resource_name_like, startable_by, active, suspended, incident_id, incident_type, incident_message, incident_message_like, tenant_id_in, without_tenant_id, include_process_definitions_without_tenant_id, version_tag, version_tag_like, without_version_tag, startable_in_tasklist, not_startable_in_tasklist, startable_permission_check)
Get List Count

Requests the number of process definitions that fulfill the query criteria. Takes the same filtering parameters as the [Get Definitions](https://docs.camunda.org/manual/7.14/reference/rest/process-definition/get-query/) method.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**process_definition_id** | Option<**String**> | Filter by process definition id. |  |
**process_definition_id_in** | Option<**String**> | Filter by a comma-separated list of process definition ids. |  |
**name** | Option<**String**> | Filter by process definition name. |  |
**name_like** | Option<**String**> | Filter by process definition names that the parameter is a substring of. |  |
**deployment_id** | Option<**String**> | Filter by the deployment the id belongs to. |  |
**deployed_after** | Option<**String**> | Filter by the deploy time of the deployment the process definition belongs to. Only selects process definitions that have been deployed after (exclusive) a specific time. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.546+0200`. |  |
**deployed_at** | Option<**String**> | Filter by the deploy time of the deployment the process definition belongs to. Only selects process definitions that have been deployed at a specific time (exact match). By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.546+0200`. |  |
**key** | Option<**String**> | Filter by process definition key, i.e., the id in the BPMN 2.0 XML. Exact match. |  |
**keys_in** | Option<**String**> | Filter by a comma-separated list of process definition keys. |  |
**key_like** | Option<**String**> | Filter by process definition keys that the parameter is a substring of. |  |
**category** | Option<**String**> | Filter by process definition category. Exact match. |  |
**category_like** | Option<**String**> | Filter by process definition categories that the parameter is a substring of. |  |
**version** | Option<**i32**> | Filter by process definition version. |  |
**latest_version** | Option<**bool**> | Only include those process definitions that are latest versions. Value may only be `true`, as `false` is the default behavior. |  |
**resource_name** | Option<**String**> | Filter by the name of the process definition resource. Exact match. |  |
**resource_name_like** | Option<**String**> | Filter by names of those process definition resources that the parameter is a substring of. |  |
**startable_by** | Option<**String**> | Filter by a user name who is allowed to start the process. |  |
**active** | Option<**bool**> | Only include active process definitions. Value may only be `true`, as `false` is the default behavior. |  |
**suspended** | Option<**bool**> | Only include suspended process definitions. Value may only be `true`, as `false` is the default behavior. |  |
**incident_id** | Option<**String**> | Filter by the incident id. |  |
**incident_type** | Option<**String**> | Filter by the incident type. See the [User Guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/incidents/#incident-types) for a list of incident types. |  |
**incident_message** | Option<**String**> | Filter by the incident message. Exact match. |  |
**incident_message_like** | Option<**String**> | Filter by the incident message that the parameter is a substring of. |  |
**tenant_id_in** | Option<**String**> | Filter by a comma-separated list of tenant ids. A process definition must have one of the given tenant ids. |  |
**without_tenant_id** | Option<**bool**> | Only include process definitions which belong to no tenant. Value may only be true, as false is the default behavior. |  |
**include_process_definitions_without_tenant_id** | Option<**bool**> | Include process definitions which belong to no tenant. Can be used in combination with `tenantIdIn`. Value may only be `true`, as `false` is the default behavior. |  |
**version_tag** | Option<**String**> | Filter by the version tag. |  |
**version_tag_like** | Option<**String**> | Filter by the version tag that the parameter is a substring of. |  |
**without_version_tag** | Option<**bool**> | Only include process definitions without a `versionTag`. |  |
**startable_in_tasklist** | Option<**bool**> | Filter by process definitions which are startable in Tasklist.. |  |
**not_startable_in_tasklist** | Option<**bool**> | Filter by process definitions which are not startable in Tasklist. |  |
**startable_permission_check** | Option<**bool**> | Filter by process definitions which the user is allowed to start in Tasklist. If the user doesn't have these permissions the result will be empty list. The permissions are: * `CREATE` permission for all Process instances * `CREATE_INSTANCE` and `READ` permission on Process definition level |  |

### Return type

[**crate::models::CountResultDto**](CountResultDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_rendered_start_form

> std::path::PathBuf get_rendered_start_form(id)
Get Rendered Start Form

Retrieves the rendered form for a process definition. This method can be used to get the HTML rendering of a [Generated Task Form](https://docs.camunda.org/manual/7.14/user-guide/task-forms/#generated-task-forms).

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the process definition to get the rendered start form for. | [required] |

### Return type

[**std::path::PathBuf**](std::path::PathBuf.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/xhtml+xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_rendered_start_form_by_key

> std::path::PathBuf get_rendered_start_form_by_key(key)
Get Rendered Start Form

Retrieves  the rendered form for the latest version of the process definition which belongs to no tenant. This method can be used to get the HTML rendering of a [Generated Task Form](https://docs.camunda.org/manual/7.14/user-guide/task-forms/#generated-task-forms).

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**key** | **String** | The key of the process definition (the latest version thereof) to be retrieved. | [required] |

### Return type

[**std::path::PathBuf**](std::path::PathBuf.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/xhtml+xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_rendered_start_form_by_key_and_tenant_id

> std::path::PathBuf get_rendered_start_form_by_key_and_tenant_id(key, tenant_id)
Get Rendered Start Form

Retrieves  the rendered form for the latest version of the process definition for a tenant. This method can be used to get the HTML rendering of a [Generated Task Form](https://docs.camunda.org/manual/7.14/user-guide/task-forms/#generated-task-forms).

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**key** | **String** | The key of the process definition (the latest version thereof) to be retrieved. | [required] |
**tenant_id** | **String** | The id of the tenant the process definition belongs to. | [required] |

### Return type

[**std::path::PathBuf**](std::path::PathBuf.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/xhtml+xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_start_form

> crate::models::FormDto get_start_form(id)
Get Start Form Key

Retrieves the key of the start form for a process definition. The form key corresponds to the `FormData#formKey` property in the engine.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the process definition to get the start form key for. | [required] |

### Return type

[**crate::models::FormDto**](FormDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_start_form_by_key

> crate::models::FormDto get_start_form_by_key(key)
Get Start Form Key

Retrieves the key of the start form for the latest version of the process definition which belongs to no tenant. The form key corresponds to the `FormData#formKey` property in the engine.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**key** | **String** | The key of the process definition (the latest version thereof) for which the form key is to be retrieved. | [required] |

### Return type

[**crate::models::FormDto**](FormDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_start_form_by_key_and_tenant_id

> crate::models::FormDto get_start_form_by_key_and_tenant_id(key, tenant_id)
Get Start Form Key

Retrieves the key of the start form for the latest version of the process definition for a tenant. The form key corresponds to the `FormData#formKey` property in the engine.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**key** | **String** | The key of the process definition (the latest version thereof) for which the form key is to be retrieved. | [required] |
**tenant_id** | **String** | The id of the tenant the process definition belongs to. | [required] |

### Return type

[**crate::models::FormDto**](FormDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_start_form_variables

> ::std::collections::HashMap<String, crate::models::VariableValueDto> get_start_form_variables(id, variable_names, deserialize_values)
Get Start Form Variables

Retrieves the start form variables for a process definition (only if they are defined via the  [Generated Task Form](https://docs.camunda.org/manual/7.14/user-guide/task-forms/#generated-task-forms) approach). The start form variables take form data specified on the start event into account. If form fields are defined, the variable types and default values of the form fields are taken into account.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the process definition to retrieve the variables for. | [required] |
**variable_names** | Option<**String**> | A comma-separated list of variable names. Allows restricting the list of requested variables to the variable names in the list. It is best practice to restrict the list of variables to the variables actually required by the form in order to minimize fetching of data. If the query parameter is ommitted all variables are fetched. If the query parameter contains non-existent variable names, the variable names are ignored. |  |
**deserialize_values** | Option<**bool**> | Determines whether serializable variable values (typically variables that store custom Java objects) should be deserialized on server side (default true).  If set to true, a serializable variable will be deserialized on server side and transformed to JSON using [Jackson's](http://jackson.codehaus.org/) POJO/bean property introspection feature. Note that this requires the Java classes of the variable value to be on the REST API's classpath.  If set to false, a serializable variable will be returned in its serialized format. For example, a variable that is serialized as XML will be returned as a JSON string containing XML.  **Note**: While true is the default value for reasons of backward compatibility, we recommend setting this parameter to false when developing web applications that are independent of the Java process applications deployed to the engine. |  |[default to true]

### Return type

[**::std::collections::HashMap<String, crate::models::VariableValueDto>**](VariableValueDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_start_form_variables_by_key

> ::std::collections::HashMap<String, crate::models::VariableValueDto> get_start_form_variables_by_key(key, variable_names, deserialize_values)
Get Start Form Variables

Retrieves the start form variables for the latest process definition which belongs to no tenant (only if they are defined via the  [Generated Task Form](https://docs.camunda.org/manual/7.14/user-guide/task-forms/#generated-task-forms) approach). The start form variables take form data specified on the start event into account. If form fields are defined, the variable types and default values of the form fields are taken into account.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**key** | **String** | The key of the process definition (the latest version thereof) to be retrieved. | [required] |
**variable_names** | Option<**String**> | A comma-separated list of variable names. Allows restricting the list of requested variables to the variable names in the list. It is best practice to restrict the list of variables to the variables actually required by the form in order to minimize fetching of data. If the query parameter is ommitted all variables are fetched. If the query parameter contains non-existent variable names, the variable names are ignored. |  |
**deserialize_values** | Option<**bool**> | Determines whether serializable variable values (typically variables that store custom Java objects) should be deserialized on server side (default true).  If set to true, a serializable variable will be deserialized on server side and transformed to JSON using [Jackson's](http://jackson.codehaus.org/) POJO/bean property introspection feature. Note that this requires the Java classes of the variable value to be on the REST API's classpath.  If set to false, a serializable variable will be returned in its serialized format. For example, a variable that is serialized as XML will be returned as a JSON string containing XML.  **Note**: While true is the default value for reasons of backward compatibility, we recommend setting this parameter to false when developing web applications that are independent of the Java process applications deployed to the engine. |  |[default to true]

### Return type

[**::std::collections::HashMap<String, crate::models::VariableValueDto>**](VariableValueDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_start_form_variables_by_key_and_tenant_id

> ::std::collections::HashMap<String, crate::models::VariableValueDto> get_start_form_variables_by_key_and_tenant_id(key, tenant_id, variable_names, deserialize_values)
Get Start Form Variables

Retrieves the start form variables for the latest process definition for a tenant (only if they are defined via the  [Generated Task Form](https://docs.camunda.org/manual/7.14/user-guide/task-forms/#generated-task-forms) approach). The start form variables take form data specified on the start event into account. If form fields are defined, the variable types and default values of the form fields are taken into account.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**key** | **String** | The key of the process definition (the latest version thereof) to be retrieved. | [required] |
**tenant_id** | **String** | The id of the tenant the process definition belongs to. | [required] |
**variable_names** | Option<**String**> | A comma-separated list of variable names. Allows restricting the list of requested variables to the variable names in the list. It is best practice to restrict the list of variables to the variables actually required by the form in order to minimize fetching of data. If the query parameter is ommitted all variables are fetched. If the query parameter contains non-existent variable names, the variable names are ignored. |  |
**deserialize_values** | Option<**bool**> | Determines whether serializable variable values (typically variables that store custom Java objects) should be deserialized on server side (default true).  If set to true, a serializable variable will be deserialized on server side and transformed to JSON using [Jackson's](http://jackson.codehaus.org/) POJO/bean property introspection feature. Note that this requires the Java classes of the variable value to be on the REST API's classpath.  If set to false, a serializable variable will be returned in its serialized format. For example, a variable that is serialized as XML will be returned as a JSON string containing XML.  **Note**: While true is the default value for reasons of backward compatibility, we recommend setting this parameter to false when developing web applications that are independent of the Java process applications deployed to the engine. |  |[default to true]

### Return type

[**::std::collections::HashMap<String, crate::models::VariableValueDto>**](VariableValueDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## restart_process_instance

> restart_process_instance(id, restart_process_instance_dto)
Restart Process Instance

Restarts process instances that were canceled or terminated synchronously. Can also restart completed process instances. It will create a new instance using the original instance information. To execute the restart asynchronously, use the [Restart Process Instance Async](https://docs.camunda.org/manual/7.14/reference/rest/process-definition/post-restart-process-instance-async/) method.  For more information about the difference between synchronous and asynchronous execution, please refer to the related section of the [User Guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/process-instance-restart/#execution).

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the process definition of the process instances to restart. | [required] |
**restart_process_instance_dto** | Option<[**RestartProcessInstanceDto**](RestartProcessInstanceDto.md)> |  |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## restart_process_instance_async_operation

> crate::models::BatchDto restart_process_instance_async_operation(id, restart_process_instance_dto)
Restart Process Instance Async

Restarts process instances that were canceled or terminated asynchronously. Can also restart completed process instances. It will create a new instance using the original instance information. To execute the restart asynchronously, use the [Restart Process Instance](https://docs.camunda.org/manual/7.14/reference/rest/process-definition/post-restart-process-instance-sync/) method.  For more information about the difference between synchronous and asynchronous execution, please refer to the related section of the [User Guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/process-instance-restart/#execution).

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the process definition of the process instances to restart. | [required] |
**restart_process_instance_dto** | Option<[**RestartProcessInstanceDto**](RestartProcessInstanceDto.md)> |  |  |

### Return type

[**crate::models::BatchDto**](BatchDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## start_process_instance

> crate::models::ProcessInstanceWithVariablesDto start_process_instance(id, start_process_instance_dto)
Start Instance

Instantiates a given process definition. Process variables and business key may be supplied in the request body.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the process definition to be retrieved. | [required] |
**start_process_instance_dto** | Option<[**StartProcessInstanceDto**](StartProcessInstanceDto.md)> |  |  |

### Return type

[**crate::models::ProcessInstanceWithVariablesDto**](ProcessInstanceWithVariablesDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## start_process_instance_by_key

> crate::models::ProcessInstanceWithVariablesDto start_process_instance_by_key(key, start_process_instance_dto)
Start Instance

Instantiates a given process definition, starts the latest version of the process definition which belongs to no tenant. Process variables and business key may be supplied in the request body.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**key** | **String** | The key of the process definition (the latest version thereof) to be retrieved. | [required] |
**start_process_instance_dto** | Option<[**StartProcessInstanceDto**](StartProcessInstanceDto.md)> |  |  |

### Return type

[**crate::models::ProcessInstanceWithVariablesDto**](ProcessInstanceWithVariablesDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## start_process_instance_by_key_and_tenant_id

> crate::models::ProcessInstanceWithVariablesDto start_process_instance_by_key_and_tenant_id(key, tenant_id, start_process_instance_dto)
Start Instance

Instantiates a given process definition, starts the latest version of the process definition for tenant. Process variables and business key may be supplied in the request body.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**key** | **String** | The key of the process definition (the latest version thereof) to be retrieved. | [required] |
**tenant_id** | **String** | The id of the tenant the process definition belongs to. | [required] |
**start_process_instance_dto** | Option<[**StartProcessInstanceDto**](StartProcessInstanceDto.md)> |  |  |

### Return type

[**crate::models::ProcessInstanceWithVariablesDto**](ProcessInstanceWithVariablesDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## submit_form

> crate::models::ProcessInstanceDto submit_form(id, start_process_instance_form_dto)
Submit Start Form

Starts a process instance using a set of process variables and the business key. If the start event has Form Field Metadata defined, the process engine will perform backend validation for any form fields which have validators defined. See [Documentation on Generated Task Forms](https://docs.camunda.org/manual/7.14/user-guide/task-forms/#generated-task-forms).

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the process definition to submit the form for. | [required] |
**start_process_instance_form_dto** | Option<[**StartProcessInstanceFormDto**](StartProcessInstanceFormDto.md)> |  |  |

### Return type

[**crate::models::ProcessInstanceDto**](ProcessInstanceDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## submit_form_by_key

> crate::models::ProcessInstanceDto submit_form_by_key(key, start_process_instance_form_dto)
Submit Start Form

Starts the latest version of the process definition which belongs to no tenant using a set of process variables and the business key. If the start event has Form Field Metadata defined, the process engine will perform backend validation for any form fields which have validators defined. See [Documentation on Generated Task Forms](https://docs.camunda.org/manual/7.14/user-guide/task-forms/#generated-task-forms).

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**key** | **String** | The key of the process definition to submit the form for. | [required] |
**start_process_instance_form_dto** | Option<[**StartProcessInstanceFormDto**](StartProcessInstanceFormDto.md)> |  |  |

### Return type

[**crate::models::ProcessInstanceDto**](ProcessInstanceDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## submit_form_by_key_and_tenant_id

> crate::models::ProcessInstanceDto submit_form_by_key_and_tenant_id(key, tenant_id, start_process_instance_form_dto)
Submit Start Form

Starts the latest version of the process definition for a tenant using a set of process variables and the business key. If the start event has Form Field Metadata defined, the process engine will perform backend validation for any form fields which have validators defined. See [Documentation on Generated Task Forms](https://docs.camunda.org/manual/7.14/user-guide/task-forms/#generated-task-forms).

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**key** | **String** | The key of the process definition to submit the form for. | [required] |
**tenant_id** | **String** | The id of the tenant the process definition belongs to. | [required] |
**start_process_instance_form_dto** | Option<[**StartProcessInstanceFormDto**](StartProcessInstanceFormDto.md)> |  |  |

### Return type

[**crate::models::ProcessInstanceDto**](ProcessInstanceDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## update_history_time_to_live_by_process_definition_id

> update_history_time_to_live_by_process_definition_id(id, history_time_to_live_dto)
Update History Time to Live

Updates history time to live for process definition. The field is used within [History cleanup](https://docs.camunda.org/manual/7.14/user-guide/process-engine/history/#history-cleanup).

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the process definition to change history time to live. | [required] |
**history_time_to_live_dto** | Option<[**HistoryTimeToLiveDto**](HistoryTimeToLiveDto.md)> |  |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## update_history_time_to_live_by_process_definition_key

> update_history_time_to_live_by_process_definition_key(key, history_time_to_live_dto)
Update History Time to Live

Updates history time to live for the latest version of the process definition which belongs to no tenant. The field is used within [History cleanup](https://docs.camunda.org/manual/7.14/user-guide/process-engine/history/#history-cleanup).

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**key** | **String** | The key of the process definition to change history time to live. | [required] |
**history_time_to_live_dto** | Option<[**HistoryTimeToLiveDto**](HistoryTimeToLiveDto.md)> |  |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## update_history_time_to_live_by_process_definition_key_and_tenant_id

> update_history_time_to_live_by_process_definition_key_and_tenant_id(key, tenant_id, history_time_to_live_dto)
Update History Time to Live

Updates history time to live for the latest version of the process definition for a tenant. The field is used within [History cleanup](https://docs.camunda.org/manual/7.14/user-guide/process-engine/history/#history-cleanup).

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**key** | **String** | The key of the process definition to change history time to live. | [required] |
**tenant_id** | **String** | The id of the tenant the process definition belongs to. | [required] |
**history_time_to_live_dto** | Option<[**HistoryTimeToLiveDto**](HistoryTimeToLiveDto.md)> |  |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## update_process_definition_suspension_state

> update_process_definition_suspension_state(process_definition_suspension_state_dto)
Activate/Suspend By Key

Activates or suspends process definitions with the given process definition key.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**process_definition_suspension_state_dto** | Option<[**ProcessDefinitionSuspensionStateDto**](ProcessDefinitionSuspensionStateDto.md)> | **Note**: Unallowed property is `processDefinitionId`. |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## update_process_definition_suspension_state_by_id

> update_process_definition_suspension_state_by_id(id, process_definition_suspension_state_dto)
Activate/Suspend By Id

Activates or suspends a given process definition by id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the process definition to activate or suspend. | [required] |
**process_definition_suspension_state_dto** | Option<[**ProcessDefinitionSuspensionStateDto**](ProcessDefinitionSuspensionStateDto.md)> | **Note**: Unallowed properties are `processDefinitionId` and `processDefinitionKey`. |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## update_process_definition_suspension_state_by_key

> update_process_definition_suspension_state_by_key(key, process_definition_suspension_state_dto)
Activate/Suspend by Id

Activates or suspends a given process definition by latest version of process definition key which belongs to no tenant.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**key** | **String** | The key of the process definition (the latest version thereof) to be activated/suspended. | [required] |
**process_definition_suspension_state_dto** | Option<[**ProcessDefinitionSuspensionStateDto**](ProcessDefinitionSuspensionStateDto.md)> | **Note**: Unallowed properties are `processDefinitionId` and `processDefinitionKey`. |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## update_process_definition_suspension_state_by_key_and_tenant_id

> update_process_definition_suspension_state_by_key_and_tenant_id(key, tenant_id, process_definition_suspension_state_dto)
Activate/Suspend by Id

Activates or suspends a given process definition by the latest version of the process definition for tenant.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**key** | **String** | The key of the process definition (the latest version thereof) to be activated/suspended. | [required] |
**tenant_id** | **String** | The id of the tenant the process definition belongs to. | [required] |
**process_definition_suspension_state_dto** | Option<[**ProcessDefinitionSuspensionStateDto**](ProcessDefinitionSuspensionStateDto.md)> | **Note**: Unallowed properties are `processDefinitionId` and `processDefinitionKey`. |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

