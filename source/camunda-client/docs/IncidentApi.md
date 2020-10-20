# \IncidentApi

All URIs are relative to *http://localhost:8080/engine-rest*

Method | HTTP request | Description
------------- | ------------- | -------------
[**get_incident**](IncidentApi.md#get_incident) | **get** /incident/{id} | Get Incident
[**get_incidents**](IncidentApi.md#get_incidents) | **get** /incident | Get List
[**get_incidents_count**](IncidentApi.md#get_incidents_count) | **get** /incident/count | Get List Count
[**resolve_incident**](IncidentApi.md#resolve_incident) | **delete** /incident/{id} | Resolve Incident



## get_incident

> crate::models::IncidentDto get_incident(id)
Get Incident

Retrieves an incident by ID.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the incident to be retrieved. | [required] |

### Return type

[**crate::models::IncidentDto**](IncidentDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_incidents

> Vec<crate::models::IncidentDto> get_incidents(incident_id, incident_type, incident_message, incident_message_like, process_definition_id, process_definition_key_in, process_instance_id, execution_id, incident_timestamp_before, incident_timestamp_after, activity_id, failed_activity_id, cause_incident_id, root_cause_incident_id, configuration, tenant_id_in, job_definition_id_in, sort_by, sort_order)
Get List

Queries for incidents that fulfill given parameters. The size of the result set can be retrieved by using the [Get Incident Count](https://docs.camunda.org/manual/7.14/reference/rest/incident/get-query-count/) method.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**incident_id** | Option<**String**> | Restricts to incidents that have the given id. |  |
**incident_type** | Option<**String**> | Restricts to incidents that belong to the given incident type. See the [User Guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/incidents/#incident-types) for a list of incident types. |  |
**incident_message** | Option<**String**> | Restricts to incidents that have the given incident message. |  |
**incident_message_like** | Option<**String**> | Restricts to incidents that incidents message is a substring of the given value. The string can include the wildcard character '%' to express like-strategy: starts with (`string%`), ends with (`%string`) or contains (`%string%`). |  |
**process_definition_id** | Option<**String**> | Restricts to incidents that belong to a process definition with the given id. |  |
**process_definition_key_in** | Option<**String**> | Restricts to incidents that belong to a process definition with the given keys. Must be a comma-separated list. |  |
**process_instance_id** | Option<**String**> | Restricts to incidents that belong to a process instance with the given id. |  |
**execution_id** | Option<**String**> | Restricts to incidents that belong to an execution with the given id. |  |
**incident_timestamp_before** | Option<**String**> | Restricts to incidents that have an incidentTimestamp date before the given date. By default, the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**incident_timestamp_after** | Option<**String**> | Restricts to incidents that have an incidentTimestamp date after the given date. By default*, the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**activity_id** | Option<**String**> | Restricts to incidents that belong to an activity with the given id. |  |
**failed_activity_id** | Option<**String**> | Restricts to incidents that were created due to the failure of an activity with the given id. |  |
**cause_incident_id** | Option<**String**> | Restricts to incidents that have the given incident id as cause incident. |  |
**root_cause_incident_id** | Option<**String**> | Restricts to incidents that have the given incident id as root cause incident. |  |
**configuration** | Option<**String**> | Restricts to incidents that have the given parameter set as configuration. |  |
**tenant_id_in** | Option<**String**> | Restricts to incidents that have one of the given comma-separated tenant ids. |  |
**job_definition_id_in** | Option<**String**> | Restricts to incidents that have one of the given comma-separated job definition ids. |  |
**sort_by** | Option<**String**> | Sort the results lexicographically by a given criterion. Must be used in conjunction with the sortOrder parameter. |  |
**sort_order** | Option<**String**> | Sort the results in a given order. Values may be asc for ascending order or desc for descending order. Must be used in conjunction with the sortBy parameter. |  |

### Return type

[**Vec<crate::models::IncidentDto>**](IncidentDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_incidents_count

> Vec<crate::models::CountResultDto> get_incidents_count(incident_id, incident_type, incident_message, incident_message_like, process_definition_id, process_definition_key_in, process_instance_id, execution_id, incident_timestamp_before, incident_timestamp_after, activity_id, failed_activity_id, cause_incident_id, root_cause_incident_id, configuration, tenant_id_in, job_definition_id_in)
Get List Count

Queries for the number of incidents that fulfill given parameters. Takes the same parameters as the [Get Incidents](https://docs.camunda.org/manual/7.14/reference/rest/incident/get-query/) method.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**incident_id** | Option<**String**> | Restricts to incidents that have the given id. |  |
**incident_type** | Option<**String**> | Restricts to incidents that belong to the given incident type. See the [User Guide](https://docs.camunda.org/manual/7.14/user-guide/process-engine/incidents/#incident-types) for a list of incident types. |  |
**incident_message** | Option<**String**> | Restricts to incidents that have the given incident message. |  |
**incident_message_like** | Option<**String**> | Restricts to incidents that incidents message is a substring of the given value. The string can include the wildcard character '%' to express like-strategy: starts with (`string%`), ends with (`%string`) or contains (`%string%`). |  |
**process_definition_id** | Option<**String**> | Restricts to incidents that belong to a process definition with the given id. |  |
**process_definition_key_in** | Option<**String**> | Restricts to incidents that belong to a process definition with the given keys. Must be a comma-separated list. |  |
**process_instance_id** | Option<**String**> | Restricts to incidents that belong to a process instance with the given id. |  |
**execution_id** | Option<**String**> | Restricts to incidents that belong to an execution with the given id. |  |
**incident_timestamp_before** | Option<**String**> | Restricts to incidents that have an incidentTimestamp date before the given date. By default, the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**incident_timestamp_after** | Option<**String**> | Restricts to incidents that have an incidentTimestamp date after the given date. By default*, the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**activity_id** | Option<**String**> | Restricts to incidents that belong to an activity with the given id. |  |
**failed_activity_id** | Option<**String**> | Restricts to incidents that were created due to the failure of an activity with the given id. |  |
**cause_incident_id** | Option<**String**> | Restricts to incidents that have the given incident id as cause incident. |  |
**root_cause_incident_id** | Option<**String**> | Restricts to incidents that have the given incident id as root cause incident. |  |
**configuration** | Option<**String**> | Restricts to incidents that have the given parameter set as configuration. |  |
**tenant_id_in** | Option<**String**> | Restricts to incidents that have one of the given comma-separated tenant ids. |  |
**job_definition_id_in** | Option<**String**> | Restricts to incidents that have one of the given comma-separated job definition ids. |  |

### Return type

[**Vec<crate::models::CountResultDto>**](CountResultDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## resolve_incident

> resolve_incident(id)
Resolve Incident

Resolves an incident with given id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the incident to be resolved. | [required] |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

