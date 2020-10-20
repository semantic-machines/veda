# \EventSubscriptionApi

All URIs are relative to *http://localhost:8080/engine-rest*

Method | HTTP request | Description
------------- | ------------- | -------------
[**get_event_subscriptions**](EventSubscriptionApi.md#get_event_subscriptions) | **get** /event-subscription | 
[**get_event_subscriptions_count**](EventSubscriptionApi.md#get_event_subscriptions_count) | **get** /event-subscription/count | 



## get_event_subscriptions

> Vec<crate::models::EventSubscriptionDto> get_event_subscriptions(event_subscription_id, event_name, event_type, execution_id, process_instance_id, activity_id, tenant_id_in, without_tenant_id, include_event_subscriptions_without_tenant_id, sort_by, sort_order, first_result, max_results)


Queries for event subscriptions that fulfill given parameters. The size of the result set can be retrieved by using the [Get Event Subscriptions count](https://docs.camunda.org/manual/7.14/reference/rest/event-subscription/get-query-count/) method.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**event_subscription_id** | Option<**String**> | Only select subscription with the given id. |  |
**event_name** | Option<**String**> | Only select subscriptions for events with the given name. |  |
**event_type** | Option<**String**> | Only select subscriptions for events with the given type. Valid values: `message`, `signal`, `compensate` and `conditional`. |  |
**execution_id** | Option<**String**> | Only select subscriptions that belong to an execution with the given id. |  |
**process_instance_id** | Option<**String**> | Only select subscriptions that belong to a process instance with the given id. |  |
**activity_id** | Option<**String**> | Only select subscriptions that belong to an activity with the given id. |  |
**tenant_id_in** | Option<**String**> | Filter by a comma-separated list of tenant ids. Only select subscriptions that belong to one of the given tenant ids. |  |
**without_tenant_id** | Option<**bool**> | Only select subscriptions which have no tenant id. Value may only be `true`, as `false` is the default behavior. |  |
**include_event_subscriptions_without_tenant_id** | Option<**bool**> | Select event subscriptions which have no tenant id. Can be used in combination with tenantIdIn parameter. Value may only be `true`, as `false` is the default behavior. |  |
**sort_by** | Option<**String**> | Sort the results lexicographically by a given criterion. Must be used in conjunction with the sortOrder parameter. |  |
**sort_order** | Option<**String**> | Sort the results in a given order. Values may be asc for ascending order or desc for descending order. Must be used in conjunction with the sortBy parameter. |  |
**first_result** | Option<**i32**> | Pagination of results. Specifies the index of the first result to return. |  |
**max_results** | Option<**i32**> | Pagination of results. Specifies the maximum number of results to return. Will return less results if there are no more results left. |  |

### Return type

[**Vec<crate::models::EventSubscriptionDto>**](EventSubscriptionDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_event_subscriptions_count

> crate::models::CountResultDto get_event_subscriptions_count(event_subscription_id, event_name, event_type, execution_id, process_instance_id, activity_id, tenant_id_in, without_tenant_id, include_event_subscriptions_without_tenant_id)


Queries for the number of event subscriptions that fulfill given parameters. Takes the same parameters as the [Get Event Subscriptions](https://docs.camunda.org/manual/7.14/reference/rest/event-subscription/get-query/) method.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**event_subscription_id** | Option<**String**> | Only select subscription with the given id. |  |
**event_name** | Option<**String**> | Only select subscriptions for events with the given name. |  |
**event_type** | Option<**String**> | Only select subscriptions for events with the given type. Valid values: `message`, `signal`, `compensate` and `conditional`. |  |
**execution_id** | Option<**String**> | Only select subscriptions that belong to an execution with the given id. |  |
**process_instance_id** | Option<**String**> | Only select subscriptions that belong to a process instance with the given id. |  |
**activity_id** | Option<**String**> | Only select subscriptions that belong to an activity with the given id. |  |
**tenant_id_in** | Option<**String**> | Filter by a comma-separated list of tenant ids. Only select subscriptions that belong to one of the given tenant ids. |  |
**without_tenant_id** | Option<**bool**> | Only select subscriptions which have no tenant id. Value may only be `true`, as `false` is the default behavior. |  |
**include_event_subscriptions_without_tenant_id** | Option<**bool**> | Select event subscriptions which have no tenant id. Can be used in combination with tenantIdIn parameter. Value may only be `true`, as `false` is the default behavior. |  |

### Return type

[**crate::models::CountResultDto**](CountResultDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

