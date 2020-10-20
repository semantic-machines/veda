# \MetricsApi

All URIs are relative to *http://localhost:8080/engine-rest*

Method | HTTP request | Description
------------- | ------------- | -------------
[**get_metrics**](MetricsApi.md#get_metrics) | **get** /metrics/{metrics-name}/sum | 
[**interval**](MetricsApi.md#interval) | **get** /metrics | 



## get_metrics

> crate::models::MetricsResultDto get_metrics(metrics_name, start_date, end_date)


Retrieves the `sum` (count) for a given metric.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**metrics_name** | **String** | The name of the metric. | [required] |
**start_date** | Option<**String**> | The start date (inclusive). |  |
**end_date** | Option<**String**> | The end date (exclusive). |  |

### Return type

[**crate::models::MetricsResultDto**](MetricsResultDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## interval

> Vec<crate::models::MetricsIntervalResultDto> interval(name, reporter, start_date, end_date, first_result, max_results, interval, aggregate_by_reporter)


Retrieves a list of metrics, aggregated for a given interval.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**name** | Option<**String**> | The name of the metric. |  |
**reporter** | Option<**String**> | The name of the reporter (host), on which the metrics was logged. This will have value provided by the [hostname configuration property](https://docs.camunda.org/manual/7.14/reference/deployment-descriptors/tags/process-engine/#hostname). |  |
**start_date** | Option<**String**> | The start date (inclusive). |  |
**end_date** | Option<**String**> | The end date (exclusive). |  |
**first_result** | Option<**i32**> | Pagination of results. Specifies the index of the first result to return. |  |
**max_results** | Option<**i32**> | Pagination of results. Specifies the maximum number of results to return. Will return less results if there are no more results left. |  |
**interval** | Option<**String**> | The interval for which the metrics should be aggregated. Time unit is seconds. Default: The interval is set to 15 minutes (900 seconds). |  |[default to 900]
**aggregate_by_reporter** | Option<**String**> | Aggregate metrics by reporter. |  |

### Return type

[**Vec<crate::models::MetricsIntervalResultDto>**](MetricsIntervalResultDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

