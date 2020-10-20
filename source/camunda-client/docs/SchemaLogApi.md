# \SchemaLogApi

All URIs are relative to *http://localhost:8080/engine-rest*

Method | HTTP request | Description
------------- | ------------- | -------------
[**get_schema_log**](SchemaLogApi.md#get_schema_log) | **get** /schema/log | 
[**query_schema_log**](SchemaLogApi.md#query_schema_log) | **post** /schema/log | 



## get_schema_log

> Vec<crate::models::SchemaLogEntryDto> get_schema_log(version, first_result, max_results)


Queries for schema log entries that fulfill given parameters.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**version** | Option<**String**> | Only return schema log entries with a specific version. |  |
**first_result** | Option<**i32**> | Pagination of results. Specifies the index of the first result to return. |  |
**max_results** | Option<**i32**> | Pagination of results. Specifies the maximum number of results to return. Will return less results if there are no more results left. |  |

### Return type

[**Vec<crate::models::SchemaLogEntryDto>**](SchemaLogEntryDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## query_schema_log

> Vec<crate::models::SchemaLogEntryDto> query_schema_log(first_result, max_results, schema_log_query_dto)


Queries for schema log entries that fulfill given parameters.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**first_result** | Option<**i32**> | Pagination of results. Specifies the index of the first result to return. |  |
**max_results** | Option<**i32**> | Pagination of results. Specifies the maximum number of results to return. Will return less results if there are no more results left. |  |
**schema_log_query_dto** | Option<[**SchemaLogQueryDto**](SchemaLogQueryDto.md)> |  |  |

### Return type

[**Vec<crate::models::SchemaLogEntryDto>**](SchemaLogEntryDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

