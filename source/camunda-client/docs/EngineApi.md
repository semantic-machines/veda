# \EngineApi

All URIs are relative to *http://localhost:8080/engine-rest*

Method | HTTP request | Description
------------- | ------------- | -------------
[**get_process_engine_names**](EngineApi.md#get_process_engine_names) | **get** /engine | 



## get_process_engine_names

> Vec<crate::models::ProcessEngineDto> get_process_engine_names()


Retrieves the names of all process engines available on your platform. **Note**: You cannot prepend `/engine/{name}` to this method.

### Parameters

This endpoint does not need any parameter.

### Return type

[**Vec<crate::models::ProcessEngineDto>**](ProcessEngineDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

