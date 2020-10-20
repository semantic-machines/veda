# \TelemetryApi

All URIs are relative to *http://localhost:8080/engine-rest*

Method | HTTP request | Description
------------- | ------------- | -------------
[**configure_telemetry**](TelemetryApi.md#configure_telemetry) | **post** /telemetry/configuration | Configure Telemetry
[**get_telemetry_configuration**](TelemetryApi.md#get_telemetry_configuration) | **get** /telemetry/configuration | Fetch Telemetry Configuration



## configure_telemetry

> configure_telemetry(telemetry_configuration_dto)
Configure Telemetry

Configures whether Camunda receives data collection of the process engine setup and usage.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**telemetry_configuration_dto** | Option<[**TelemetryConfigurationDto**](TelemetryConfigurationDto.md)> |  |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_telemetry_configuration

> crate::models::TelemetryConfigurationDto get_telemetry_configuration()
Fetch Telemetry Configuration

Fetches Telemetry Configuration.

### Parameters

This endpoint does not need any parameter.

### Return type

[**crate::models::TelemetryConfigurationDto**](TelemetryConfigurationDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

