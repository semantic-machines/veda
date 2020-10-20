# \DeploymentApi

All URIs are relative to *http://localhost:8080/engine-rest*

Method | HTTP request | Description
------------- | ------------- | -------------
[**create_deployment**](DeploymentApi.md#create_deployment) | **post** /deployment/create | 
[**delete_deployment**](DeploymentApi.md#delete_deployment) | **delete** /deployment/{id} | 
[**get_deployment**](DeploymentApi.md#get_deployment) | **get** /deployment/{id} | 
[**get_deployment_resource**](DeploymentApi.md#get_deployment_resource) | **get** /deployment/{id}/resources/{resourceId} | 
[**get_deployment_resource_data**](DeploymentApi.md#get_deployment_resource_data) | **get** /deployment/{id}/resources/{resourceId}/data | 
[**get_deployment_resources**](DeploymentApi.md#get_deployment_resources) | **get** /deployment/{id}/resources | 
[**get_deployments**](DeploymentApi.md#get_deployments) | **get** /deployment | 
[**get_deployments_count**](DeploymentApi.md#get_deployments_count) | **get** /deployment/count | 
[**redeploy**](DeploymentApi.md#redeploy) | **post** /deployment/{id}/redeploy | 



## create_deployment

> crate::models::DeploymentWithDefinitionsDto create_deployment(tenant_id, deployment_source, deploy_changed_only, enable_duplicate_filtering, deployment_name, data)


Creates a deployment.  **Security Consideration**  Deployments can contain custom code in form of scripts or EL expressions to customize process behavior. This may be abused for remote execution of arbitrary code.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**tenant_id** | Option<**String**> | The tenant id for the deployment to be created. |  |
**deployment_source** | Option<**String**> | The source for the deployment to be created. |  |
**deploy_changed_only** | Option<**bool**> | A flag indicating whether the process engine should perform duplicate checking on a per-resource basis. If set to true, only those resources that have actually changed are deployed. Checks are made against resources included previous deployments of the same name and only against the latest versions of those resources. If set to true, the option enable-duplicate-filtering is overridden and set to true. |  |[default to false]
**enable_duplicate_filtering** | Option<**bool**> | A flag indicating whether the process engine should perform duplicate checking for the deployment or not. This allows you to check if a deployment with the same name and the same resouces already exists and if true, not create a new deployment but instead return the existing deployment. The default value is false. |  |[default to false]
**deployment_name** | Option<**String**> | The name for the deployment to be created. |  |
**data** | Option<**std::path::PathBuf**> | The binary data to create the deployment resource. It is possible to have more than one form part with different form part names for the binary data to create a deployment. |  |

### Return type

[**crate::models::DeploymentWithDefinitionsDto**](DeploymentWithDefinitionsDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: multipart/form-data
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## delete_deployment

> delete_deployment(id, cascade, skip_custom_listeners, skip_io_mappings)


Deletes a deployment by id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the deployment to be deleted. | [required] |
**cascade** | Option<**bool**> | `true`, if all process instances, historic process instances and jobs for this deployment should be deleted. |  |[default to false]
**skip_custom_listeners** | Option<**bool**> | `true`, if only the built-in ExecutionListeners should be notified with the end event. |  |[default to false]
**skip_io_mappings** | Option<**bool**> | `true`, if all input/output mappings should not be invoked. |  |[default to false]

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_deployment

> Vec<crate::models::DeploymentDto> get_deployment(id)


Retrieves a deployment by id, according to the `Deployment` interface of the engine.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the deployment. | [required] |

### Return type

[**Vec<crate::models::DeploymentDto>**](DeploymentDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_deployment_resource

> crate::models::DeploymentResourceDto get_deployment_resource(id, resource_id)


Retrieves a deployment resource by resource id for the given deployment.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the deployment | [required] |
**resource_id** | **String** | The id of the deployment resource | [required] |

### Return type

[**crate::models::DeploymentResourceDto**](DeploymentResourceDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_deployment_resource_data

> std::path::PathBuf get_deployment_resource_data(id, resource_id)


Retrieves the binary content of a deployment resource for the given deployment by id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the deployment. | [required] |
**resource_id** | **String** | The id of the deployment resource. | [required] |

### Return type

[**std::path::PathBuf**](std::path::PathBuf.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/octet-stream, */*, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_deployment_resources

> Vec<crate::models::DeploymentResourceDto> get_deployment_resources(id)


Retrieves all deployment resources of a given deployment.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the deployment to retrieve the deployment resources for. | [required] |

### Return type

[**Vec<crate::models::DeploymentResourceDto>**](DeploymentResourceDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_deployments

> Vec<crate::models::DeploymentDto> get_deployments(id, name, name_like, source, without_source, tenant_id_in, without_tenant_id, include_deployments_without_tenant_id, after, before, sort_by, sort_order, first_result, max_results)


Queries for deployments that fulfill given parameters. Parameters may be the properties of deployments, such as the id or name or a range of the deployment time. The size of the result set can be retrieved by using the [Get Deployment count](https://docs.camunda.org/manual/7.14/reference/rest/deployment/get-query-count/) method.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | Option<**String**> | Filter by deployment id |  |
**name** | Option<**String**> | Filter by the deployment name. Exact match. |  |
**name_like** | Option<**String**> | Filter by the deployment name that the parameter is a substring of. The parameter can include the wildcard `%` to express like-strategy such as: starts with (`%`name), ends with (name`%`) or contains (`%`name`%`). |  |
**source** | Option<**String**> | Filter by the deployment source. |  |
**without_source** | Option<**bool**> | Filter by the deployment source whereby source is equal to `null`. |  |[default to false]
**tenant_id_in** | Option<**String**> | Filter by a comma-separated list of tenant ids. A deployment must have one of the given tenant ids. |  |
**without_tenant_id** | Option<**bool**> | Only include deployments which belong to no tenant. Value may only be `true`, as `false` is the default behavior. |  |[default to false]
**include_deployments_without_tenant_id** | Option<**bool**> | Include deployments which belong to no tenant. Can be used in combination with `tenantIdIn`. Value may only be `true`, as `false` is the default behavior. |  |[default to false]
**after** | Option<**String**> | Restricts to all deployments after the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**before** | Option<**String**> | Restricts to all deployments before the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**sort_by** | Option<**String**> | Sort the results lexicographically by a given criterion. Must be used in conjunction with the sortOrder parameter. |  |
**sort_order** | Option<**String**> | Sort the results in a given order. Values may be asc for ascending order or desc for descending order. Must be used in conjunction with the sortBy parameter. |  |
**first_result** | Option<**i32**> | Pagination of results. Specifies the index of the first result to return. |  |
**max_results** | Option<**i32**> | Pagination of results. Specifies the maximum number of results to return. Will return less results if there are no more results left. |  |

### Return type

[**Vec<crate::models::DeploymentDto>**](DeploymentDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_deployments_count

> crate::models::CountResultDto get_deployments_count(id, name, name_like, source, without_source, tenant_id_in, without_tenant_id, include_deployments_without_tenant_id, after, before)


Queries for the number of deployments that fulfill given parameters. Takes the same parameters as the [Get Deployments](https://docs.camunda.org/manual/7.14/reference/rest/deployment/get-query/) method.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | Option<**String**> | Filter by deployment id |  |
**name** | Option<**String**> | Filter by the deployment name. Exact match. |  |
**name_like** | Option<**String**> | Filter by the deployment name that the parameter is a substring of. The parameter can include the wildcard `%` to express like-strategy such as: starts with (`%`name), ends with (name`%`) or contains (`%`name`%`). |  |
**source** | Option<**String**> | Filter by the deployment source. |  |
**without_source** | Option<**bool**> | Filter by the deployment source whereby source is equal to `null`. |  |[default to false]
**tenant_id_in** | Option<**String**> | Filter by a comma-separated list of tenant ids. A deployment must have one of the given tenant ids. |  |
**without_tenant_id** | Option<**bool**> | Only include deployments which belong to no tenant. Value may only be `true`, as `false` is the default behavior. |  |[default to false]
**include_deployments_without_tenant_id** | Option<**bool**> | Include deployments which belong to no tenant. Can be used in combination with `tenantIdIn`. Value may only be `true`, as `false` is the default behavior. |  |[default to false]
**after** | Option<**String**> | Restricts to all deployments after the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |
**before** | Option<**String**> | Restricts to all deployments before the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. |  |

### Return type

[**crate::models::CountResultDto**](CountResultDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## redeploy

> crate::models::DeploymentWithDefinitionsDto redeploy(id, redeployment_dto)


Re-deploys an existing deployment.  The deployment resources to re-deploy can be restricted by using the properties `resourceIds` or `resourceNames`. If no deployment resources to re-deploy are passed then all existing resources of the given deployment are re-deployed.  **Warning**: Deployments can contain custom code in form of scripts or EL expressions to customize process behavior. This may be abused for remote execution of arbitrary code. See the section on [security considerations for custom code](https://docs.camunda.org/manual/7.14/user-guide/process-engine/securing-custom-code/) in the user guide for details.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the deployment to re-deploy. | [required] |
**redeployment_dto** | Option<[**RedeploymentDto**](RedeploymentDto.md)> |  |  |

### Return type

[**crate::models::DeploymentWithDefinitionsDto**](DeploymentWithDefinitionsDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

