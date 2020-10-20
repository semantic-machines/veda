# \TaskIdentityLinkApi

All URIs are relative to *http://localhost:8080/engine-rest*

Method | HTTP request | Description
------------- | ------------- | -------------
[**add_identity_link**](TaskIdentityLinkApi.md#add_identity_link) | **post** /task/{id}/identity-links | 
[**delete_identity_link**](TaskIdentityLinkApi.md#delete_identity_link) | **post** /task/{id}/identity-links/delete | 
[**get_identity_links**](TaskIdentityLinkApi.md#get_identity_links) | **get** /task/{id}/identity-links | 



## add_identity_link

> add_identity_link(id, identity_link_dto)


Adds an identity link to a task by id. Can be used to link any user or group to a task and specify a relation.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task to add a link to. | [required] |
**identity_link_dto** | Option<[**IdentityLinkDto**](IdentityLinkDto.md)> |  |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## delete_identity_link

> delete_identity_link(id, identity_link_dto)


Removes an identity link from a task by id

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task to remove a link from. | [required] |
**identity_link_dto** | Option<[**IdentityLinkDto**](IdentityLinkDto.md)> |  |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_identity_links

> Vec<crate::models::IdentityLinkDto> get_identity_links(id, _type)


Gets the identity links for a task by id, which are the users and groups that are in *some* relation to it (including assignee and owner).

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task to retrieve the identity links for. | [required] |
**_type** | Option<**String**> | Filter by the type of links to include. |  |

### Return type

[**Vec<crate::models::IdentityLinkDto>**](IdentityLinkDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

