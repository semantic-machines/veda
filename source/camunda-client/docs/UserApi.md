# \UserApi

All URIs are relative to *http://localhost:8080/engine-rest*

Method | HTTP request | Description
------------- | ------------- | -------------
[**available_operations**](UserApi.md#available_operations) | **options** /user | Options
[**available_user_operations**](UserApi.md#available_user_operations) | **options** /user/{id} | Options
[**create_user**](UserApi.md#create_user) | **post** /user/create | Create
[**delete_user**](UserApi.md#delete_user) | **delete** /user/{id} | Delete
[**get_user_count**](UserApi.md#get_user_count) | **get** /user/count | Get List Count
[**get_user_profile**](UserApi.md#get_user_profile) | **get** /user/{id}/profile | Get Profile
[**get_users**](UserApi.md#get_users) | **get** /user | Get List
[**unlock_user**](UserApi.md#unlock_user) | **post** /user/{id}/unlock | Unlock User
[**update_credentials**](UserApi.md#update_credentials) | **put** /user/{id}/credentials | Update Credentials



## available_operations

> crate::models::ResourceOptionsDto available_operations()
Options

The `/user` resource supports two custom `OPTIONS` requests, one for the resource as such and one for individual user instances. The `OPTIONS` request allows checking for the set of available operations that the currently authenticated user can perform on the /user resource. If the user can perform an operation or not may depend on various things, including the user's authorizations to interact with this resource and the internal configuration of the process engine.

### Parameters

This endpoint does not need any parameter.

### Return type

[**crate::models::ResourceOptionsDto**](ResourceOptionsDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## available_user_operations

> crate::models::ResourceOptionsDto available_user_operations(id)
Options

The `/user` resource supports two custom `OPTIONS` requests, one for the resource as such and one for individual user instances. The `OPTIONS` request allows checking for the set of available operations that the currently authenticated user can perform on the /user resource. If the user can perform an operation or not may depend on various things, including the user's authorizations to interact with this resource and the internal configuration of the process engine.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the user to be deleted. | [required] |

### Return type

[**crate::models::ResourceOptionsDto**](ResourceOptionsDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## create_user

> create_user(user_dto)
Create

Create a new user.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**user_dto** | Option<[**UserDto**](UserDto.md)> |  |  |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## delete_user

> delete_user(id)
Delete

Deletes a user by id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the user to be deleted. | [required] |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_user_count

> crate::models::CountResultDto get_user_count(id, id_in, first_name, first_name_like, last_name, last_name_like, email, email_like, member_of_group, member_of_tenant, potential_starter)
Get List Count

Queries for the number of deployments that fulfill given parameters. Takes the same parameters as the [Get Users](https://docs.camunda.org/manual/7.14/reference/rest/user/get-query/) method.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | Option<**String**> | Filter by user id |  |
**id_in** | Option<**String**> | Filter by a comma-separated list of user ids. |  |
**first_name** | Option<**String**> | Filter by the first name of the user. Exact match. |  |
**first_name_like** | Option<**String**> | Filter by the first name that the parameter is a substring of. |  |
**last_name** | Option<**String**> | Filter by the last name of the user. Exact match. |  |
**last_name_like** | Option<**String**> | Filter by the last name that the parameter is a substring of. |  |
**email** | Option<**String**> | Filter by the email of the user. Exact match. |  |
**email_like** | Option<**String**> | Filter by the email that the parameter is a substring of. |  |
**member_of_group** | Option<**String**> | Filter for users which are members of the given group. |  |
**member_of_tenant** | Option<**String**> | Filter for users which are members of the given tenant. |  |
**potential_starter** | Option<**String**> | Only select Users that are potential starter for the given process definition. |  |

### Return type

[**crate::models::CountResultDto**](CountResultDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_user_profile

> Vec<crate::models::UserProfileDto> get_user_profile(id)
Get Profile

Retrieves a user's profile.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the user to retrieve. | [required] |

### Return type

[**Vec<crate::models::UserProfileDto>**](UserProfileDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_users

> Vec<crate::models::UserProfileDto> get_users(id, id_in, first_name, first_name_like, last_name, last_name_like, email, email_like, member_of_group, member_of_tenant, potential_starter, sort_by, sort_order, first_result, max_results)
Get List

Query for a list of users using a list of parameters. The size of the result set can be retrieved by using the Get User Count method. [Get User Count](https://docs.camunda.org/manual/7.14/reference/rest/user/get-query-count/) method.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | Option<**String**> | Filter by user id |  |
**id_in** | Option<**String**> | Filter by a comma-separated list of user ids. |  |
**first_name** | Option<**String**> | Filter by the first name of the user. Exact match. |  |
**first_name_like** | Option<**String**> | Filter by the first name that the parameter is a substring of. |  |
**last_name** | Option<**String**> | Filter by the last name of the user. Exact match. |  |
**last_name_like** | Option<**String**> | Filter by the last name that the parameter is a substring of. |  |
**email** | Option<**String**> | Filter by the email of the user. Exact match. |  |
**email_like** | Option<**String**> | Filter by the email that the parameter is a substring of. |  |
**member_of_group** | Option<**String**> | Filter for users which are members of the given group. |  |
**member_of_tenant** | Option<**String**> | Filter for users which are members of the given tenant. |  |
**potential_starter** | Option<**String**> | Only select Users that are potential starter for the given process definition. |  |
**sort_by** | Option<**String**> | Sort the results lexicographically by a given criterion. Must be used in conjunction with the sortOrder parameter. |  |
**sort_order** | Option<**String**> | Sort the results in a given order. Values may be asc for ascending order or desc for descending order. Must be used in conjunction with the sortBy parameter. |  |
**first_result** | Option<**i32**> | Pagination of results. Specifies the index of the first result to return. |  |
**max_results** | Option<**i32**> | Pagination of results. Specifies the maximum number of results to return. Will return less results if there are no more results left. |  |

### Return type

[**Vec<crate::models::UserProfileDto>**](UserProfileDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## unlock_user

> unlock_user(id)
Unlock User

Unlocks a user by id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the user to be unlocked. | [required] |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## update_credentials

> crate::models::AnyType update_credentials(id, password, authenticated_user_password, user_credentials_dto)
Update Credentials

Updates a user's credentials (password)

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the user to be updated. | [required] |
**password** | **String** | The users new password. | [required] |
**authenticated_user_password** | **String** | The password of the authenticated user who changes the password of the user (i.e., the user with passed id as path parameter). | [required] |
**user_credentials_dto** | Option<[**UserCredentialsDto**](UserCredentialsDto.md)> |  |  |

### Return type

[**crate::models::AnyType**](AnyType.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

