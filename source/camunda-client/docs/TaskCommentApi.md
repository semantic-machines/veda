# \TaskCommentApi

All URIs are relative to *http://localhost:8080/engine-rest*

Method | HTTP request | Description
------------- | ------------- | -------------
[**create_comment**](TaskCommentApi.md#create_comment) | **post** /task/{id}/comment/create | 
[**get_comment**](TaskCommentApi.md#get_comment) | **get** /task/{id}/comment/{commentId} | 
[**get_comments**](TaskCommentApi.md#get_comments) | **get** /task/{id}/comment | 



## create_comment

> crate::models::CommentDto create_comment(id, comment_dto)


Creates a comment for a task by id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task to add the comment to. | [required] |
**comment_dto** | Option<[**CommentDto**](CommentDto.md)> | **Note:** Only the `message` property will be used. Every other property passed to this endpoint will be ignored. |  |

### Return type

[**crate::models::CommentDto**](CommentDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_comment

> crate::models::CommentDto get_comment(id, comment_id)


Retrieves a task comment by task id and comment id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task. | [required] |
**comment_id** | **String** | The id of the comment to be retrieved. | [required] |

### Return type

[**crate::models::CommentDto**](CommentDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_comments

> Vec<crate::models::CommentDto> get_comments(id)


Gets the comments for a task by id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task to retrieve the comments for. | [required] |

### Return type

[**Vec<crate::models::CommentDto>**](CommentDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

