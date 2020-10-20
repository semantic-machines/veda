# \TaskAttachmentApi

All URIs are relative to *http://localhost:8080/engine-rest*

Method | HTTP request | Description
------------- | ------------- | -------------
[**add_attachment**](TaskAttachmentApi.md#add_attachment) | **post** /task/{id}/attachment/create | 
[**delete_attachment**](TaskAttachmentApi.md#delete_attachment) | **delete** /task/{id}/attachment/{attachmentId} | 
[**get_attachment**](TaskAttachmentApi.md#get_attachment) | **get** /task/{id}/attachment/{attachmentId} | 
[**get_attachment_data**](TaskAttachmentApi.md#get_attachment_data) | **get** /task/{id}/attachment/{attachmentId}/data | 
[**get_attachments**](TaskAttachmentApi.md#get_attachments) | **get** /task/{id}/attachment | 



## add_attachment

> crate::models::AttachmentDto add_attachment(id, attachment_name, attachment_description, attachment_type, url, content)


Creates an attachment for a task.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task to add the attachment to. | [required] |
**attachment_name** | Option<**String**> | The name of the attachment. |  |
**attachment_description** | Option<**String**> | The description of the attachment. |  |
**attachment_type** | Option<**String**> | The type of the attachment. |  |
**url** | Option<**String**> | The url to the remote content of the attachment. |  |
**content** | Option<**std::path::PathBuf**> | The content of the attachment. |  |

### Return type

[**crate::models::AttachmentDto**](AttachmentDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: multipart/form-data
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## delete_attachment

> delete_attachment(id, attachment_id)


Removes an attachment from a task by id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task. | [required] |
**attachment_id** | **String** | The id of the attachment to be removed. | [required] |

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_attachment

> crate::models::AttachmentDto get_attachment(id, attachment_id)


Retrieves a task attachment by task id and attachment id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task. | [required] |
**attachment_id** | **String** | The id of the attachment to be retrieved. | [required] |

### Return type

[**crate::models::AttachmentDto**](AttachmentDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_attachment_data

> std::path::PathBuf get_attachment_data(id, attachment_id)


Retrieves the binary content of a task attachment by task id and attachment id.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task. | [required] |
**attachment_id** | **String** | The id of the attachment to be retrieved. | [required] |

### Return type

[**std::path::PathBuf**](std::path::PathBuf.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/octet-stream, text/plain, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)


## get_attachments

> Vec<crate::models::AttachmentDto> get_attachments(id)


Gets the attachments for a task.

### Parameters


Name | Type | Description  | Required | Notes
------------- | ------------- | ------------- | ------------- | -------------
**id** | **String** | The id of the task to retrieve the attachments for. | [required] |

### Return type

[**Vec<crate::models::AttachmentDto>**](AttachmentDto.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

