# CommentDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | Option<**String**> | The id of the task comment. | [optional]
**user_id** | Option<**String**> | The id of the user who created the comment. | [optional]
**task_id** | Option<**String**> | The id of the task to which the comment belongs. | [optional]
**time** | Option<**String**> | The time when the comment was created. [Default format]($(docsUrl)/reference/rest/overview/date-format/) `yyyy-MM-dd'T'HH:mm:ss.SSSZ`. | [optional]
**message** | Option<**String**> | The content of the comment. | [optional]
**removal_time** | Option<**String**> | The time after which the comment should be removed by the History Cleanup job. [Default format]($(docsUrl)/reference/rest/overview/date-format/) `yyyy-MM-dd'T'HH:mm:ss.SSSZ`. | [optional]
**root_process_instance_id** | Option<**String**> | The process instance id of the root process instance that initiated the process containing the task. | [optional]
**links** | Option<[**Vec<crate::models::AtomLink>**](AtomLink.md)> | The links associated to this resource, with `method`, `href` and `rel`. | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


