# AttachmentDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | Option<**String**> | The id of the task attachment. | [optional]
**name** | Option<**String**> | The name of the task attachment. | [optional]
**description** | Option<**String**> | The description of the task attachment. | [optional]
**task_id** | Option<**String**> | The id of the task to which the attachment belongs. | [optional]
**_type** | Option<**String**> | Indication of the type of content that this attachment refers to. Can be MIME type or any other indication. | [optional]
**url** | Option<**String**> | The url to the remote content of the task attachment. | [optional]
**create_time** | Option<**String**> | The time the variable was inserted. [Default format](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/) `yyyy-MM-dd'T'HH:mm:ss.SSSZ`. | [optional]
**removal_time** | Option<**String**> | The time after which the attachment should be removed by the History Cleanup job. [Default format](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/) `yyyy-MM-dd'T'HH:mm:ss.SSSZ`. | [optional]
**root_process_instance_id** | Option<**String**> | The process instance id of the root process instance that initiated the process containing the task. | [optional]
**links** | Option<[**Vec<crate::models::AtomLink>**](AtomLink.md)> | The links associated to this resource, with `method`, `href` and `rel`. | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


