# ExternalTaskFailureDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**worker_id** | Option<**String**> | The id of the worker that reports the failure. Must match the id of the worker who has most recently locked the task. | [optional]
**error_message** | Option<**String**> | An message indicating the reason of the failure. | [optional]
**error_details** | Option<**String**> | A detailed error description. | [optional]
**retries** | Option<**i32**> | A number of how often the task should be retried. Must be >= 0. If this is 0, an incident is created and the task cannot be fetched anymore unless the retries are increased again. The incident's message is set to the `errorMessage` parameter. | [optional]
**retry_timeout** | Option<**i64**> | A timeout in milliseconds before the external task becomes available again for fetching. Must be >= 0. | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


