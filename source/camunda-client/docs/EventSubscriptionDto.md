# EventSubscriptionDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | Option<**String**> | The id of the event subscription. | [optional]
**event_type** | Option<**String**> | The type of the event subscription. | [optional]
**event_name** | Option<**String**> | The name of the event this subscription belongs to as defined in the process model. | [optional]
**execution_id** | Option<**String**> | The execution that is subscribed on the referenced event. | [optional]
**process_instance_id** | Option<**String**> | The process instance this subscription belongs to. | [optional]
**activity_id** | Option<**String**> | The identifier of the activity that this event subscription belongs to. This could for example be the id of a receive task. | [optional]
**created_date** | Option<**String**> | The time this event subscription was created. | [optional]
**tenant_id** | Option<**String**> | The id of the tenant this event subscription belongs to. Can be `null` if the subscription belongs to no single tenant. | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


