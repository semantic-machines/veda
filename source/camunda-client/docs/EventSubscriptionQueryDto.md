# EventSubscriptionQueryDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**event_subscription_id** | Option<**String**> | The id of the event subscription. | [optional]
**event_name** | Option<**String**> | The name of the event this subscription belongs to as defined in the process model. | [optional]
**event_type** | Option<**String**> | The type of the event subscription. | [optional]
**execution_id** | Option<**String**> | The execution that is subscribed on the referenced event. | [optional]
**process_instance_id** | Option<**String**> | The process instance this subscription belongs to. | [optional]
**activity_id** | Option<**String**> | The identifier of the activity that this event subscription belongs to. This could for example be the id of a receive task. | [optional]
**tenant_id_in** | Option<**Vec<String>**> | Filter by a comma-separated list of tenant ids. Only select subscriptions that belong to one of the given tenant ids. | [optional]
**without_tenant_id** | Option<**bool**> | Only select subscriptions which have no tenant id. Value may only be `true`, as `false` is the default behavior. | [optional]
**include_event_subscriptions_without_tenant_id** | Option<**bool**> | Select event subscriptions which have no tenant id. Can be used in combination with tenantIdIn parameter. Value may only be `true`, as `false` is the default behavior. | [optional]
**sorting** | Option<[**Vec<crate::models::EventSubscriptionQueryDtoSorting>**](EventSubscriptionQueryDto_sorting.md)> | Apply sorting of the result | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


