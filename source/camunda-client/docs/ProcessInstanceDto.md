# ProcessInstanceDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | Option<**String**> | The id of the process instance. | [optional]
**definition_id** | Option<**String**> | The id of the process definition that this process instance belongs to. | [optional]
**business_key** | Option<**String**> | The business key of the process instance. | [optional]
**case_instance_id** | Option<**String**> | The id of the case instance associated with the process instance. | [optional]
**ended** | Option<**bool**> | A flag indicating whether the process instance has ended or not. Deprecated: will always be false! | [optional]
**suspended** | Option<**bool**> | A flag indicating whether the process instance is suspended or not. | [optional]
**tenant_id** | Option<**String**> | The tenant id of the process instance. | [optional]
**links** | Option<[**Vec<crate::models::AtomLink>**](AtomLink.md)> | The links associated to this resource, with `method`, `href` and `rel`. | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


