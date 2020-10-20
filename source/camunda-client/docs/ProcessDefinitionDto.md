# ProcessDefinitionDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | Option<**String**> | The id of the process definition | [optional]
**key** | Option<**String**> | The key of the process definition, i.e., the id of the BPMN 2.0 XML process definition. | [optional]
**category** | Option<**String**> | The category of the process definition. | [optional]
**description** | Option<**String**> | The description of the process definition. | [optional]
**name** | Option<**String**> | The name of the process definition. | [optional]
**version** | Option<**i32**> | The version of the process definition that the engine assigned to it. | [optional]
**resource** | Option<**String**> | The file name of the process definition. | [optional]
**deployment_id** | Option<**String**> | The deployment id of the process definition. | [optional]
**diagram** | Option<**String**> | The file name of the process definition diagram, if it exists. | [optional]
**suspended** | Option<**bool**> | A flag indicating whether the definition is suspended or not. | [optional]
**tenant_id** | Option<**String**> | The tenant id of the process definition. | [optional]
**version_tag** | Option<**String**> | The version tag of the process definition. | [optional]
**history_time_to_live** | Option<**i32**> | History time to live value of the process definition. Is used within [History cleanup](https://docs.camunda.org/manual/7.14/user-guide/process-engine/history/#history-cleanup). | [optional]
**startable_in_tasklist** | Option<**bool**> | A flag indicating whether the process definition is startable in Tasklist or not. | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


