# HistoricActivityInstanceQueryDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**activity_instance_id** | Option<**String**> | Filter by activity instance id. | [optional]
**process_instance_id** | Option<**String**> | Filter by process instance id. | [optional]
**process_definition_id** | Option<**String**> | Filter by process definition id. | [optional]
**execution_id** | Option<**String**> | Filter by the id of the execution that executed the activity instance. | [optional]
**activity_id** | Option<**String**> | Filter by the activity id (according to BPMN 2.0 XML). | [optional]
**activity_name** | Option<**String**> | Filter by the activity name (according to BPMN 2.0 XML). | [optional]
**activity_type** | Option<**String**> | Filter by activity type. | [optional]
**task_assignee** | Option<**String**> | Only include activity instances that are user tasks and assigned to a given user. | [optional]
**finished** | Option<**bool**> | Only include finished activity instances. Value may only be `true`, as `false` behaves the same as when the property is not set. | [optional]
**unfinished** | Option<**bool**> | Only include unfinished activity instances. Value may only be `true`, as `false` behaves the same as when the property is not set. | [optional]
**canceled** | Option<**bool**> | Only include canceled activity instances. Value may only be `true`, as `false` behaves the same as when the property is not set. | [optional]
**complete_scope** | Option<**bool**> | Only include activity instances which completed a scope. Value may only be `true`, as `false` behaves the same as when the property is not set. | [optional]
**started_before** | Option<**String**> | Restrict to instances that were started before the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. | [optional]
**started_after** | Option<**String**> | Restrict to instances that were started after the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. | [optional]
**finished_before** | Option<**String**> | Restrict to instances that were finished before the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. | [optional]
**finished_after** | Option<**String**> | Restrict to instances that were finished after the given date. By [default](https://docs.camunda.org/manual/7.14/reference/rest/overview/date-format/), the date must have the format `yyyy-MM-dd'T'HH:mm:ss.SSSZ`, e.g., `2013-01-23T14:42:45.000+0200`. | [optional]
**tenant_id_in** | Option<**Vec<String>**> | Must be a JSON array of Strings. An activity instance must have one of the given tenant ids. | [optional]
**without_tenant_id** | Option<**bool**> | Only include historic activity instances that belong to no tenant. Value may only be `true`, as `false` is the default behavior. | [optional]
**sorting** | Option<[**Vec<crate::models::HistoricActivityInstanceQueryDtoSorting>**](HistoricActivityInstanceQueryDto_sorting.md)> | Apply sorting of the result | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


