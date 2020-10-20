# FetchExternalTaskTopicDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**topic_name** | **String** | **Mandatory.** The topic's name. | 
**lock_duration** | Option<**i64**> | **Mandatory.** The duration to lock the external tasks for in milliseconds. | 
**variables** | Option<**Vec<String>**> | A JSON array of `String` values that represent variable names. For each result task belonging to this topic, the given variables are returned as well if they are accessible from the external task's execution. If not provided - all variables will be fetched. | [optional]
**local_variables** | Option<**bool**> | If `true` only local variables will be fetched. | [optional][default to false]
**business_key** | Option<**String**> | A `String` value which enables the filtering of tasks based on process instance business key. | [optional]
**process_definition_id** | Option<**String**> | Filter tasks based on process definition id. | [optional]
**process_definition_id_in** | Option<**Vec<String>**> | Filter tasks based on process definition ids. | [optional]
**process_definition_key** | Option<**String**> | Filter tasks based on process definition key. | [optional]
**process_definition_key_in** | Option<**Vec<String>**> | Filter tasks based on process definition keys. | [optional]
**process_definition_version_tag** | Option<**String**> | Filter tasks based on process definition version tag. | [optional]
**without_tenant_id** | Option<**bool**> | Filter tasks without tenant id. | [optional][default to false]
**tenant_id_in** | Option<**Vec<String>**> | Filter tasks based on tenant ids. | [optional]
**process_variables** | Option<[**::std::collections::HashMap<String, serde_json::Value>**](serde_json::Value.md)> | A `JSON` object used for filtering tasks based on process instance variable values. A property name of the object represents a process variable name, while the property value represents the process variable value to filter tasks by. | [optional]
**deserialize_values** | Option<**bool**> | Determines whether serializable variable values (typically variables that store custom Java objects) should be deserialized on server side (default `false`).  If set to `true`, a serializable variable will be deserialized on server side and transformed to JSON using [Jackson's](https://github.com/FasterXML/jackson) POJO/bean property introspection feature. Note that this requires the Java classes of the variable value to be on the REST API's classpath.  If set to `false`, a serializable variable will be returned in its serialized format. For example, a variable that is serialized as XML will be returned as a JSON string containing XML. | [optional][default to false]
**include_extension_properties** | Option<**bool**> | Determines whether custom extension properties defined in the BPMN activity of the external task (e.g. via the Extensions tab in the Camunda modeler) should be included in the response. Default: false | [optional][default to false]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


