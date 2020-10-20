# TriggerVariableValueDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**local** | Option<**bool**> | Indicates whether the variable should be a local variable or not. If set to true, the variable becomes a local variable of the execution entering the target activity. | [optional]
**value** | Option<[**serde_json::Value**](.md)> | The variable's value. Value differs depending on the variable's type and on the deserializeValues parameter. | [optional]
**_type** | Option<**String**> | The value type of the variable. | [optional]
**value_info** | Option<[**::std::collections::HashMap<String, serde_json::Value>**](serde_json::Value.md)> | A JSON object containing additional, value-type-dependent properties. For serialized variables of type Object, the following properties can be provided:  * `objectTypeName`: A string representation of the object's type name. * `serializationDataFormat`: The serialization format used to store the variable.  For serialized variables of type File, the following properties can be provided:  * `filename`: The name of the file. This is not the variable name but the name that will be used when downloading the file again. * `mimetype`: The MIME type of the file that is being uploaded. * `encoding`: The encoding of the file that is being uploaded. | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


