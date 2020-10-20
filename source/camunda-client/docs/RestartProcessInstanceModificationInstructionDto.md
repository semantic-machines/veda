# RestartProcessInstanceModificationInstructionDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**_type** | **String** | **Mandatory**. One of the following values: `startBeforeActivity`, `startAfterActivity`, `startTransition`.  * A `startBeforeActivity` instruction requests to enter a given activity. * A `startAfterActivity` instruction requests to execute the single outgoing sequence flow of a given activity. * A `startTransition` instruction requests to execute a specific sequence flow. | 
**activity_id** | Option<**String**> | **Can be used with instructions of types** `startBeforeActivity` and `startAfterActivity`. Specifies the sequence flow to start. | [optional]
**transition_id** | Option<**String**> | **Can be used with instructions of types** `startTransition`. Specifies the sequence flow to start. | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


