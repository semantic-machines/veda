# DeploymentWithDefinitionsDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**deployed_process_definitions** | Option<[**::std::collections::HashMap<String, crate::models::ProcessDefinitionDto>**](ProcessDefinitionDto.md)> | A JSON Object containing a property for each of the process definitions, which are successfully deployed with that deployment. The key is the process definition id, the value is a JSON Object corresponding to the process definition. | [optional]
**deployed_decision_definitions** | Option<[**::std::collections::HashMap<String, crate::models::DecisionDefinitionDto>**](DecisionDefinitionDto.md)> | A JSON Object containing a property for each of the decision definitions, which are successfully deployed with that deployment. The key is the decision definition id, the value is a JSON Object corresponding to the decision definition. | [optional]
**deployed_decision_requirements_definitions** | Option<[**::std::collections::HashMap<String, crate::models::DecisionRequirementsDefinitionDto>**](DecisionRequirementsDefinitionDto.md)> | A JSON Object containing a property for each of the decision requirements definitions, which are successfully deployed with that deployment. The key is the decision requirements definition id, the value is a JSON Object corresponding to the decision requirements definition. | [optional]
**deployed_case_definitions** | Option<[**::std::collections::HashMap<String, crate::models::CaseDefinitionDto>**](CaseDefinitionDto.md)> | A JSON Object containing a property for each of the case definitions, which are successfully deployed with that deployment. The key is the case definition id, the value is a JSON Object corresponding to the case definition. | [optional]
**id** | Option<**String**> | The id of the deployment. | [optional]
**tenant_id** | Option<**String**> | The tenant id of the deployment. | [optional]
**deployment_time** | Option<**String**> | The time when the deployment was created. | [optional]
**source** | Option<**String**> | The source of the deployment. | [optional]
**name** | Option<**String**> | The name of the deployment. | [optional]
**links** | Option<[**Vec<crate::models::AtomLink>**](AtomLink.md)> | The links associated to this resource, with `method`, `href` and `rel`. | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


