# MultiFormDeploymentDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**tenant_id** | Option<**String**> | The tenant id for the deployment to be created. | [optional]
**deployment_source** | Option<**String**> | The source for the deployment to be created. | [optional]
**deploy_changed_only** | Option<**bool**> | A flag indicating whether the process engine should perform duplicate checking on a per-resource basis. If set to true, only those resources that have actually changed are deployed. Checks are made against resources included previous deployments of the same name and only against the latest versions of those resources. If set to true, the option enable-duplicate-filtering is overridden and set to true. | [optional][default to false]
**enable_duplicate_filtering** | Option<**bool**> | A flag indicating whether the process engine should perform duplicate checking for the deployment or not. This allows you to check if a deployment with the same name and the same resouces already exists and if true, not create a new deployment but instead return the existing deployment. The default value is false. | [optional][default to false]
**deployment_name** | Option<**String**> | The name for the deployment to be created. | [optional]
**data** | Option<[**std::path::PathBuf**](std::path::PathBuf.md)> | The binary data to create the deployment resource. It is possible to have more than one form part with different form part names for the binary data to create a deployment. | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


