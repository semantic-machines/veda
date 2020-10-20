# BatchDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**id** | Option<**String**> | The id of the batch. | [optional]
**_type** | Option<**String**> | The type of the batch. | [optional]
**total_jobs** | Option<**i32**> | The total jobs of a batch is the number of batch execution jobs required to complete the batch. | [optional]
**jobs_created** | Option<**i32**> | The number of batch execution jobs already created by the seed job. | [optional]
**batch_jobs_per_seed** | Option<**i32**> | The number of batch execution jobs created per seed job invocation. The batch seed job is invoked until it has created all batch execution jobs required by the batch (see totalJobs property). | [optional]
**invocations_per_batch_job** | Option<**i32**> | Every batch execution job invokes the command executed by the batch invocationsPerBatchJob times. E.g., for a process instance migration batch this specifies the number of process instances which are migrated per batch execution job. | [optional]
**seed_job_definition_id** | Option<**String**> | The job definition id for the seed jobs of this batch. | [optional]
**monitor_job_definition_id** | Option<**String**> | The job definition id for the monitor jobs of this batch. | [optional]
**batch_job_definition_id** | Option<**String**> | The job definition id for the batch execution jobs of this batch. | [optional]
**suspended** | Option<**bool**> | Indicates whether this batch is suspended or not. | [optional]
**tenant_id** | Option<**String**> | The tenant id of the batch. | [optional]
**create_user_id** | Option<**String**> | The id of the user that created the batch. | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


