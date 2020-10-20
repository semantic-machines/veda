# DurationReportResultDto

## Properties

Name | Type | Description | Notes
------------ | ------------- | ------------- | -------------
**period** | Option<**i32**> | Specifies a timespan within a year. **Note:** The period must be interpreted in conjunction with the returned `periodUnit`. | [optional]
**period_unit** | Option<**String**> | The unit of the given period. Possible values are `MONTH` and `QUARTER`. | [optional]
**minimum** | Option<**i64**> | The smallest duration in milliseconds of all completed process instances which were started in the given period. | [optional]
**maximum** | Option<**i64**> | The greatest duration in milliseconds of all completed process instances which were started in the given period. | [optional]
**average** | Option<**i64**> | The average duration in milliseconds of all completed process instances which were started in the given period. | [optional]

[[Back to Model list]](../README.md#documentation-for-models) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to README]](../README.md)


