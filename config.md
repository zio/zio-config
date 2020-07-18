## Configuration Details
 
|FieldName    |Format             |Description         |Sources           |
|-------------|-------------------|--------------------|------------------|
|[root](#root)|[any-one-of](#root)|                    |                  |
|user         |primitive          |value of type string|system environment|

### root

|FieldName                       |Format                     |Description|Sources|
|--------------------------------|---------------------------|-----------|-------|
|[credentials](#root.credentials)|[all-of](#root.credentials)|           |       |
|[aws1](#root.aws1)              |[all-of](#root.aws1)       |           |       |

### root.credentials

|FieldName|Format   |Description         |Sources                                          |
|---------|---------|--------------------|-------------------------------------------------|
|username |primitive|value of type string|system environment                               |
|token_id |primitive|value of type string|docker env, system properties, system environment|

### root.aws1

|FieldName   |Format   |Description         |Sources           |
|------------|---------|--------------------|------------------|
|account_name|primitive|value of type string|system environment|
|region      |primitive|value of type string|system environment|