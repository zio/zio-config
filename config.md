## Configuration Details
 
|FieldName                     |Format           |Description|Sources   |
|----------                    |----------       |----------|----------|
|[](#)|[all-of](#)      |          |          |

|FieldName                     |Format                   |Description         |Sources           |
|----------                    |----------               |----------          |----------        |
|[](#)|[any-one-of](#)          |                    |                  |
|user|primitive                |value of type string|system environment|

|FieldName                     |Format           |Description|Sources   |
|----------                    |----------       |----------|----------|
|[credentials](#credentials)|[all-of](#credentials)|          |          |
|[aws1](#aws1)|[all-of](#aws1)  |          |          |

### 1_credentials

|FieldName           |Format    |Description         |Sources           |
|----------          |----------|----------          |----------        |
|credentials.username|primitive |value of type string|system environment|
|credentials.token_id|primitive |value of type string|docker env, system properties, system environment|

### 1_aws1

|FieldName        |Format    |Description         |Sources           |
|----------       |----------|----------          |----------        |
|aws1.account_name|primitive |value of type string|system environment|
|aws1.region|primitive |value of type string|system environment|