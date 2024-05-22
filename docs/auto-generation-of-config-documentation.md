---
id: auto-generation-of-config-documentation
title: "Auto-generation of Config Documentation"
---

ZIO Config has a built-in support for generating documentation for the configuration descriptors. This feature is useful for library authors to provide documentation for their configuration stuff. No matter how we have defined our config descriptors, manually or automatically using magnolia, we can generate documentation for them.

## Example 1: Simple Configuration

```scala mdoc:silent
import utils._

printSource("examples/shared/src/main/scala/zio/config/examples/documentation/GeneratingConfigDocumentation.scala")
```

Here is the output:

```md
auto-generated documentation of MyConfig:

## Configuration Details


|FieldName|Format                     |Description|Sources|
|---      |---                        |---        |---    |
|         |[all-of](fielddescriptions)|           |       |

### Field Descriptions

|FieldName|Format   |Description                       |Sources|
|---      |---      |---                               |---    |
|LDAP     |primitive|a text property, Related to auth  |       |
|PORT     |primitive|an integer property, Database port|       | 
|DB_URL   |primitive|a text property, URL of database  |       |
```

Currently, ZIO Config supports generating the documentation in two flavors: GitHub and Confluence markdown.

## Example 2: Nested Configuration

Here is another example, which includes nested configuration values:

```scala mdoc:silent
import utils._
printSource("examples/shared/src/main/scala/zio/config/examples/documentation/NestedConfigDocumentation.scala")
```

Let's see how the documentation looks like:

```md
Auto-generated documentation of AppConfig:

## Configuration Details


|FieldName|Format                     |Description|Sources|
|---      |---                        |---        |---    |
|         |[all-of](fielddescriptions)|           |       |

### Field Descriptions

|FieldName                 |Format               |Description                        |Sources|
|---                       |---                  |---                                |---    |
|SECRET                    |primitive            |a text property, Application secret|       |
|[CREDENTIALS](credentials)|[all-of](credentials)|Credentials                        |       |
|[DATABASE](database)      |[all-of](database)   |Database                           |       |

### CREDENTIALS

|FieldName|Format   |Description                      |Sources|
|---      |---      |---                              |---    |
|USERNAME |primitive|a text property, Example: ZioUser|       |
|PASSWORD |primitive|a text property, Example: ZioPass|       |

### DATABASE

|FieldName|Format   |Description                       |Sources|
|---      |---      |---                               |---    |
|PORT     |primitive|an integer property, Example: 8088|       |
|URL      |primitive|a text property, Example: abc.com |       |
```
