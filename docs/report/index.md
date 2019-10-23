---
id: report_index
title:  "Report"
---

# Example: Generating A Configuration Report

Calling `docs` and passing an optional configuration data instance causes zio-config
to generate a *configuration report*.

This is like a configuration manual, except it also shows the values for each documentation node.

```scala
  docs(appConfig, Some(result))
```

yields the result:

```scala
    And(
      And(
        And(
          PathDetails(Vector("south", "connection"), Some("abc.com"), List("value of type string", "South details")),
          PathDetails(Vector("south", "port"), Some("8111"), List("value of type int", "South details"))
        ),
        And(
          PathDetails(Vector("east", "connection"), Some("xyz.com"), List("value of type string", "East details")),
          PathDetails(Vector("east", "port"), Some("8888"), List("value of type int", "East details"))
        )
      ),
      PathDetails(Vector("appName"), Some("myApp"), List("value of type string"))
    )
```
