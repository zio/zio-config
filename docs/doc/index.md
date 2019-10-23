---
id: doc_index
title:  "Documentation"
---

# Example: Generating a Configuration Manual

To generate the documentation for a `ConfigDescriptor`, call `docs`. 

```scala
  docs(appConfig, None)
```

yields the result:

```scala
  And(
    And(
      And(
        PathDetails(Vector("south", "connection"), None, List("value of type string", "South details", "asdf")),
        PathDetails(Vector("south", "port"), None, List("value of type int", "South details", "asdf"))
      ),
      And(
        PathDetails(Vector("east", "connection"), None, List("value of type string", "East details", "asdf")),
        PathDetails(Vector("east", "port"), None, List("value of type int", "East details", "asdf"))
      )
    ),
    PathDetails(Vector("appName"), None, List("value of type string", "asdf"))
  )
```
