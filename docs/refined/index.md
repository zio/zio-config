---
id: refined_index
title:  "Automatic Validations"
---

By bringing in `zio-config-refined` module, you get validations for your config parameters almost for free. 
`zio-config` elegantly integrates with `Refined` library for you to achieve this with same ergnomics.

Those who are not familiar with `refined` library (https://github.com/fthomas/refined) , check it out and the below examples would make sense.

```scala mdoc:silent

 import zio.config.ConfigDescriptor
 import zio.config.refined.refine

 import eu.timepit.refined._, api._, string._

 // A string config, such that it should be a Url
 val urlConfig: ConfigDescriptor[Refined[String, Url]] =
   refine[String, Url]("URL")

```

A much more interesting is being able to get a refined type out of an already derived ConfigDescriptor.
Take a look at the below example

```scala mdoc:silent

 import zio.config._, ConfigDescriptor._
 import zio.config.refined.refine
 import zio.config.magnolia.DeriveConfigDescriptor.descriptor

 import eu.timepit.refined._, api._, numeric._, collection._

 case class MyConfig(url: String, port: Int)

 val configs: ConfigDescriptor[List[MyConfig]] =
   list("databases")(descriptor[MyConfig])

 // A list of database configs, such that size should be greater than 2.
 val databaseList: ConfigDescriptor[Refined[List[MyConfig], Size[Greater[W.`2`.T]]]] =
   refine[List[MyConfig], Size[Greater[W.`2`.T]]](configs)
```


Check out sample usage of `zio-config-refined` in `examples` module of the project.