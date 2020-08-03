package zio.config.examples

import zio.config._
import zio.system.System
import ConfigDescriptor._

object MultipleSourcesExample extends App {

  final case class MyConfig(ldap: String, port: Int, dburl: Option[String])

  // Only used to fetch the source - the pattern is only for explanation purpose
  val runtime = zio.Runtime.default

  // Assume they are different sources (env, property file, HOCON / database (in future))
  private val source1 = ConfigSource.fromMap(Map("LDAP" -> "jolap"), "constant")
  private val source2 = runtime.unsafeRun(ConfigSource.fromSystemProperties)

  private val source3 = runtime.unsafeRun(ConfigSource.fromSystemEnv.provideLayer(System.live))
  private val source4 = ConfigSource.fromMap(Map("PORT" -> "1999"), "constant")
  private val source5 = ConfigSource.fromMap(Map("DB_URL" -> "newyork.com"), "constant")

  private val oneValidSource =
    ConfigSource.fromMap(
      Map(
        "LDAP"   -> "jolap",
        "PORT"   -> "1999",
        "DB_URL" -> "newyork.com"
      ),
      "constant"
    )

  val myConfig =
    ((string("LDAP").from(source1.orElse(source3)) |@| int("PORT").from(source4)) |@|
      string("DB_URL").optional.from(source1.orElse(source5)))(MyConfig.apply, MyConfig.unapply)

  // Let's reset the whole source details in the original description
  val myConfigWithReset = myConfig.unsourced.from(oneValidSource) // Equivalent to myConfig.fromNothing

  // Have got a few more sources to be tried, on top of what's there already ?
  val myConfigChangedSource = myConfig.updateSource(_.orElse(source2))

  //

  assert(
    read(myConfig) == Right(MyConfig("jolap", 1999, Some("newyork.com")))
  )

  assert(
    read(myConfigWithReset) == Right(MyConfig("jolap", 1999, Some("newyork.com")))
  )

  assert(
    read(myConfigChangedSource) == Right(MyConfig("jolap", 1999, Some("newyork.com")))
  )
}
