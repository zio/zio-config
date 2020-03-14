package zio.config.examples

import zio.config.ConfigDescriptor._
import zio.config._

object MultipleSources extends App {

  final case class MyConfig(ldap: String, port: Int, dburl: Option[String])

  // Assume they are different sources (env, property file, HOCON / database (in future))
  private val source1 = ConfigSource.fromMap(Map("LDAP" -> "jolap"))
  private val source3 = ConfigSource.fromEnv(None)
  private val source4 = ConfigSource.fromMap(Map("PORT" -> "1999"))
  private val source5 = ConfigSource.fromMap(Map("DB_URL" -> "newyork.com"))

  private val oneValidSource =
    ConfigSource.fromMap(
      Map(
        "LDAP"   -> "jolap",
        "PORT"   -> "1999",
        "DB_URL" -> "newyork.com"
      )
    )

  val myConfig =
    ((string("LDAP").from(source1.orElse(source3)) |@| int("PORT").from(source4)) |@|
      string("DB_URL").optional.from(source1.orElse(source5)))(MyConfig.apply, MyConfig.unapply)

  // Let's reset the whole source details in the original description
  val myConfigWithReset = myConfig.unsourced.from(oneValidSource) // Equivalent to myConfig.fromNothing

  // Have got a few more sources to be tried, on top of what's there already ?
  val myConfigChangedSource = myConfig.updateSource(_.orElse(ConfigSource.fromProperty(None)))

  //
  val runtime = zio.Runtime.default

  assert(
    runtime.unsafeRun(read(myConfig)) == MyConfig("jolap", 1999, Some("newyork.com"))
  )

  assert(
    runtime.unsafeRun(read(myConfigWithReset)) == MyConfig("jolap", 1999, Some("newyork.com"))
  )

  assert(
    runtime.unsafeRun(read(myConfigChangedSource)) == MyConfig("jolap", 1999, Some("newyork.com"))
  )
}
