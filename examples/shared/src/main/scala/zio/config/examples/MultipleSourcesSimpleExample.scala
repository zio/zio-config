package zio.config.examples

import com.github.ghik.silencer.silent
import zio.config._

import Config._

object MultipleSourcesSimpleExample extends App {

  final case class MyConfig(ldap: String, port: Int, dburl: Option[String])

  // Only used to fetch the source - the pattern is only for explanation purpose
  val runtime = zio.Runtime.default

  // Assume they are different sources (env, property file, HOCON / database (in future))
  private val source1 = ConfigProvider.fromMap(Map("LDAP" -> "jolap"), "constant")
  @silent("deprecated")
  private val source2 = ConfigSource.fromSystemProps()

  private val source3 = ConfigSource.fromSystemEnv()
  private val source4 = ConfigProvider.fromMap(Map("PORT" -> "1999"), "constant")
  private val source5 = ConfigProvider.fromMap(Map("DB_URL" -> "newyork.com"), "constant")

  private val oneValidSource =
    ConfigProvider.fromMap(
      Map(
        "LDAP"   -> "jolap",
        "PORT"   -> "1999",
        "DB_URL" -> "newyork.com"
      ),
      "constant"
    )

  val myConfig: Config[MyConfig] =
    ((string("LDAP").from(source1.orElse(source3)) zip int("PORT").from(source4)) zip
      string("DB_URL").optional.from(source1.orElse(source5))).to[MyConfig]

  // Let's reset the whole source details in the original description
  val myConfigWithReset: Config[MyConfig] =
    myConfig.unsourced.from(oneValidSource) // Equivalent to myConfig.fromNothing

  // Have got a few more sources to be tried, on top of what's there already ?
  val myConfigChangedSource: Config[MyConfig] = myConfig.updateSource(_.orElse(source2))

  //

  assert(
    read(myConfig) equalM MyConfig("jolap", 1999, Some("newyork.com"))
  )

  assert(
    read(myConfigWithReset) equalM MyConfig("jolap", 1999, Some("newyork.com"))
  )

  assert(
    read(myConfigChangedSource) equalM MyConfig("jolap", 1999, Some("newyork.com"))
  )
}
