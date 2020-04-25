package zio.config.typesafe

import zio.config.{ BaseSpec, ConfigSource }
import zio.config._, ConfigDescriptor._
import zio.test.Assertion._
import zio.test._
import TypesafeConfigReadWriteTestUtils._
import com.typesafe.config.ConfigRenderOptions
import TypesafeConfigTestSupport._

object TypesafeConfigReadWriteTest
    extends BaseSpec(
      suite("read-write roundtrip tests")(
        test(
          "Simple: read(descriptor from typesafeSource) == read(descriptor from write(descriptor, read(descriptor from typesafeSource)))"
        ) {
          val readSource =
            read(string("a") from TypesafeConfigSource.fromHoconString("a=b").loadOrThrow)

          val written =
            write(string("a"), "b").loadOrThrow.toHocon.render()

          val readWritten =
            read(string("a") from TypesafeConfigSource.fromHoconString(written).loadOrThrow)

          assert((readWritten, readSource))(equalTo((readSource, Right("b"))))
        },
        test(
          "Nested: read(descriptor from typesafeSource) == read(descriptor from write(descriptor, read(descriptor from typesafeSource)))"
        ) {
          val config =
            nested("a") { string("b") }

          val readSource =
            read(config from TypesafeConfigSource.fromHoconString("a : { b = c }").loadOrThrow)

          val written =
            write(config, "c").loadOrThrow.toHocon.render()

          val readWritten =
            read(config from TypesafeConfigSource.fromHoconString(written).loadOrThrow)

          assert((readWritten, readSource))(equalTo((readSource, Right("c"))))
        },
        test(
          "Nested List: read(descriptor from typesafeSource) == read(descriptor from write(descriptor, read(descriptor from typesafeSource)))"
        ) {
          val config =
            nested("a") { list("b") { string } }

          val readSource =
            read(config from TypesafeConfigSource.fromHoconString("a : { b = [c, c] }").loadOrThrow)

          val written =
            write(config, readSource.loadOrThrow).loadOrThrow.toHocon.render()

          val readWritten =
            read(config from TypesafeConfigSource.fromHoconString(written).loadOrThrow)

          assert((readWritten, readSource))(equalTo((readSource, Right(List("c", "c")))))
        },
        test(
          "Nested Empty List: read(descriptor from typesafeSource) == read(descriptor from write(descriptor, read(descriptor from typesafeSource)))"
        ) {
          val config =
            nested("a") { list("b") { string } }

          val readSource =
            read(config from TypesafeConfigSource.fromHoconString("a : { b = [] }").loadOrThrow)

          val written =
            write(config, readSource.loadOrThrow).loadOrThrow.toHocon.render()

          val readWritten =
            read(config from TypesafeConfigSource.fromHoconString(written).loadOrThrow)

          assert((readWritten, readSource))(equalTo((readSource, Right(Nil))))
        },
        test(
          "Nested Config with primitive lists: read(descriptor from typesafeSource) == read(descriptor from write(descriptor, read(descriptor from typesafeSource)))"
        ) {
          val hocon =
            s"""
               | a : {
               |    b : {
               |       c : {
               |          d : [a, b]
               |          e : [1, 2]
               |          f : 2
               |          g : []
               |       }
               |    }
               | }
               |
               | f : h
               |
               |""".stripMargin

          final case class DEF(d: List[String], e: List[Int], f: Int, g: List[String])
          final case class Cfg(f: String, de: DEF)

          val deConfig = nested("a") {
            nested("b") {
              nested("c") {
                ((list("d") { string } |@| list("e") { int } |@| int("f") |@| list("g") { string }))(
                  DEF.apply,
                  DEF.unapply
                )
              }
            }
          }

          val expected =
            Cfg("h", DEF(List("a", "b"), List(1, 2), 2, Nil))

          val fConfig = string("f")

          val config = (fConfig |@| deConfig)(Cfg.apply, Cfg.unapply)

          val readSource =
            read(config from TypesafeConfigSource.fromHoconString(hocon).loadOrThrow)

          val written =
            write(config, readSource.loadOrThrow).loadOrThrow.toHocon
              .render(ConfigRenderOptions.concise().setFormatted(true).setJson(false))

          val readWritten =
            read(config from TypesafeConfigSource.fromHoconString(written).loadOrThrow)

          assert((readWritten, readSource))(equalTo((readSource, Right(expected))))
        },
        test(
          "Map: read(descriptor from typesafeSource) == read(descriptor from write(descriptor, read(descriptor from typesafeSource)))"
        ) {
          val readSource =
            read(sssDescription from source)

          val written =
            write(sssDescription, readSource.loadOrThrow).loadOrThrow.toHocon.render()

          val readWritten =
            read(sssDescription from TypesafeConfigSource.fromHoconString(written).loadOrThrow)

          assert((readWritten, readSource))(
            equalTo(
              (readSource, Right(sss(Map("syd" -> List(1, 2), "melb" -> List(1)), Nil, List(1, 3, 3), Map("v" -> "a"))))
            )
          )
        },
        test(
          "Complex Map: read(descriptor from typesafeSource) == read(descriptor from write(descriptor, read(descriptor from typesafeSource)))"
        ) {
          val readSource =
            read(complexConfig from complexSource)

          val written =
            write(complexConfig, readSource.loadOrThrow).loadOrThrow.toHocon.render()

          val readWritten =
            read(complexConfig from TypesafeConfigSource.fromHoconString(written).loadOrThrow)

          val innerResult =
            sss(Map("melb" -> List(1), "syd" -> List(1, 2)), List(), List(1, 3, 3), Map("v" -> "a"))

          val expected =
            TypesafeConfigReadWriteTestUtils.Nested(Map("dynamic1" -> innerResult, "dynamic2" -> innerResult))

          assert((readWritten, readSource))(equalTo((readSource, Right(expected))))
        },
        test(
          "optional field: read(descriptor from typesafeSource) == read(descriptor from write(descriptor, read(descriptor from typesafeSource)))"
        ) {
          val optionalConfig =
            string("a").optional

          val readSource =
            read(optionalConfig from TypesafeConfigSource.fromHoconString("b = 1").loadOrThrow)

          val written =
            write(optionalConfig, readSource.loadOrThrow).loadOrThrow.toHocon.render()

          val readWritten =
            read(optionalConfig from TypesafeConfigSource.fromHoconString(written).loadOrThrow)

          assert((readWritten, readSource))(equalTo((readSource, Right(None))))
        },
        test(
          "complex source: read(descriptor from typesafeSource) == read(descriptor from write(descriptor, read(descriptor from typesafeSource)))"
        ) {
          val writtenHocon =
            write(complexDescription, readComplexSource).loadOrThrow.toHocon
              .render(ConfigRenderOptions.concise().setFormatted(true).setJson(false))

          val writtenProperty =
            write(complexDescription, readComplexSource).loadOrThrow

          val readWrittenProperty =
            read(complexDescription from ConfigSource.fromPropertyTree(writtenProperty, "tree")).loadOrThrow

          val readWrittenHocon =
            read(complexDescription from TypesafeConfigSource.fromHoconString(writtenHocon).loadOrThrow).loadOrThrow

          assert((readWrittenHocon, readWrittenProperty, readComplexSource))(
            equalTo((readComplexSource, expectedResult, expectedResult))
          )
        }
      )
    )

object TypesafeConfigReadWriteTestUtils {
  private val hocon =
    s"""
       | zones: {
       |    syd  : [1, 2]
       |    melb : [1]
       |  }
       |
       |  l : []
       |
       |  l2: [1, 3, 3]
       |
       |  z : {
       |     v : a
       |  }
       |""".stripMargin

  val source: ConfigSource = TypesafeConfigSource.fromHoconString(hocon).loadOrThrow

  final case class sss(s: Map[String, List[Int]], l: List[Int], l2: List[Int], value: Map[String, String])

  val c1: ConfigDescriptor[Map[String, List[Int]]] =
    map("zones")(list(int))

  private val c2 = list("l")(int)
  private val c3 = list("l2")(int)

  val c4: ConfigDescriptor[Map[String, String]] =
    map("z")(string)

  val sssDescription: ConfigDescriptor[sss] =
    (c1 |@| c2 |@| c3 |@| c4)((a, b, c, d) => sss(a, b, c, d), sss.unapply)

  final case class Nested(s: Map[String, sss])

  val hocon2: String =
    s"""
       |result : {
       | dynamic1 : {
       |       zones: {
       |         syd  : [1, 2]
       |         melb : [1]
       |      }
       |
       |       l : []
       |
       |       l2: [1, 3, 3]
       |
       |       z : {
       |         v : a
       |       }
       | }
       |
       | dynamic2 : {
       |      zones: {
       |        syd  : [1, 2]
       |        melb : [1]
       |      }
       |
       |      l : []
       |
       |     l2: [1, 3, 3]
       |
       |      z : {
       |         v : a
       |      }
       | }
       |}
       |""".stripMargin

  val complexConfig: ConfigDescriptor[Nested] =
    nested("result")(mapStrict(sssDescription))(
      TypesafeConfigReadWriteTestUtils.Nested.apply,
      TypesafeConfigReadWriteTestUtils.Nested.unapply
    )

  val complexSource: ConfigSource =
    TypesafeConfigSource.fromHoconString(hocon2).loadOrThrow
}
