// package zio.config.typesafe

// import com.typesafe.config.ConfigRenderOptions
// import zio.config.{BaseSpec, _}
// import zio.test.Assertion._
// import zio.test._

// import ConfigDescriptor._
// import TypesafeConfigReadWriteTestUtils._
// import TypesafeConfigTestSupport._

// object TypesafeConfigReadWriteTest extends BaseSpec {

//   val spec: Spec[Environment, Failure] =
//     suite("read-write roundtrip tests")(
//       testM(
//         "Simple: read(descriptor from typesafeSource) == read(descriptor from write(descriptor, read(descriptor from typesafeSource)))"
//       ) {
//         val readSource =
//           read(string("a") from TypesafeConfigSource.fromHoconString("a=b"))

//         val written =
//           write(string("a"), "b").loadOrThrow.toHocon.render()

//         val readWritten =
//           read(string("a") from TypesafeConfigSource.fromHoconString(written))

//         assertZIO(readWritten.zip(readSource))(equalTo(("b", "b")))
//       },
//       testM(
//         "Nested: read(descriptor from typesafeSource) == read(descriptor from write(descriptor, read(descriptor from typesafeSource)))"
//       ) {
//         val config =
//           nested("a")(string("b"))

//         val readSource =
//           read(config from TypesafeConfigSource.fromHoconString("a : { b = c }"))

//         val written =
//           write(config, "c").loadOrThrow.toHocon.render()

//         val readWritten =
//           read(config from TypesafeConfigSource.fromHoconString(written))

//         assert((readWritten.zip(readSource)))(equalTo(("c", "c")))
//       },
//       testM(
//         "Nested List: read(descriptor from typesafeSource) == read(descriptor from write(descriptor, read(descriptor from typesafeSource)))"
//       ) {
//         val config =
//           nested("a")(list("b")(string))

//         val result =
//           for {
//             readSource  <- read(config from TypesafeConfigSource.fromHoconString("a : { b = [c, c] }"))
//             written     <- ZIO.fromEither(write(config, result))
//             readWritten <- read(config from TypesafeConfigSource.fromHoconString(written))
//           } yield (readWritten, readSource)

//         assertZIO(result)(equalTo((List("c", "c"), List("c", "c"))))
//       },
//       testM(
//         "Nested Empty List: read(descriptor from typesafeSource) == read(descriptor from write(descriptor, read(descriptor from typesafeSource)))"
//       ) {
//         val config =
//           nested("a")(list("b")(string))

//         val result =
//           for {
//             readSource  <- read(config from TypesafeConfigSource.fromHoconString("a : { b = [] }"))
//             written     <- ZIO.fromEither(write(config, result))
//             readWritten <- read(config from TypesafeConfigSource.fromHoconString(written))
//           } yield (readWritten, readSource)

//         assertZIO(readWritten.zip(readSource))(equalTo((Nil, Nil)))
//       },
//       testM(
//         "Nested Config with primitive lists: read(descriptor from typesafeSource) == read(descriptor from write(descriptor, read(descriptor from typesafeSource)))"
//       ) {
//         val hocon =
//           s"""
//              | a : {
//              |    b : {
//              |       c : {
//              |          d : [a, b]
//              |          e : [1, 2]
//              |          f : 2
//              |          g : []
//              |       }
//              |    }
//              | }
//              |
//              | f : h
//              |
//              |""".stripMargin

//         final case class DEF(d: List[String], e: List[Int], f: Int, g: List[String])
//         final case class Cfg(f: String, de: DEF)

//         val deConfig = nested("a") {
//           nested("b") {
//             nested("c") {
//               ((list("d")(string) |@| list("e")(int) |@| int("f") |@| list("g")(string)))(
//                 DEF.apply,
//                 DEF.unapply
//               )
//             }
//           }
//         }

//         val expected =
//           Cfg("h", DEF(List("a", "b"), List(1, 2), 2, Nil))

//         val fConfig = string("f")

//         val config = (fConfig |@| deConfig)(Cfg.apply, Cfg.unapply)

//         val result =
//           for {
//             readSource  <- read(config from TypesafeConfigSource.fromHoconString(hocon))
//             written     <- ZIO.fromEither(write(config, result))
//             readWritten <- read(config from TypesafeConfigSource.fromHoconString(written))
//           } yield (readWritten, readSource)

//         assert(result)(equalTo((expected, expected)))
//       },
//       testM(
//         "Map: read(descriptor from typesafeSource) == read(descriptor from write(descriptor, read(descriptor from typesafeSource)))"
//       ) {
//         val readSource =
//           read(sssDescription from source)

//         val written =
//           write(sssDescription, readSource.loadOrThrow).loadOrThrow.toHocon.render()

//         val readWritten =
//           read(sssDescription from TypesafeConfigSource.fromHoconString(written))

//         val result =
//           for {
//             readSource  <- read(config from TypesafeConfigSource.fromHoconString("a : { b = [] }"))
//             written     <- ZIO.fromEither(write(config, result))
//             readWritten <- read(config from TypesafeConfigSource.fromHoconString(written))
//           } yield (readWritten, readSource)

//         val expected =
//           sss(Map("syd" -> List(1, 2), "melb" -> List(1)), Nil, List(1, 3, 3), Map("v" -> "a"))

//         assertZIO(readWritten.zip(readSource))(
//           equalTo(
//             (readSource, expected)
//           )
//         )
//       },
//       testM(
//         "Complex Map: read(descriptor from typesafeSource) == read(descriptor from write(descriptor, read(descriptor from typesafeSource)))"
//       ) {
//         val readSource =
//           read(complexConfig from complexSource)

//         val written =
//           write(complexConfig, readSource.loadOrThrow).loadOrThrow.toHocon.render()

//         val readWritten =
//           read(complexConfig from TypesafeConfigSource.fromHoconString(written))

//         val innerResult =
//           sss(Map("melb" -> List(1), "syd" -> List(1, 2)), List(), List(1, 3, 3), Map("v" -> "a"))

//         val expected    =
//           TypesafeConfigReadWriteTestUtils.Nested(Map("dynamic1" -> innerResult, "dynamic2" -> innerResult))

//         assertZIO((readWritten.zip(readSource)))(equalTo((expected, expected)))
//       },
//       testM(
//         "optional field: read(descriptor from typesafeSource) == read(descriptor from write(descriptor, read(descriptor from typesafeSource)))"
//       ) {
//         val optionalConfig =
//           string("a").optional

//         val readSource =
//           read(optionalConfig from TypesafeConfigSource.fromHoconString("b = 1"))

//         val written =
//           write(optionalConfig, readSource.loadOrThrow).loadOrThrow.toHocon.render()

//         val readWritten =
//           read(optionalConfig from TypesafeConfigSource.fromHoconString(written))

//         assertZIO(readWritten.zip(readSource))(equalTo((None, None)))
//       },
//       test(
//         "complex source: read(descriptor from typesafeSource) == read(descriptor from write(descriptor, read(descriptor from typesafeSource)))"
//       ) {
//         val writtenHocon =
//           write(complexDescription, readComplexSource).loadOrThrow.toHocon
//             .render(ConfigRenderOptions.concise().setFormatted(true).setJson(false))

//         val writtenProperty =
//           ZIO.fromEither(write(complexDescription, readComplexSource))

//         val readWrittenProperty =
//           read(
//             complexDescription from ConfigSource.fromPropertyTree(writtenProperty, "tree")
//           )

//         val readWrittenHocon =
//           read(complexDescription from TypesafeConfigSource.fromHoconString(writtenHocon))

//         assertZIO((readWrittenHocon.zip(readWrittenProperty).zip(readComplexSource)))(
//           equalTo((expectedResult, expectedResult, expectedResult))
//         )
//       },
//       testM(
//         "duplicate keys source: read(descriptor from typesafeSource) == read(descriptor from write(descriptor, read(descriptor from typesafeSource)))"
//       ) {
//         final case class A(metadata: List[B])

//         final case class B(metadata: List[C])

//         final case class C(c: String, metadata: String)

//         val cDescription: ConfigDescriptor[C] = (string("c") |@| string("metadata"))((c, m) => C(c, m), C.unapply)
//         val bDescription: ConfigDescriptor[B] = list("metadata")(cDescription)(B.apply, B.unapply)
//         val aDescription: ConfigDescriptor[A] = list("metadata")(bDescription)(A.apply, A.unapply)

//         val success = A(List(B(List(C(c = "abc", metadata = "xyz")))))

//         val duplicateKeysHocon =
//           s"""
//              | metadata: [
//              |   {
//              |     metadata: [
//              |       {
//              |         "c": "abc",
//              |         "metadata": "xyz"
//              |       }
//              |     ]
//              |   }
//              | ]
//              |""".stripMargin

//         val duplicateKeysSource: ConfigSource = TypesafeConfigSource.fromHoconString(duplicateKeysHocon)

//         val readSource = read(aDescription from duplicateKeysSource)

//         val written =
//           write(aDescription, readSource.loadOrThrow).loadOrThrow.toHocon.render()

//         val readWrittenHocon =
//           read(aDescription from TypesafeConfigSource.fromHoconString(written))

//         assertZIO((readSource.zip(readWrittenHocon)))(
//           equalTo(
//             (success, success)
//           )
//         )
//       }
//     )
// }

// object TypesafeConfigReadWriteTestUtils {
//   private val hocon =
//     s"""
//        | zones: {
//        |    syd  : [1, 2]
//        |    melb : [1]
//        |  }
//        |
//        |  l : []
//        |
//        |  l2: [1, 3, 3]
//        |
//        |  z : {
//        |     v : a
//        |  }
//        |""".stripMargin

//   val source: ConfigSource = TypesafeConfigSource.fromHoconString(hocon)

//   final case class sss(s: Map[String, List[Int]], l: List[Int], l2: List[Int], value: Map[String, String])

//   val c1: ConfigDescriptor[Map[String, List[Int]]] =
//     map("zones")(list(int))

//   private val c2 = list("l")(int)
//   private val c3 = list("l2")(int)

//   val c4: ConfigDescriptor[Map[String, String]] =
//     map("z")(string)

//   val sssDescription: ConfigDescriptor[sss] =
//     (c1 |@| c2 |@| c3 |@| c4)((a, b, c, d) => sss(a, b, c, d), sss.unapply)

//   final case class Nested(s: Map[String, sss])

//   val hocon2: String =
//     s"""
//        |result : {
//        | dynamic1 : {
//        |       zones: {
//        |         syd  : [1, 2]
//        |         melb : [1]
//        |      }
//        |
//        |       l : []
//        |
//        |       l2: [1, 3, 3]
//        |
//        |       z : {
//        |         v : a
//        |       }
//        | }
//        |
//        | dynamic2 : {
//        |      zones: {
//        |        syd  : [1, 2]
//        |        melb : [1]
//        |      }
//        |
//        |      l : []
//        |
//        |     l2: [1, 3, 3]
//        |
//        |      z : {
//        |         v : a
//        |      }
//        | }
//        |}
//        |""".stripMargin

//   val complexConfig: ConfigDescriptor[Nested] =
//     nested("result")(map(sssDescription))(
//       TypesafeConfigReadWriteTestUtils.Nested.apply,
//       TypesafeConfigReadWriteTestUtils.Nested.unapply
//     )

//   val complexSource: ConfigSource =
//     TypesafeConfigSource.fromHoconString(hocon2)
// }
