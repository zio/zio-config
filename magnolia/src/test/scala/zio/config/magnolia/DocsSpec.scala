package zio.config.magnolia

import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.test.Assertion._
import zio.test._
import zio.config._, ConfigDescriptor._
import Table._, FieldName._

object DocsSpec extends DefaultRunnableSpec {
  val spec = suite("DocsSpec")(
    test("support default value") {

      final case class RawConfig(tableDetails: List[RawTableConfig2])

      final case class RawTableConfig2(
        database: Option[String],
        featureBucket: String,
        table: String,
        partitionScheme: Option[PartitionScheme],
        @describe("Example: as_at_date=2019-10-11/minor_version=1/run_time=123") partitionString: Option[String],
        numberOfPartitions: Option[Int],
        transformOptions: Option[TransformOptions],
        blockSizeMb: Option[Int],
        destination: Destination
      )

      final case class Destination(
        downstream: Downstream,
        dataFileNaming: Option[PatternFileNamingPolicy],
        eotFileNaming: Option[PatternFileNamingPolicy],
        controlFileNaming: Option[PatternFileNamingPolicy],
        rename: Option[Map[String, String]],
        roleArn: Option[String]
      )

      final case class Downstream(details: Either[S32, S3])
      final case class PatternFileNamingPolicy(pattern: String, dateFormat: String = "yyyyMMdd'T'HHmmss")

      final case class S3(
        d: String
      )

      final case class S32(
        c: String,
        e: String
      )

      final case class Sftp(
        host: String,
        username: String,
        authFilePath: String = "/opt/keys/sfmc/id_rsa.cij",
        port: Option[Int],
        directory: String,
        options: Map[String, String] = Map("StrictHostKeyChecking" -> "no"),
        timeout: Option[Int],
        encryption: Option[Encryption]
      )

      final case class Encryption(algorithm: String, keyFile: String = "/opt/keys/sfmc/pgp.sfmc.pub", armor: Boolean)

      final case class TransformOptions(
        columns: List[String],
        filter: Option[String],
        maskedColumns: Option[List[String]],
        header: Boolean,
        separator: String,
        compression: Option[String],
        dateFormat: Option[String],
        timestampFormat: Option[String],
        quoteAll: Boolean,
        emptyValue: Option[String],
        nullValue: Option[String],
        intermediateS3BasePath: String
      )

      final case class PartitionScheme(
        partitionName: Option[String],
        dateFormat: String,
        hasVersion: Boolean,
        hasRunDate: Boolean
      )

      case class H(b: Either[String, Int])
      case class X(c: String)

      import zio.config._, ConfigDescriptor._
      println(
        string
      )
      //val hello = nested("b")(string.orElseEither(int))
      // val hello2 = nested("b")(nested("c")(string).orElseEither(nested("d")(int)))(H.apply, H.unapply)

      println(descriptor[String])

      /*      println(generateDocs(hello).toTable.asMarkdownContent)
      println("*************************************")*/
      //  println(generateDocs(hello2).toTable.asMarkdownContent)
      //println("*************************************")

      println(generateDocs(descriptor[Downstream]).toTable.asMarkdownContent)
      //println(generateDocs(hello2).toTable.asMarkdownContent)

      // If left has nested in it, then the parent has to have key in it.
      // or if right has nested in it, then the parent has to have key in it.
      // And if parent has key in it, then remove the key from the non nested.
      //

      val result = 1

      assert(result)(equalTo(1))
    },
    test("A single nested config with inner orElseEither gives correct table") {
      val config = nested("a")(int.orElseEither(string))

      val result   = generateDocs(config).toTable
      val markdown = result.asMarkdownContent

      val expectedMarkdown =
        s"""
           |## Configuration Details
           |
           |
           ||FieldName|Format                |Description|Sources|
           ||---      |---                   |---        |---    |
           ||         |[any-one-of](#roota_0)|           |       |
           |
           |### root.a_0
           |
           ||FieldName|Format   |Description         |Sources|
           ||---      |---      |---                 |---    |
           ||a        |primitive|value of type string|       |
           ||a        |primitive|value of type int   |       |
           |""".stripMargin

      val expectedTable =
        Table(
          List(Root),
          List(
            TableRow(
              List(),
              Some(Format.AnyOneOf),
              List(),
              Some(
                Table(
                  List(Root, Key("a")),
                  List(
                    TableRow(
                      List(Root, Key("a")),
                      Some(Format.Primitive),
                      List(ConfigDocs.raw("value of type int")),
                      None,
                      Set()
                    ),
                    TableRow(
                      List(Root, Key("a")),
                      Some(Format.Primitive),
                      List(ConfigDocs.raw("value of type string")),
                      None,
                      Set()
                    )
                  )
                )
              ),
              Set()
            )
          )
        )

      assert(result)(equalTo(expectedTable)) && assert(markdown)(equalTo(expectedMarkdown))
    },
    test("An outer nested config with orElseEither with nested on left") {
      import zio.config._, ConfigDescriptor._

      val config = nested("a")(nested("b")(int).orElseEither(string))

      val result = generateDocs(config).toTable.asMarkdownContent

      val expectedMarkdown =
        s"""
           |## Configuration Details
           |
           |
           ||FieldName    |Format                |Description|Sources|
           ||---          |---                   |---        |---    |
           ||[a](#roota_0)|[any-one-of](#roota_0)|           |       |
           |
           |### root.a_0
           |
           ||FieldName|Format   |Description         |Sources|
           ||---      |---      |---                 |---    |
           ||         |primitive|value of type string|       |
           ||b        |primitive|value of type int   |       |
           |""".stripMargin

      assert(result)(equalTo(expectedMarkdown))
    },
    test("An outer nested config with orElseEither with nested on right") {
      import zio.config._, ConfigDescriptor._

      val config = nested("a")(int.orElseEither(nested("b")(string)))

      val result = generateDocs(config).toTable.asMarkdownContent

      val expectedMarkdown =
        s"""
           |## Configuration Details
           |
           |
           ||FieldName    |Format                |Description|Sources|
           ||---          |---                   |---        |---    |
           ||[a](#roota_0)|[any-one-of](#roota_0)|           |       |
           |
           |### root.a_0
           |
           ||FieldName|Format   |Description         |Sources|
           ||---      |---      |---                 |---    |
           ||b        |primitive|value of type string|       |
           ||         |primitive|value of type int   |       |
           |""".stripMargin

      assert(result)(equalTo(expectedMarkdown))
    },
    test("An outer nested config with orElseEither with nested on left and right") {
      import zio.config._, ConfigDescriptor._

      val config = nested("a")(nested("b")(int).orElseEither(nested("c")(string)))

      val result = generateDocs(config).toTable.asMarkdownContent

      val expectedMarkdown =
        s"""
           |## Configuration Details
           |
           |
           ||FieldName    |Format                |Description|Sources|
           ||---          |---                   |---        |---    |
           ||[a](#roota_0)|[any-one-of](#roota_0)|           |       |
           |
           |### root.a_0
           |
           ||FieldName|Format   |Description         |Sources|
           ||---      |---      |---                 |---    |
           ||c        |primitive|value of type string|       |
           ||b        |primitive|value of type int   |       |
           |""".stripMargin

      assert(result)(equalTo(expectedMarkdown))
    },
    test("An outer nested config with orElseEither with nested on orElseEither with 3 configs") {
      import zio.config._, ConfigDescriptor._

      val config = nested("a")(nested("b")(int).orElseEither(nested("c")(string).orElseEither(nested("d")(int))))

      val result = generateDocs(config).toTable.asMarkdownContent

      val expectedMarkdown =
        s"""
           |## Configuration Details
           |
           |
           ||FieldName    |Format                |Description|Sources|
           ||---          |---                   |---        |---    |
           ||[a](#roota_0)|[any-one-of](#roota_0)|           |       |
           |
           |### root.a_0
           |
           ||FieldName|Format   |Description         |Sources|
           ||---      |---      |---                 |---    |
           ||d        |primitive|value of type int   |       |
           ||c        |primitive|value of type string|       |
           ||b        |primitive|value of type int   |       |
           |""".stripMargin

      assert(result)(equalTo(expectedMarkdown))
    },
    test("Markdown works for configs with no keys") {
      import zio.config._, ConfigDescriptor._

      val config = nested("a")(string).orElseEither(nested("b")(int).orElseEither(double))

      val result = generateDocs(config).toTable.asMarkdownContent

      val expectedMarkdown =
        s"""
           |## Configuration Details
           |
           |
           ||FieldName|Format               |Description|Sources|
           ||---      |---                  |---        |---    |
           ||         |[any-one-of](#root_0)|           |       |
           |
           |### root_0
           |
           ||FieldName|Format   |Description         |Sources|
           ||---      |---      |---                 |---    |
           ||         |primitive|value of type double|       |
           ||b        |primitive|value of type int   |       |
           ||a        |primitive|value of type string|       |
           |""".stripMargin

      assert(result)(equalTo(expectedMarkdown))
    },
    test("Markdown works a single key that can be multiple types") {
      import zio.config._, ConfigDescriptor._

      val config =
        nested("a")(string.orElseEither(int).orElseEither(double))

      val result = generateDocs(config).toTable.asMarkdownContent

      val expectedMarkdown =
        s"""
           |## Configuration Details
           |
           |
           ||FieldName|Format                |Description|Sources|
           ||---      |---                   |---        |---    |
           ||         |[any-one-of](#roota_0)|           |       |
           |
           |### root.a_0
           |
           ||FieldName|Format   |Description         |Sources|
           ||---      |---      |---                 |---    |
           ||a        |primitive|value of type double|       |
           ||a        |primitive|value of type int   |       |
           ||a        |primitive|value of type string|       |
           |""".stripMargin

      assert(result)(equalTo(expectedMarkdown))
    },
    test("Markdown works a single key that can be multiple types zipped with same") {
      import zio.config._, ConfigDescriptor._

      val config =
        nested("a")(string.orElseEither(int).orElseEither(double))

      val result = generateDocs(config.zip(config)).toTable.asMarkdownContent

      val expectedMarkdown =
        s"""
           |## Configuration Details
           |
           |
           ||FieldName|Format           |Description|Sources|
           ||---      |---              |---        |---    |
           ||         |[all-of](#root_0)|           |       |
           |
           |### root_0
           |
           ||FieldName|Format                  |Description|Sources|
           ||---      |---                     |---        |---    |
           ||         |[any-one-of](#roota_0_1)|           |       |
           ||         |[any-one-of](#roota_0_0)|           |       |
           |
           |### root.a_0_1
           |
           ||FieldName|Format   |Description         |Sources|
           ||---      |---      |---                 |---    |
           ||a        |primitive|value of type double|       |
           ||a        |primitive|value of type int   |       |
           ||a        |primitive|value of type string|       |
           |
           |### root.a_0_0
           |
           ||FieldName|Format   |Description         |Sources|
           ||---      |---      |---                 |---    |
           ||a        |primitive|value of type double|       |
           ||a        |primitive|value of type int   |       |
           ||a        |primitive|value of type string|       |
           |""".stripMargin

      assert(result)(equalTo(expectedMarkdown))
    },
    test("Markdown works when there a parent key with a few childs not having key") {
      import zio.config._, ConfigDescriptor._

      val config =
        nested("a")(string.orElseEither(nested("b")(int).orElseEither(double).orElseEither(long)))

      val result = generateDocs(config).toTable.asMarkdownContent

      val expectedMarkdown =
        s"""
           |## Configuration Details
           |
           |
           ||FieldName|Format           |Description|Sources|
           ||---      |---              |---        |---    |
           ||         |[all-of](#root_0)|           |       |
           |
           |### root_0
           |
           ||FieldName|Format                  |Description|Sources|
           ||---      |---                     |---        |---    |
           ||         |[any-one-of](#roota_0_1)|           |       |
           ||         |[any-one-of](#roota_0_0)|           |       |
           |
           |### root.a_0_1
           |
           ||FieldName|Format   |Description         |Sources|
           ||---      |---      |---                 |---    |
           ||a        |primitive|value of type double|       |
           ||a        |primitive|value of type int   |       |
           ||a        |primitive|value of type string|       |
           |
           |### root.a_0_0
           |
           ||FieldName|Format   |Description         |Sources|
           ||---      |---      |---                 |---    |
           ||a        |primitive|value of type double|       |
           ||a        |primitive|value of type int   |       |
           ||a        |primitive|value of type string|       |
           |""".stripMargin

      assert(result)(equalTo(expectedMarkdown))
    }
  )
}

object Hi extends App {
  val config =
    nested("d")(
      nested("a")(
        (int("b") |@| string("h").orElseEither((string("k") |@| string("k1") |@| string("k2")).tupled)).tupled
      )
    )

  val result = generateDocs(config).toTable.asMarkdownContent

  println(result)
}
