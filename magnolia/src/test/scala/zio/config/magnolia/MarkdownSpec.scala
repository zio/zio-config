package zio.config.magnolia

import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.test.Assertion._
import zio.test._
import zio.config._, ConfigDescriptor._

object MarkdownSpec extends DefaultRunnableSpec {
  val spec = suite("Markdown Spec")(
    test("asMarkdown works for a complex config") {

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

      final case class Downstream(`type`: String, details: Either[S32, S3])
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

      val expectedMarkdown =
        s"""
           |## Configuration Details
           |
           |
           ||FieldName                        |Format                   |Description|Sources|
           ||---                              |---                      |---        |---    |
           ||[tableDetails](#roottabledetails)|[list](#roottabledetails)|           |       |
           |
           |### root.tableDetails
           |
           ||FieldName                            |Format                     |Description                                                                                      |Sources|
           ||---                                  |---                        |---                                                                                              |---    |
           ||featureBucket                        |primitive                  |value of type string                                                                             |       |
           ||table                                |primitive                  |value of type string                                                                             |       |
           ||[partitionScheme](#partitionscheme)  |[all-of](#partitionscheme) |optional value                                                                                   |       |
           ||partitionString                      |primitive                  |value of type string, optional value, Example: as_at_date=2019-10-11/minor_version=1/run_time=123|       |
           ||numberOfPartitions                   |primitive                  |value of type int, optional value                                                                |       |
           ||[transformOptions](#transformoptions)|[all-of](#transformoptions)|optional value                                                                                   |       |
           ||blockSizeMb                          |primitive                  |value of type int, optional value                                                                |       |
           ||[destination](#destination)          |[all-of](#destination)     |                                                                                                 |       |
           ||database                             |primitive                  |value of type string, optional value                                                             |       |
           |
           |### partitionScheme
           |
           ||FieldName    |Format   |Description                         |Sources|
           ||---          |---      |---                                 |---    |
           ||dateFormat   |primitive|value of type string                |       |
           ||hasVersion   |primitive|value of type boolean               |       |
           ||hasRunDate   |primitive|value of type boolean               |       |
           ||partitionName|primitive|value of type string, optional value|       |
           |
           |### transformOptions
           |
           ||FieldName             |Format   |Description                         |Sources|
           ||---                   |---      |---                                 |---    |
           ||filter                |primitive|value of type string, optional value|       |
           ||maskedColumns         |list     |value of type string, optional value|       |
           ||header                |primitive|value of type boolean               |       |
           ||separator             |primitive|value of type string                |       |
           ||compression           |primitive|value of type string, optional value|       |
           ||dateFormat            |primitive|value of type string, optional value|       |
           ||timestampFormat       |primitive|value of type string, optional value|       |
           ||quoteAll              |primitive|value of type boolean               |       |
           ||emptyValue            |primitive|value of type string, optional value|       |
           ||nullValue             |primitive|value of type string, optional value|       |
           ||intermediateS3BasePath|primitive|value of type string                |       |
           ||columns               |list     |value of type string                |       |
           |
           |### destination
           |
           ||FieldName                              |Format                      |Description                         |Sources|
           ||---                                    |---                         |---                                 |---    |
           ||[dataFileNaming](#datafilenaming)      |[all-of](#datafilenaming)   |optional value                      |       |
           ||[eotFileNaming](#eotfilenaming)        |[all-of](#eotfilenaming)    |optional value                      |       |
           ||[controlFileNaming](#controlfilenaming)|[all-of](#controlfilenaming)|optional value                      |       |
           ||rename                                 |map                         |value of type string, optional value|       |
           ||roleArn                                |primitive                   |value of type string, optional value|       |
           ||[downstream](#downstream)              |[all-of](#downstream)       |                                    |       |
           |
           |### dataFileNaming
           |
           ||FieldName |Format   |Description                                           |Sources|
           ||---       |---      |---                                                   |---    |
           ||dateFormat|primitive|value of type string, default value: yyyyMMdd'T'HHmmss|       |
           ||pattern   |primitive|value of type string                                  |       |
           |
           |### eotFileNaming
           |
           ||FieldName |Format   |Description                                           |Sources|
           ||---       |---      |---                                                   |---    |
           ||dateFormat|primitive|value of type string, default value: yyyyMMdd'T'HHmmss|       |
           ||pattern   |primitive|value of type string                                  |       |
           |
           |### controlFileNaming
           |
           ||FieldName |Format   |Description                                           |Sources|
           ||---       |---      |---                                                   |---    |
           ||dateFormat|primitive|value of type string, default value: yyyyMMdd'T'HHmmss|       |
           ||pattern   |primitive|value of type string                                  |       |
           |
           |### downstream
           |
           ||FieldName          |Format                |Description         |Sources|
           ||---                |---                   |---                 |---    |
           ||[details](#details)|[any-one-of](#details)|                    |       |
           ||type               |primitive             |value of type string|       |
           |
           |### details
           |
           ||FieldName|Format         |Description         |Sources|
           ||---      |---            |---                 |---    |
           ||d        |primitive      |value of type string|       |
           ||         |[all-of](#root)|                    |       |
           |
           |### root
           |
           ||FieldName|Format   |Description         |Sources|
           ||---      |---      |---                 |---    |
           ||e        |primitive|value of type string|       |
           ||c        |primitive|value of type string|       |
           |""".stripMargin

      val markdown = generateDocs(descriptor[RawConfig]).toTable.asMarkdown

      assert(markdown)(equalTo(expectedMarkdown))
    },
    test("asMarkdown works for nested config with inner orElseEither gives correct table") {
      val config = nested("a")(int.orElseEither(string))

      val markdown = generateDocs(config).toTable.asMarkdown

      val expectedMarkdown =
        s"""
           |## Configuration Details
           |
           |
           ||FieldName  |Format              |Description|Sources|
           ||---        |---                 |---        |---    |
           ||[a](#roota)|[any-one-of](#roota)|           |       |
           |
           |### root.a
           |
           ||FieldName|Format   |Description         |Sources|
           ||---      |---      |---                 |---    |
           ||         |primitive|value of type string|       |
           ||         |primitive|value of type int   |       |
           |""".stripMargin

      assert(markdown)(equalTo(expectedMarkdown)) && assert(markdown)(equalTo(expectedMarkdown))
    },
    test("An outer nested config with orElseEither with nested on left") {
      import zio.config._, ConfigDescriptor._

      val config = nested("a")(nested("b")(int).orElseEither(string))

      val result = generateDocs(config).toTable.asMarkdown

      val expectedMarkdown =
        s"""
           |## Configuration Details
           |
           |
           ||FieldName  |Format              |Description|Sources|
           ||---        |---                 |---        |---    |
           ||[a](#roota)|[any-one-of](#roota)|           |       |
           |
           |### root.a
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

      val result = generateDocs(config).toTable.asMarkdown

      val expectedMarkdown =
        s"""
           |## Configuration Details
           |
           |
           ||FieldName  |Format              |Description|Sources|
           ||---        |---                 |---        |---    |
           ||[a](#roota)|[any-one-of](#roota)|           |       |
           |
           |### root.a
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

      val result = generateDocs(config).toTable.asMarkdown

      val expectedMarkdown =
        s"""
           |## Configuration Details
           |
           |
           ||FieldName  |Format              |Description|Sources|
           ||---        |---                 |---        |---    |
           ||[a](#roota)|[any-one-of](#roota)|           |       |
           |
           |### root.a
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

      val result = generateDocs(config).toTable.asMarkdown

      val expectedMarkdown =
        s"""
           |## Configuration Details
           |
           |
           ||FieldName  |Format              |Description|Sources|
           ||---        |---                 |---        |---    |
           ||[a](#roota)|[any-one-of](#roota)|           |       |
           |
           |### root.a
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

      val result = generateDocs(config).toTable.asMarkdown

      val expectedMarkdown =
        s"""
           |## Configuration Details
           |
           |
           ||FieldName|Format             |Description|Sources|
           ||---      |---                |---        |---    |
           ||         |[any-one-of](#root)|           |       |
           |
           |### root
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

      val result = generateDocs(config).toTable.asMarkdown

      val expectedMarkdown =
        s"""
           |## Configuration Details
           |
           |
           ||FieldName  |Format              |Description|Sources|
           ||---        |---                 |---        |---    |
           ||[a](#roota)|[any-one-of](#roota)|           |       |
           |
           |### root.a
           |
           ||FieldName|Format   |Description         |Sources|
           ||---      |---      |---                 |---    |
           ||         |primitive|value of type double|       |
           ||         |primitive|value of type int   |       |
           ||         |primitive|value of type string|       |
           |""".stripMargin

      assert(result)(equalTo(expectedMarkdown))
    },
    test("Markdown works a single key that can be multiple types zipped with same") {
      import zio.config._, ConfigDescriptor._

      val config1 =
        nested("a")(string.orElseEither(int).orElseEither(double))

      val config2 =
        nested("b")(string.orElseEither(int).orElseEither(double))

      val result = generateDocs(config1.zip(config2)).toTable.asMarkdown

      val expectedMarkdown =
        s"""
           |## Configuration Details
           |
           |
           ||FieldName|Format         |Description|Sources|
           ||---      |---            |---        |---    |
           ||         |[all-of](#root)|           |       |
           |
           |### root_0
           |
           ||FieldName|Format                |Description|Sources|
           ||---      |---                   |---        |---    |
           ||         |[any-one-of](#roota)  |           |       |
           ||         |[any-one-of](#roota-1)|           |       |
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
        nested("a")(
          nested("b")(
            (int("c") |@| string("d").orElseEither((string("e") |@| string("f") |@| string("g")).tupled)).tupled
          )
        )

      val result = generateDocs(config).toTable.asMarkdown

      val expectedMarkdown =
        s"""
           |## Configuration Details
           |
           |
           ||FieldName  |Format          |Description|Sources|
           ||---        |---             |---        |---    |
           ||[a](#roota)|[nested](#roota)|           |       |
           |
           |### root.a
           |
           ||FieldName   |Format           |Description|Sources|
           ||---         |---              |---        |---    |
           ||[b](#rootab)|[all-of](#rootab)|           |       |
           |
           |### root.a.b
           |
           ||FieldName|Format             |Description      |Sources|
           ||---      |---                |---              |---    |
           ||         |[any-one-of](#root)|                 |       |
           ||c        |primitive          |value of type int|       |
           |
           |### root
           |
           ||FieldName|Format           |Description         |Sources|
           ||---      |---              |---                 |---    |
           ||         |[all-of](#root-1)|                    |       |
           ||d        |primitive        |value of type string|       |
           |
           |### root
           |
           ||FieldName|Format   |Description         |Sources|
           ||---      |---      |---                 |---    |
           ||g        |primitive|value of type string|       |
           ||f        |primitive|value of type string|       |
           ||e        |primitive|value of type string|       |
           |""".stripMargin

      assert(result)(equalTo(expectedMarkdown))
    }
  )
}
