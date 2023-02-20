package zio.config.magnolia

import zio.config._
import zio.test.Assertion._
import zio.test._

import zio.Config, Config._
import MarkdowSpecUtils._

object MarkdownSpec extends BaseSpec {
  val spec = suite("Markdown Spec")(
    test("toGithubFlavouredMarkdown works for a complex config") {
      val markdown =
        generateDocs(deriveConfig[RawConfig]).toTable.toGithubFlavouredMarkdown

      assert(markdown)(equalTo(expectedMarkdown))
    },
    test("toGithubFlavouredMarkdown works for nested config with inner orElseEither gives correct table") {
      val config = int.orElseEither(string).nested("a")

      val markdown = generateDocs(config).toTable.toGithubFlavouredMarkdown

      val expectedMarkdown =
        s"""
           |## Configuration Details
           |
           |
           ||FieldName|Format         |Description|Sources|
           ||---      |---            |---        |---    |
           ||[a](a)   |[any-one-of](a)|           |       |
           |
           |### a
           |
           ||FieldName|Format   |Description         |Sources|
           ||---      |---      |---                 |---    |
           ||         |primitive|value of type int   |       |
           ||         |primitive|value of type string|       |
           |""".stripMargin

      assert(markdown)(equalTo(expectedMarkdown)) && assert(markdown)(equalTo(expectedMarkdown))
    },
    test("An outer nested config with orElseEither with nested on left") {

      val config = int.nested("b").nested("a").orElseEither(string)

      val result = generateDocs(config).toTable.toGithubFlavouredMarkdown

      val expectedMarkdown =
        s"""
           |## Configuration Details
           |
           |
           ||FieldName|Format         |Description|Sources|
           ||---      |---            |---        |---    |
           ||[a](a)   |[any-one-of](a)|           |       |
           |
           |### a
           |
           ||FieldName|Format   |Description         |Sources|
           ||---      |---      |---                 |---    |
           ||b        |primitive|value of type int   |       |
           ||         |primitive|value of type string|       |
           |""".stripMargin

      assert(result)(equalTo(expectedMarkdown))
    },
    test("An outer nested config with orElseEither with nested on right") {

      val config = (int.orElseEither(string("b"))).nested("a")

      val result = generateDocs(config).toTable.toGithubFlavouredMarkdown

      val expectedMarkdown =
        s"""
           |## Configuration Details
           |
           |
           ||FieldName|Format         |Description|Sources|
           ||---      |---            |---        |---    |
           ||[a](a)   |[any-one-of](a)|           |       |
           |
           |### a
           |
           ||FieldName|Format   |Description         |Sources|
           ||---      |---      |---                 |---    |
           ||         |primitive|value of type int   |       |
           ||b        |primitive|value of type string|       |
           |""".stripMargin

      assert(result)(equalTo(expectedMarkdown))
    },
    test("Markdown works for a single key that can be more than 2 types") {

      val config =
       (string.orElseEither(int).orElseEither(double)).nested("a")

      val result = generateDocs(config).toTable.toGithubFlavouredMarkdown

      val expectedMarkdown =
        s"""
           |## Configuration Details
           |
           |
           ||FieldName|Format         |Description|Sources|
           ||---      |---            |---        |---    |
           ||[a](a)   |[any-one-of](a)|           |       |
           |
           |### a
           |
           ||FieldName|Format   |Description         |Sources|
           ||---      |---      |---                 |---    |
           ||         |primitive|value of type string|       |
           ||         |primitive|value of type int   |       |
           ||         |primitive|value of type double|       |
           |""".stripMargin

      assert(result)(equalTo(expectedMarkdown))
    },
    test("Markdown works for a single key that can be multiple types zipped with same config") {

      val config1 =
        (string.orElseEither(int).orElseEither(double)).nested("a")

      val config2 =
        (string.orElseEither(int).orElseEither(double)).nested("b")

      val result = generateDocs(config1.zip(config2)).toTable.toGithubFlavouredMarkdown

      val expectedMarkdown =
        s"""
           |## Configuration Details
           |
           |
           ||FieldName|Format                     |Description|Sources|
           ||---      |---                        |---        |---    |
           ||         |[all-of](fielddescriptions)|           |       |
           |
           |### Field Descriptions
           |
           ||FieldName|Format         |Description|Sources|
           ||---      |---            |---        |---    |
           ||[a](a)   |[any-one-of](a)|           |       |
           ||[b](b)   |[any-one-of](b)|           |       |
           |
           |### a
           |
           ||FieldName|Format   |Description         |Sources|
           ||---      |---      |---                 |---    |
           ||         |primitive|value of type string|       |
           ||         |primitive|value of type int   |       |
           ||         |primitive|value of type double|       |
           |
           |### b
           |
           ||FieldName|Format   |Description         |Sources|
           ||---      |---      |---                 |---    |
           ||         |primitive|value of type string|       |
           ||         |primitive|value of type int   |       |
           ||         |primitive|value of type double|       |
           |""".stripMargin

      assert(result)(equalTo(expectedMarkdown))
    },
    test("Markdown works when there is a  parent key with a few children not having key") {

      val config =
        (
            (int("c") zip string("d").orElseEither((string("e") zip string("f") zip string("g")))).nested("b")
        ).nested("a")

      val result = generateDocs(config).toTable.toGithubFlavouredMarkdown

      val expectedMarkdown =
        s"""
           |## Configuration Details
           |
           |
           ||FieldName|Format     |Description|Sources|
           ||---      |---        |---        |---    |
           ||[a](a)   |[nested](a)|           |       |
           |
           |### a
           |
           ||FieldName|Format      |Description|Sources|
           ||---      |---         |---        |---    |
           ||[b](ab)  |[all-of](ab)|           |       |
           |
           |### a.b
           |
           ||FieldName|Format                         |Description      |Sources|
           ||---      |---                            |---              |---    |
           ||c        |primitive                      |value of type int|       |
           ||         |[any-one-of](fielddescriptions)|                 |       |
           |
           |### Field Descriptions
           |
           ||FieldName|Format                       |Description         |Sources|
           ||---      |---                          |---                 |---    |
           ||d        |primitive                    |value of type string|       |
           ||         |[all-of](fielddescriptions-1)|                    |       |
           |
           |### Field Descriptions
           |
           ||FieldName|Format   |Description         |Sources|
           ||---      |---      |---                 |---    |
           ||e        |primitive|value of type string|       |
           ||f        |primitive|value of type string|       |
           ||g        |primitive|value of type string|       |
           |""".stripMargin

      assert(result)(equalTo(expectedMarkdown))
    }
  )
}

object MarkdowSpecUtils {
  // Complex config
  sealed trait Cloud

  object Cloud {
    case object Aws extends Cloud

    case object Azure extends Cloud

    case class Gcp(@name("DOMAIN") domain: String, e: String) extends Cloud

  }

  final case class RawConfig(tableDetails: List[RawTableConfig])

  final case class RawTableConfig(
    database: Option[String],
    featureBucket: String,
    table: String,
    @describe("partition scheme of s3 paths") partitionScheme: Option[PartitionScheme],
    @describe("Example: as_at_date=2019-10-11/minor_version=1/run_time=123") partitionString: Option[String],
    numberOfPartitions: Option[Int],
    transformOptions: Option[TransformOptions],
    blockSizeMb: Option[Int],
    destination: Destination,
    cloud: Cloud
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

  final case class S3(d: String)

  final case class S32(d: String, e: String)

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

  val expectedMarkdown: String =
    s"""
       |## Configuration Details
       |
       |
       ||FieldName                   |Format              |Description|Sources|
       ||---                         |---                 |---        |---    |
       ||[tableDetails](tabledetails)|[list](tabledetails)|           |       |
       |
       |### tableDetails
       |
       ||FieldName                           |Format                    |Description                                                                                      |Sources|
       ||---                                 |---                       |---                                                                                              |---    |
       ||database                            |primitive                 |value of type string, optional value                                                             |       |
       ||[cloud](cloud)                      |[any-one-of](cloud)       |                                                                                                 |       |
       ||[destination](destination)          |[all-of](destination)     |                                                                                                 |       |
       ||blockSizeMb                         |primitive                 |value of type int, optional value                                                                |       |
       ||[transformOptions](transformoptions)|[all-of](transformoptions)|optional value                                                                                   |       |
       ||numberOfPartitions                  |primitive                 |value of type int, optional value                                                                |       |
       ||partitionString                     |primitive                 |value of type string, optional value, Example: as_at_date=2019-10-11/minor_version=1/run_time=123|       |
       ||[partitionScheme](partitionscheme)  |[all-of](partitionscheme) |optional value, partition scheme of s3 paths                                                     |       |
       ||table                               |primitive                 |value of type string                                                                             |       |
       ||featureBucket                       |primitive                 |value of type string                                                                             |       |
       |
       |### cloud
       |
       ||FieldName |Format       |Description            |Sources|
       ||---       |---          |---                    |---    |
       ||          |primitive    |constant string 'Aws'  |       |
       ||          |primitive    |constant string 'Azure'|       |
       ||[Gcp](gcp)|[all-of](gcp)|                       |       |
       |
       |### Gcp
       |
       ||FieldName|Format   |Description         |Sources|
       ||---      |---      |---                 |---    |
       ||DOMAIN   |primitive|value of type string|       |
       ||e        |primitive|value of type string|       |
       |
       |### destination
       |
       ||FieldName                             |Format                     |Description                         |Sources|
       ||---                                   |---                        |---                                 |---    |
       ||[downstream](downstream)              |[all-of](downstream)       |                                    |       |
       ||roleArn                               |primitive                  |value of type string, optional value|       |
       ||rename                                |map                        |value of type string, optional value|       |
       ||[controlFileNaming](controlfilenaming)|[all-of](controlfilenaming)|optional value                      |       |
       ||[eotFileNaming](eotfilenaming)        |[all-of](eotfilenaming)    |optional value                      |       |
       ||[dataFileNaming](datafilenaming)      |[all-of](datafilenaming)   |optional value                      |       |
       |
       |### downstream
       |
       ||FieldName         |Format               |Description         |Sources|
       ||---               |---                  |---                 |---    |
       ||type              |primitive            |value of type string|       |
       ||[details](details)|[any-one-of](details)|                    |       |
       |
       |### details
       |
       ||FieldName|Format                     |Description         |Sources|
       ||---      |---                        |---                 |---    |
       ||         |[all-of](fielddescriptions)|                    |       |
       ||d        |primitive                  |value of type string|       |
       |
       |### Field Descriptions
       |
       ||FieldName|Format   |Description         |Sources|
       ||---      |---      |---                 |---    |
       ||d        |primitive|value of type string|       |
       ||e        |primitive|value of type string|       |
       |
       |### controlFileNaming
       |
       ||FieldName |Format   |Description                                           |Sources|
       ||---       |---      |---                                                   |---    |
       ||pattern   |primitive|value of type string                                  |       |
       ||dateFormat|primitive|value of type string, default value: yyyyMMdd'T'HHmmss|       |
       |
       |### eotFileNaming
       |
       ||FieldName |Format   |Description                                           |Sources|
       ||---       |---      |---                                                   |---    |
       ||pattern   |primitive|value of type string                                  |       |
       ||dateFormat|primitive|value of type string, default value: yyyyMMdd'T'HHmmss|       |
       |
       |### dataFileNaming
       |
       ||FieldName |Format   |Description                                           |Sources|
       ||---       |---      |---                                                   |---    |
       ||pattern   |primitive|value of type string                                  |       |
       ||dateFormat|primitive|value of type string, default value: yyyyMMdd'T'HHmmss|       |
       |
       |### transformOptions
       |
       ||FieldName             |Format   |Description                         |Sources|
       ||---                   |---      |---                                 |---    |
       ||columns               |list     |value of type string                |       |
       ||intermediateS3BasePath|primitive|value of type string                |       |
       ||nullValue             |primitive|value of type string, optional value|       |
       ||emptyValue            |primitive|value of type string, optional value|       |
       ||quoteAll              |primitive|value of type boolean               |       |
       ||timestampFormat       |primitive|value of type string, optional value|       |
       ||dateFormat            |primitive|value of type string, optional value|       |
       ||compression           |primitive|value of type string, optional value|       |
       ||separator             |primitive|value of type string                |       |
       ||header                |primitive|value of type boolean               |       |
       ||maskedColumns         |list     |value of type string, optional value|       |
       ||filter                |primitive|value of type string, optional value|       |
       |
       |### partitionScheme
       |
       ||FieldName    |Format   |Description                         |Sources|
       ||---          |---      |---                                 |---    |
       ||partitionName|primitive|value of type string, optional value|       |
       ||hasRunDate   |primitive|value of type boolean               |       |
       ||hasVersion   |primitive|value of type boolean               |       |
       ||dateFormat   |primitive|value of type string                |       |
       |""".stripMargin
}
