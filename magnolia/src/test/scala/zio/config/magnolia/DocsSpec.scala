package zio.config.magnolia

import zio.config._
import zio.config.magnolia.DeriveConfigDescriptor.descriptor
import zio.test.Assertion._
import zio.test._

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

      final case class Downstream(`type`: Option[String], details: Either[Sftp, S3])
      final case class PatternFileNamingPolicy(pattern: String, dateFormat: String = "yyyyMMdd'T'HHmmss")

      final case class S3(
        bucket: String
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

      println(generateDocs(descriptor[RawConfig]).toTable.asMarkdownContent)

      val result = 1

      assert(result)(equalTo(1))
    }
  )
}
