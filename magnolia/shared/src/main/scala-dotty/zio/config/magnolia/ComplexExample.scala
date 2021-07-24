package zio.config.magnolia

import zio.config.magnolia._
import zio.config._

sealed trait Cloud

object Cloud {
  case object Aws                                           extends Cloud
  case object Azure                                         extends Cloud
  case class Gcp(@name("DOMAIN") domain: String, e: String) extends Cloud

}

final case class RawConfig(tableDetails: List[RawTableConfig])

final case class RawTableConfig(
  x: String,
                                 transformOptions: Option[TransformOptions],
                                 //database: Option[String],
//                                 featureBucket: String,
//                                 table: String,
//                                 @describe("partition scheme of s3 paths") partitionScheme: Option[PartitionScheme],
//                                 @describe("Example: as_at_date=2019-10-11/minor_version=1/run_time=123") partitionString: Option[String],
//                                 numberOfPartitions: Option[Int],
//                                 blockSizeMb: Option[Int],
//                                 destination: Destination,
//                                 cloud: Cloud
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
final case class PatternFileNamingPolicy(pattern: String, dateFormat: String)

final case class S3(
                     d: String
                   )

final case class S32(
                      d: String,
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
                                   maskedColumns: Option[String],
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

object X extends App:
  println(generateDocs(descriptor[RawConfig]).toTable.toGithubFlavouredMarkdown)
end X
