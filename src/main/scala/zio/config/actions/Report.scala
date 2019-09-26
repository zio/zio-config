package zio.config.actions

import zio.config.{ Config, ConfigReport, ConfigSource, ReadError }
import zio.ZIO

case class Report[A](run: ZIO[ConfigSource, List[ReadError], ConfigReport])

// Report
object Report {
  final def report[A](config: Config[A]): Report[A] =
    Report(Read.read(config).run.map(_._1))
}
