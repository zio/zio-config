package zio.config.actions

import zio.ZIO
import zio.config.{ Config, ConfigReport, ConfigSource, _ }

case class Report[A](run: ZIO[ConfigSource, ReadErrors, ConfigReport])

// Report
object Report {
  final def report[A](config: Config[A]): Report[A] =
    Report(Read.read(config).run.map(_._1))
}
