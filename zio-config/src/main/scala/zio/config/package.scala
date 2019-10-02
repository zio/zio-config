package zio

import zio.config.actions.{ Read, Report, Write }

package object config extends Sources {
  type ConfigOption[A] = Config[Option[A]]

  def read[A](config: => Config[A]): Read[A]     = Read.read[A](config)
  def write[A](config: => Config[A]): Write[A]   = Write.write[A](config)
  def report[A](config: => Config[A]): Report[A] = Report.report[A](config)

  def getConfigValue(path: String): ZIO[ConfigSource, Unit, String] = ZIO.accessM(_.configService.getConfigValue(path))

  type ReadErrors = ::[ReadError]

  object ReadErrors {
    def apply(a: ReadError, as: ReadError*): ReadErrors =
      ::(a, as.toList)

    def concat(l: ReadErrors, r: ReadErrors): ReadErrors =
      ::(l.head, l.tail ++ r)
  }
}
