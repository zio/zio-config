package zio.config

import ConfigDescriptor._

final case class Config(x: String, y: String)

object Hello extends App {
  val s = ConfigSource.fromPropertiesFile("filepath").memoize

  // val hello = s.access.flatMap(_.map(a => a(PropertyTreePath.apply(Vector.empty)))).use(identity)

  // val hello =
  //   for {
  //     reader <- s.run.access.map(_.map(_(PropertyTreePath.apply(Vector.empty))))
  //     x      <- reader
  //     x1     <- reader
  //     x2     <- reader
  //     x3     <- reader
  //     x      <- reader
  //     h      <- x.toManaged_
  //     h      <- x1.toManaged_
  //     h      <- x2.toManaged_
  //     h      <- x3.toManaged_
  //   } yield h

  // val s2 = zio.Runtime.default.unsafeRun(hello.use(a => zio.ZIO.succeed(a)))

  val hi =
    read((string("x") |@| string("x")).to[Config] from s)

  val s2 =
    zio.Runtime.default.unsafeRun(hi)

  println(s2)
}
