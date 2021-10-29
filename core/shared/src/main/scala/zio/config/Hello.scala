package zio.config

import ConfigDescriptor._

final case class Config(x: String, y: String, x2: String, x4: String)

object Hello extends App {
  val s = ConfigSource.fromPropertiesFile("filepath").memoize

  // val hello = s.access.flatMap(_.map(a => a(PropertyTreePath.apply(Vector.empty)))).use(identity)

  val hello =
    for {
      reader <- s.access.map(_.map(_(PropertyTreePath.apply(Vector.empty))))
      _      <- reader
      _      <- reader
      x      <- reader
      h      <- x.toManaged_
    } yield h

  val s2 = zio.Runtime.default.unsafeRun(hello.use(a => zio.ZIO.succeed(a)))

  // val s2 =
  //   zio.Runtime.default.unsafeRun(read((string("x") |@| string("y") |@| string("x") |@| string("y")).to[Config] from s))

  println(s2)
}
