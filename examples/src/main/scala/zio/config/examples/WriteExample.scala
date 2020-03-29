package zio.config.examples

import zio.config._, ConfigDescriptor._
import zio.config.examples.typesafe.EitherImpureOps

object WriteExample extends App with EitherImpureOps {

  final case class Id(int: Int)

  final case class B(c: Id, d: Id)
  final case class A(b: B, i: Int)

  val description = {
    val bConfig = (int("c")(Id.apply, Id.unapply) |@| int("d")(Id.apply, Id.unapply))(B.apply, B.unapply)

    (bConfig |@| int("i"))(A.apply, A.unapply)
  }

  val map =
    Map(
      "c" -> "1",
      "d" -> "2",
      "i" -> "3"
    )

  // loadOrThrow here is only for the purpose of example
  val readFromSource: A =
    read(description from ConfigSource.fromMap(map, "map")).loadOrThrow

  val written: PropertyTree[String, String] =
    write(description, readFromSource).loadOrThrow

  val readFromTree: A =
    read(description from ConfigSource.fromPropertyTree(written, "tree")).loadOrThrow

  assert(readFromTree == readFromSource)

  val flattenedTree: Map[String, ::[String]] =
    written.flattenString()

  println(flattenedTree)

  val readFromMap =
    read(description from ConfigSource.fromMultiMap(written.flattenString(), "tree")).loadOrThrow

  assert(readFromMap == readFromSource)
}
