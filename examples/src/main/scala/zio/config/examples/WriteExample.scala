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

  val flattenedTree: Map[String, ::[String]] =
    written.flattenString()

  assert(
    written.flattenString() ==
      Map(
        "c" -> List("1"),
        "d" -> List("2"),
        "i" -> List("3")
      )
  )

  val readFromTree: A =
    read(description from ConfigSource.fromPropertyTree(written, "tree", LeafForSequence.Valid)).loadOrThrow

  assert(readFromTree == readFromSource)

  val readFromMap =
    read(description from ConfigSource.fromMultiMap(written.flattenString(), "tree")).loadOrThrow

  assert(readFromMap == readFromSource)
}
