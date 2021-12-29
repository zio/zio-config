package zio.config.examples

import zio.IO
import zio.config._
import zio.config.examples.typesafe.EitherImpureOps

import ConfigDescriptor._

object WriteExample extends App with EitherImpureOps {

  final case class Id(int: Int)

  final case class B(c: Id, d: Id)
  final case class A(b: B, i: Int)

  val description: ConfigDescriptor[A] = {
    val bConfig = (int("c").to[Id] zip int("d").to[Id]).to[B]

    (bConfig zip int("i")).to[A]
  }

  val map: Map[String, String] =
    Map(
      "c" -> "1",
      "d" -> "2",
      "i" -> "3"
    )

  // loadOrThrow here is only for the purpose of example
  val readFromSource: A =
    read(description from ConfigSource.fromMap(map, "map")).unsafeRun

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

  val readFromTree: IO[ReadError[String], A] =
    read(description from ConfigSource.fromPropertyTree(written, "tree"))

  assert(readFromTree equalM readFromSource)

  val readFromMap: IO[ReadError[String], A] =
    read(description from ConfigSource.fromMultiMap(written.flattenString(), "tree"))

  assert(readFromMap equalM readFromSource)
}
