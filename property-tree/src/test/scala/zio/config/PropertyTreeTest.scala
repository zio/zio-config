package zio.config

import zio.config.PropertyTree._

import scala.collection.immutable.Nil

object PropertyTreeTest extends App {

  val map =
    Map(
      "key"  -> "name",
      "valu" -> "bla",
      "sel"  -> "ri"
    )

  val result = PropertyTree.fromStringMap(map, ':', ':')

  println(result)

}

object PropertyTreeTest2 extends App {

  def fromMap(
    map: Map[String, String],
    pathDelimiter: Char = '.',
    valueDelimter: Char = ','
  ): ConfigSource[String, String] =
    fromMapA(map)(x => { val s = x.split(valueDelimter).toList; ::(s.head, s.tail) }, pathDelimiter)

  def fromMultiMap(map: Map[String, ::[String]], pathDelimiter: Char = '.'): ConfigSource[String, String] =
    fromMapA(map)(identity, pathDelimiter)

  final case class ConfigSource[K, V](
    getConfigValue: Vector[K] => Option[PropertyTree[K, V]],
    sourceDescription: List[String]
  ) { self =>
    final def orElse(that: => ConfigSource[K, V]): ConfigSource[K, V] =
      ConfigSource(
        k => getConfigValue(k).orElse(that.getConfigValue(k)),
        self.sourceDescription ++ that.sourceDescription
      )

    final def <>(that: => ConfigSource[K, V]): ConfigSource[K, V] =
      self orElse that
  }

  private def fromMapA[A, B](map: Map[String, A])(f: A => ::[B], pathDelimiter: Char): ConfigSource[String, B] =
    getConfigSource(
      unflatten(
        map.map(
          tuple =>
            tuple._1.split(pathDelimiter).toVector.filterNot(_.trim == "") ->
              f(tuple._2)
        )
      ),
      pathDelimiter,
      "constant map"
    )

  def getConfigSource[B](
    list: List[PropertyTree[String, B]],
    keyDelimiter: Char,
    sourceName: String
  ): ConfigSource[String, B] =
    ConfigSource(
      (path: Vector[String]) => {
        list
          .map(tree => tree.getPath(path.toList))
          .collectFirst {
            case Some(tree) => tree
          }
      },
      sourceName :: Nil
    )

  val simpleMap =
    Map(
      "c" -> "a",
      "b" -> "b",
      "d" -> ""
    )

  val source                                        = fromMap(simpleMap, '.', ',')
  val result1: Option[PropertyTree[String, String]] = source.getConfigValue(Vector("c"))
  val result2: Option[PropertyTree[String, String]] = source.getConfigValue(Vector("b"))

  println(source.getConfigValue(Vector("c")).get)

  println(PropertyTree.sequence(source.getConfigValue(Vector("c")).get))

  assert(PropertyTree.sequence(source.getConfigValue(Vector("c")).get) == Some(Leaf(List("a"))))
  assert(PropertyTree.sequence(source.getConfigValue(Vector("b")).get) == Some(Leaf(List("b"))))
  assert(PropertyTree.sequence(source.getConfigValue(Vector("d")).get) == Some(Leaf(List(""))))

  val complexMap =
    Map(
      "a.c" -> "a,b,c",
      "a.b" -> "x,y,z",
      "a.x" -> "x",
      "a.d" -> "y",
      "a.e" -> ""
    )

  val complexSource =
    fromMap(complexMap, '.', ',')

  assert(
    PropertyTree.sequence(complexSource.getConfigValue(Vector("a")).get) ==
      Some(
        Record(
          Map(
            "c" -> Leaf(List("a", "b", "c")),
            "b" -> Leaf(List("x", "y", "z")),
            "x" -> Leaf(List("x")),
            "d" -> Leaf(List("y")),
            "e" -> Leaf(List(""))
          )
        )
      )
  )

  assert(PropertyTree.sequence(complexSource.getConfigValue(Vector("a", "b")).get) == Some(Leaf(List("x", "y", "z"))))
  assert(PropertyTree.sequence(complexSource.getConfigValue(Vector("a", "c")).get) == Some(Leaf(List("a", "b", "c"))))
  assert(PropertyTree.sequence(complexSource.getConfigValue(Vector("a", "x")).get) == Some(Leaf(List("x"))))
  assert(PropertyTree.sequence(complexSource.getConfigValue(Vector("a", "e")).get) == Some(Leaf(List(""))))

  /**
   * our logic doesn't handle this below scenario.
   *
   *
  val moreComplexMap =
    Map(
      "a.b"         -> "a,b,c",
      "a.b.c"       -> "x,y,z",
      "a.b.c.d"     -> "x",
      "a.b.c.d.e"   -> "y",
      "a.b.c.d.e.f" -> ""
    )

  val moreComplexSource =
    fromMap(complexMap, '.', ',')

  println(PropertyTree.sequence(moreComplexSource.getConfigValue(Vector("a")).get))
  println(moreComplexSource.getConfigValue(Vector("a")).get)

  assert(
    PropertyTree.sequence(moreComplexSource.getConfigValue(Vector("a")).get) ==
      Some(
        Record(
          Map(
            "b" -> Leaf(List("a", "b", "c"))
          )
        )
      )
  )
   */
  val propertyTree1: Option[PropertyTree[String, Either[String, String]]] =
    complexSource.getConfigValue(Vector("a")).map(_.map(_ => Left("failed"): Either[String, String]))

  val propertyTree2: Option[PropertyTree[String, Either[String, String]]] =
    complexSource.getConfigValue(Vector("a")).map(_.map(x => Right(x): Either[String, String]))

  assert(
    PropertyTree.orElseEither(propertyTree2.get, propertyTree1.get)((a, b) => a + " or " + b) ==
      Some(
        Record(
          Map(
            "e" -> Sequence(List(Leaf(Right(Left(""))))),
            "x" -> Sequence(List(Leaf(Right(Left("x"))))),
            "b" -> Sequence(List(Leaf(Right(Left("x"))), Leaf(Right(Left("y"))), Leaf(Right(Left("z"))))),
            "c" -> Sequence(List(Leaf(Right(Left("a"))), Leaf(Right(Left("b"))), Leaf(Right(Left("c"))))),
            "d" -> Sequence(List(Leaf(Right(Left("y")))))
          )
        )
      )
  )

  assert(
    PropertyTree.orElseEither(propertyTree1.get, propertyTree1.get)((a, b) => a + " or " + b) ==
      Some(
        Record(
          Map(
            "e" -> Sequence(List(Leaf(Left("failed or failed")))),
            "x" -> Sequence(List(Leaf(Left("failed or failed")))),
            "b" -> Sequence(
              List(Leaf(Left("failed or failed")), Leaf(Left("failed or failed")), Leaf(Left("failed or failed")))
            ),
            "c" -> Sequence(
              List(Leaf(Left("failed or failed")), Leaf(Left("failed or failed")), Leaf(Left("failed or failed")))
            ),
            "d" -> Sequence(List(Leaf(Left("failed or failed"))))
          )
        )
      )
  )

  assert(
    PropertyTree.orElseEither(propertyTree1.get, propertyTree2.get)((a, b) => a + " or " + b) ==
      Some(
        Record(
          Map(
            "e" -> Sequence(List(Leaf(Right(Right(""))))),
            "x" -> Sequence(List(Leaf(Right(Right("x"))))),
            "b" -> Sequence(List(Leaf(Right(Right("x"))), Leaf(Right(Right("y"))), Leaf(Right(Right("z"))))),
            "c" -> Sequence(List(Leaf(Right(Right("a"))), Leaf(Right(Right("b"))), Leaf(Right(Right("c"))))),
            "d" -> Sequence(List(Leaf(Right(Right("y")))))
          )
        )
      )
  )

  assert(
    PropertyTree.orElse(propertyTree1.get, propertyTree2.get)((a, b) => a + " or " + b) ==
      Some(
        Record(
          Map(
            "e" -> Sequence(List(Leaf(Right("")))),
            "x" -> Sequence(List(Leaf(Right("x")))),
            "b" -> Sequence(List(Leaf(Right("x")), Leaf(Right("y")), Leaf(Right("z")))),
            "c" -> Sequence(List(Leaf(Right("a")), Leaf(Right("b")), Leaf(Right("c")))),
            "d" -> Sequence(List(Leaf(Right("y"))))
          )
        )
      )
  )

  assert(
    PropertyTree.orElse(propertyTree1.get, propertyTree1.get)((a, b) => a + " or " + b) ==
      Some(
        Record(
          Map(
            "e" -> Sequence(List(Leaf(Left("failed or failed")))),
            "x" -> Sequence(List(Leaf(Left("failed or failed")))),
            "b" -> Sequence(
              List(Leaf(Left("failed or failed")), Leaf(Left("failed or failed")), Leaf(Left("failed or failed")))
            ),
            "c" -> Sequence(
              List(Leaf(Left("failed or failed")), Leaf(Left("failed or failed")), Leaf(Left("failed or failed")))
            ),
            "d" -> Sequence(List(Leaf(Left("failed or failed"))))
          )
        )
      )
  )
}
