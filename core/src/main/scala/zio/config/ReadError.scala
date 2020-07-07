package zio.config

sealed trait ReadError[A] extends Exception { self =>

  def is(f: PartialFunction[ReadError[A], Boolean]): Boolean =
    self match {
      case ReadError.OrErrors(list)     => list.forall(_.is(f))
      case ReadError.AndErrors(list, _) => list.forall(_.is(f))
      case ReadError.ListErrors(list)   => list.forall(_.is(f))
      case ReadError.MapErrors(list)    => list.forall(_.is(f))
      case a                            => f.applyOrElse(a, (_: ReadError[A]) => false)
    }

  def allMissingValues: Boolean = is {
    case ReadError.MissingValue(_, _) => true
  }

  /**
   * Returns a `String` with the ReadError pretty-printed.
   */
  final def prettyPrint(keyDelimiter: Char = '.'): String = {

    sealed trait Segment
    sealed trait Step extends Segment

    final case class Sequential(all: List[Step])     extends Segment
    final case class Parallel(all: List[Sequential]) extends Step
    final case class Failure(lines: List[String])    extends Step

    def renderSteps[A](steps: List[ReadError.Step[A]]): String =
      steps
        .foldLeft(new StringBuilder()) {
          case (r, ReadError.Step.Key(k))   => r.append(keyDelimiter).append(k.toString)
          case (r, ReadError.Step.Index(i)) => r.append('[').append(i).append(']')
        }
        .delete(0, 1)
        .toString()

    def prefixBlock(values: List[String], p1: String, p2: String): List[String] =
      values match {
        case Nil => Nil
        case head :: tail =>
          (p1 + head) :: tail.map(p2 + _)
      }

    def parallelSegments[A](readError: ReadError[A]): List[Sequential] =
      readError match {
        case ReadError.AndErrors(head :: tail, _)  => parallelSegments(head) ++ tail.flatMap(parallelSegments)
        case ReadError.ListErrors(head :: tail)    => parallelSegments(head) ++ tail.flatMap(parallelSegments)
        case ReadError.MapErrors(head :: tail)     => parallelSegments(head) ++ tail.flatMap(parallelSegments)
        case ReadError.Irrecoverable(head :: tail) => parallelSegments(head) ++ tail.flatMap(parallelSegments)
        case _                                     => List(readErrorToSequential(readError))
      }

    def linearSegments[A](readError: ReadError[A]): List[Step] =
      readError match {
        case ReadError.OrErrors(head :: tail) => linearSegments(head) ++ tail.flatMap(linearSegments)
        case _                                => readErrorToSequential(readError).all
      }

    def renderMissingValue[A](err: ReadError.MissingValue[A]): Sequential = {
      val strings =
        "MissingValue" :: s"path: ${renderSteps(err.path)}" :: Nil

      Sequential(
        err.detail match {
          case ::(head, next) =>
            List(Failure(strings :+ s"Details: ${(head :: next).mkString(", ")}"))
          case Nil =>
            List(Failure(strings))
        }
      )
    }

    def renderFormatError[A](err: ReadError.FormatError[A]): Sequential =
      Sequential(
        List(
          Failure(
            "FormatError" :: s"cause: ${err.message}" :: s"path: ${renderSteps(err.path)}" :: Nil
          )
        )
      )

    def renderConversionError[A](err: ReadError.ConversionError[A]): Sequential =
      Sequential(
        List(
          Failure(
            "ConversionError" :: s"cause: ${err.message}" :: s"path: ${renderSteps(err.path)}" :: Nil
          )
        )
      )

    def readErrorToSequential[A](readError: ReadError[A]): Sequential =
      readError match {
        case r: ReadError.MissingValue[A]    => renderMissingValue(r)
        case r: ReadError.FormatError[A]     => renderFormatError(r)
        case r: ReadError.ConversionError[A] => renderConversionError(r)
        case t: ReadError.OrErrors[A]        => Sequential(linearSegments(t))
        case b: ReadError.AndErrors[A]       => Sequential(List(Parallel(parallelSegments(b))))
        case b: ReadError.ListErrors[A]      => Sequential(List(Parallel(parallelSegments(b))))
        case b: ReadError.MapErrors[A]       => Sequential(List(Parallel(parallelSegments(b))))
        case b: ReadError.Irrecoverable[A]   => Sequential(List(Parallel(parallelSegments(b))))
      }

    def format(segment: Segment): List[String] =
      segment match {
        case Failure(lines) =>
          prefixBlock(lines, "─", " ")
        case Parallel(all) =>
          List(("══╦" * (all.size - 1)) + "══╗") ++
            all.foldRight[List[String]](Nil) {
              case (current, acc) =>
                prefixBlock(acc, "  ║", "  ║") ++
                  prefixBlock(format(current), "  ", "  ")
            }
        case Sequential(all) =>
          all.flatMap { segment =>
            List("║") ++
              prefixBlock(format(segment), "╠", "║")
          } ++ List("▼")
      }

    val sequence = readErrorToSequential(self)

    ("ReadError:" :: {
      sequence match {
        // use simple report for single failures
        case Sequential(List(Failure(readError))) => readError

        case _ => format(sequence).updated(0, "╥")
      }
    }).mkString(System.lineSeparator())
  }

  def productTerms: Int =
    self match {
      case ReadError.AndErrors(list, _) => list.map(_.productTerms).sum
      case _                            => 1
    }

  def cardinality: Int =
    self match {
      case ReadError.MissingValue(_, _)    => 1
      case ReadError.FormatError(_, _)     => 1
      case ReadError.ConversionError(_, _) => 1
      case ReadError.OrErrors(list)        => list.map(_.size).sum
      case ReadError.AndErrors(list, _)    => list.map(_.size).sum
      case ReadError.ListErrors(_)         => 1
      case ReadError.MapErrors(_)          => 1
      case ReadError.Irrecoverable(_)      => 1
    }

  def size: Int =
    self match {
      case ReadError.MissingValue(_, _)    => 1
      case ReadError.FormatError(_, _)     => 1
      case ReadError.ConversionError(_, _) => 1
      case ReadError.OrErrors(list)        => list.map(_.size).sum
      case ReadError.AndErrors(list, _)    => list.map(_.size).sum
      case ReadError.ListErrors(list)      => list.map(_.size).sum
      case ReadError.MapErrors(list)       => list.map(_.size).sum
      case ReadError.Irrecoverable(list)   => list.map(_.size).sum
    }

  def sumTerms: Int =
    self match {
      case ReadError.OrErrors(list) => list.map(_.sumTerms).sum
      case _                        => 1
    }

  override def toString: String = self match {
    case ReadError.MissingValue(path, message)    => s"MissingValue(${path}, ${message})"
    case ReadError.FormatError(path, message)     => s"FormatError(${path}, ${message})"
    case ReadError.ConversionError(path, message) => s"ConversionError(${path},${message}"
    case ReadError.OrErrors(list)                 => s"OrErrors(${list.map(_.toString)})"
    case ReadError.AndErrors(list, fallBack) =>
      s"AndErrors(${list.map(t => t.toString)}, appliedAnyFallBacks = ${fallBack})"
    case ReadError.ListErrors(list)    => s"ListErrors(${list.map(_.toString)})"
    case ReadError.MapErrors(list)     => s"MapErrors(${list.map(_.toString)})"
    case ReadError.Irrecoverable(list) => s"Irrecoverable(${list.map(_.toString)})"
  }
}

object ReadError {
  sealed trait Step[+K]

  object Step {
    final case class Key[+K](key: K)   extends Step[K]
    final case class Index(index: Int) extends Step[Nothing]
  }

  final case class MissingValue[A](path: List[Step[A]], detail: List[String] = Nil)            extends ReadError[A]
  final case class FormatError[A](path: List[Step[A]], message: String)                        extends ReadError[A]
  final case class ConversionError[A](path: List[Step[A]], message: String)                    extends ReadError[A]
  final case class Irrecoverable[A](list: List[ReadError[A]])                                  extends ReadError[A]
  final case class OrErrors[A](list: List[ReadError[A]])                                       extends ReadError[A]
  final case class AndErrors[A](list: List[ReadError[A]], anyNonDefaultValue: Boolean = false) extends ReadError[A]
  final case class ListErrors[A](list: List[ReadError[A]])                                     extends ReadError[A]
  final case class MapErrors[A](list: List[ReadError[A]])                                      extends ReadError[A]

  def partitionWith[K, V, A](
    trees: List[ReadError[V]]
  )(pf: PartialFunction[ReadError[V], A]): List[A] =
    trees.collect {
      case tree if pf.isDefinedAt(tree) => pf(tree) :: Nil
    }.foldLeft((List.empty[A])) {
      case (accLeft, left) => (accLeft ++ left)
    }
}
