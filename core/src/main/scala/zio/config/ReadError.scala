package zio.config

import zio.config.ReadError.Step

sealed trait ReadError[A] extends Exception { self =>
  def fold[B](default: B)(f: PartialFunction[ReadError[A], B])(g: (B, B) => B, zero: B): B = {
    def go(list: List[ReadError[A]]): B =
      list.foldLeft(zero)((a, b) => g(b.fold(default)(f)(g, zero), a))

    self match {
      case e @ ReadError.MissingValue(_, _)    => f.applyOrElse(e, (_: ReadError[A]) => default)
      case e @ ReadError.FormatError(_, _, _)  => f.applyOrElse(e, (_: ReadError[A]) => default)
      case e @ ReadError.ConversionError(_, _) => f.applyOrElse(e, (_: ReadError[A]) => default)
      case e @ ReadError.Irrecoverable(list)   => f.applyOrElse(e, (_: ReadError[A]) => go(list))
      case e @ ReadError.OrErrors(list)        => f.applyOrElse(e, (_: ReadError[A]) => go(list))
      case e @ ReadError.ZipErrors(list, _)    => f.applyOrElse(e, (_: ReadError[A]) => go(list))
      case e @ ReadError.ListErrors(list)      => f.applyOrElse(e, (_: ReadError[A]) => go(list))
      case e @ ReadError.MapErrors(list)       => f.applyOrElse(e, (_: ReadError[A]) => go(list))
    }
  }

  def getListOfSteps: List[List[Step[A]]] =
    fold(List.empty[List[Step[A]]]) {
      case ReadError.MissingValue(steps, _) => List(steps)
    }(_ ++ _, List.empty[List[Step[A]]])

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
        case ReadError.ZipErrors(head :: tail, _)  => parallelSegments(head) ++ tail.flatMap(parallelSegments)
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

    def renderFormatError[A](err: ReadError.FormatError[A]): Sequential = {
      val strings =
        "FormatError" :: s"cause: ${err.message}" :: s"path: ${renderSteps(err.path)}" :: Nil

      Sequential(
        err.detail match {
          case ::(head, next) =>
            List(Failure(strings :+ s"Details: ${(head :: next).mkString(", ")}"))
          case Nil =>
            List(Failure(strings))
        }
      )
    }

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
        case b: ReadError.ZipErrors[A]       => Sequential(List(Parallel(parallelSegments(b))))
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

  def sizeOfZipAndOrErrors: Int =
    self match {
      case ReadError.MissingValue(_, _)    => 1
      case ReadError.FormatError(_, _, _)  => 1
      case ReadError.ConversionError(_, _) => 1
      case ReadError.OrErrors(list)        => list.map(_.size).sum
      case ReadError.ZipErrors(list, _)    => list.map(_.size).sum
      case ReadError.ListErrors(_)         => 1
      case ReadError.MapErrors(_)          => 1
      case ReadError.Irrecoverable(_)      => 1
    }

  def size: Int =
    self match {
      case ReadError.MissingValue(_, _)    => 1
      case ReadError.FormatError(_, _, _)  => 1
      case ReadError.ConversionError(_, _) => 1
      case ReadError.OrErrors(list)        => list.map(_.size).sum
      case ReadError.ZipErrors(list, _)    => list.map(_.size).sum
      case ReadError.ListErrors(list)      => list.map(_.size).sum
      case ReadError.MapErrors(list)       => list.map(_.size).sum
      case ReadError.Irrecoverable(list)   => list.map(_.size).sum
    }

  override def toString: String = self match {
    case ReadError.MissingValue(path, message)        => s"MissingValue(${path}, ${message})"
    case ReadError.FormatError(path, message, detail) => s"FormatError(${path}, ${message}, ${detail})"
    case ReadError.ConversionError(path, message)     => s"ConversionError(${path},${message}"
    case ReadError.OrErrors(list)                     => s"OrErrors(${list.map(_.toString)})"
    case ReadError.ZipErrors(list, fallBack) =>
      s"ZipErrors(${list.map(t => t.toString)}, appliedAnyFallBacks = ${fallBack})"
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

  final case class MissingValue[A](path: List[Step[A]], detail: List[String] = Nil)                 extends ReadError[A]
  final case class FormatError[A](path: List[Step[A]], message: String, detail: List[String] = Nil) extends ReadError[A]
  final case class ConversionError[A](path: List[Step[A]], message: String)                         extends ReadError[A]
  final case class Irrecoverable[A](list: List[ReadError[A]])                                       extends ReadError[A]
  final case class OrErrors[A](list: List[ReadError[A]])                                            extends ReadError[A]
  final case class ZipErrors[A](list: List[ReadError[A]], anyNonDefaultValue: Boolean = false)      extends ReadError[A]
  final case class ListErrors[A](list: List[ReadError[A]])                                          extends ReadError[A]
  final case class MapErrors[A](list: List[ReadError[A]])                                           extends ReadError[A]
}
