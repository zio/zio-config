package zio.config

import com.github.ghik.silencer.silent
import zio.config.PropertyTreePath.Step

import scala.util.Try
import scala.util.matching.Regex

final case class PropertyTreePath[K](path: Vector[Step[K]]) {
  def mapKeys(f: K => K): PropertyTreePath[K] =
    PropertyTreePath(path.map(_.map(f)))
}

@silent("Unused import")
object PropertyTreePath {
  import scala.collection.compat._
  import VersionSpecificSupport._

  sealed trait Step[+K] { self =>
    def map[K1 >: K, K2](f: K1 => K2): Step[K2] =
      self match {
        case Step.Index(n) => Step.Index(n)
        case Step.Key(k)   => Step.Key(f(k.asInstanceOf[K1]))
      }
  }

  object Step {
    private val pattern: Regex = """([a-zA-Z0-9 -@\-^-~]*)(\[([0-9])*\])?""".r.anchored

    def steps[K](s: String)(implicit IsString: String =:= K): Vector[Step[K]] =
      Step.pattern
        .findAllIn(s)
        .matchData
        .filter(_.group(0).nonEmpty)
        .toVector
        .flatMap { regexMatched =>
          val optionalKey   = Option(regexMatched.group(1)).flatMap(s => if (s.isEmpty) None else Some(s))
          val optionalValue = Option(regexMatched.group(3))
            .flatMap(s => if (s.isEmpty) None else Try(s.toInt).toOption)

          (optionalKey, optionalValue) match {
            case (Some(key), Some(value)) => Vector(Step.Key(key), Step.Index(value))
            case (None, Some(value))      => Vector(Step.Index(value))
            case (Some(key), None)        => Vector(Step.Key(IsString(key)))
            case (None, None)             => Vector.empty
          }
        }

    final case class Index(n: Int) extends Step[Nothing]
    final case class Key[K](k: K)  extends Step[K]
  }

  def $(path: String): PropertyTreePath[String] =
    PropertyTreePath(path.split('.').toVector.flatMap(str => Step.steps(str)))
}
