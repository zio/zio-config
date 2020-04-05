package zio.config.examples.typesafe

import zio.config.{ ConfigSource, PropertyTree }
import zio.config.PropertyTree.{ Leaf, Record, Sequence }
import zio.config.examples.typesafe.CommandLineArgs.These.{ Both, That, This }

import scala.collection.immutable.Nil

object CommandLineArgs extends App {

  val argss =
    "--psql.database -username=1 --psql.database.password=hi --psql.database -url=jdbc://xyz --vault -username=3 --vault.password=10 --vault -something=11 --users 100 --region 111,122"

  def getPropertyTree(
    args: List[String],
    keyDelimiter: Option[Char],
    valueDelimiter: Option[Char]
  ): List[PropertyTree[String, String]] = {
    def unflatten(key: List[String], tree: PropertyTree[String, String]): PropertyTree[String, String] =
      key match {
        case ::(head, next) => Record(Map(head -> unflatten(next, tree)))
        case Nil            => tree
      }

    def unflattenWith(
      key: String,
      tree: PropertyTree[String, String]
    ): PropertyTree[String, String] =
      keyDelimiter.fold(Record(Map(key -> tree)): PropertyTree[String, String])(
        value => unflatten(key.split(value).toList, tree)
      )

    def toSeq[V](leaf: String): PropertyTree[String, String] =
      valueDelimiter.fold(Leaf(leaf): PropertyTree[String, String])(
        c => Sequence[String, String](leaf.split(c).toList.map(Leaf(_)))
      )

    def loop(args: List[String]): List[PropertyTree[String, String]] =
      args match {
        case h1 :: h2 :: h3 =>
          (KeyValue.mk(h1), KeyValue.mk(h2)) match {
            case (Some(keyValue1), Some(keyValue2)) =>
              (keyValue1, keyValue2) match {
                case (Both(l1, r1), Both(l2, r2)) =>
                  unflatten(l1.value.split('.').toList, toSeq(r1.value)) ::
                    unflattenWith(l2.value, toSeq(r2.value)) :: loop(h3)

                case (Both(l1, r1), This(l2)) =>
                  unflattenWith(l1.value, toSeq(r1.value)) :: h3.headOption.fold(
                    List.empty[PropertyTree[String, String]]
                  )(
                    x =>
                      loop(List(x)).map(
                        tree => unflattenWith(l2.value, tree)
                      ) ++ loop(h3.tail)
                  )

                case (Both(l1, r1), That(r2)) =>
                  unflattenWith(l1.value, toSeq(r1.value)) :: toSeq(r2.value) :: loop(h3)

                case (This(l1), Both(l2, r2)) =>
                  unflattenWith(l1.value, unflattenWith(l2.value, toSeq(r2.value))) :: loop(h3)

                case (This(l1), This(l2)) =>
                  loop(h3).map(tree => unflattenWith(l1.value, unflattenWith(l2.value, tree)))

                case (This(l1), That(r2)) =>
                  unflattenWith(l1.value, toSeq(r2.value)) :: loop(h3)

                case (That(r1), Both(l2, r2)) =>
                  toSeq(r1.value) :: unflattenWith(l2.value, toSeq(r2.value)) :: loop(h3)

                case (That(r1), That(r2)) =>
                  toSeq(r1.value) :: toSeq(r2.value) :: loop(h3)

                case (That(r1), This(l2)) =>
                  toSeq(r1.value) :: loop(h3).map(tree => unflattenWith(l2.value, tree))
              }

            case (Some(_), None) => loop(h1 :: h3)
            case (None, Some(_)) => loop(h2 :: h3)
            case (None, None)    => loop(h3)
          }

        case h1 :: Nil =>
          KeyValue.mk(h1) match {
            case Some(value) =>
              value match {
                case Both(left, right) =>
                  unflattenWith(left.value, toSeq(right.value)) :: Nil
                case This(l1) =>
                  toSeq(l1.value) :: Nil
                case That(value) =>
                  toSeq(value.value) :: Nil
              }
            case None => Nil
          }
        case Nil => Nil
      }

    loop(args)
  }

  final case class Value(value: String) extends AnyVal

  type KeyValue = These[Key, Value]

  object KeyValue {
    def mk(s: String): Option[KeyValue] = {
      val splitted = s.split('=').toList

      (splitted.headOption, splitted.lift(1)) match {
        case (Some(possibleKey), Some(possibleValue)) =>
          Key.mk(possibleKey) match {
            case Some(actualKey) => Some(Both(actualKey, Value(possibleValue)))
            case None            => Some(That(Value(possibleValue)))
          }
        case (None, Some(possibleValue)) =>
          Some(That(Value(possibleValue)))

        case (Some(possibleKey), None) =>
          Key.mk(possibleKey) match {
            case Some(value) => Some(This(value))
            case None        => Some(That(Value(possibleKey)))
          }

        case (None, None) => None
      }
    }
  }

  private[config] class Key private (val value: String) extends AnyVal {
    override def toString: String = value
  }

  object Key {
    def mk(s: String): Option[Key] =
      if (s.startsWith("--")) {
        Some(new Key(s.replace("--", "")))
      } else if (s.startsWith("-")) {
        Some(new Key(s.replace("-", "")))
      } else
        None
  }

  sealed trait These[+A, +B] { self =>
    override def toString: String = self match {
      case Both(left, right) => s"Both(${left}, ${right})"
      case This(left)        => s"This(${left})"
      case That(right)       => s"That(${right})"
    }
  }

  object These {
    final case class Both[A, B](left: A, right: B) extends These[A, B]
    final case class This[A](left: A)              extends These[A, Nothing]
    final case class That[B](right: B)             extends These[Nothing, B]
  }

  def fromCommandLineArgs(
    args: Array[String],
    keyDelimiter: Option[Char],
    valueDelimiter: Option[Char]
  ): ConfigSource[String, String] = {
    println("the tree is " + getPropertyTree(args.toList, keyDelimiter, valueDelimiter))
    ConfigSource.fromPropertyTrees(
      getPropertyTree(args.toList.filter(_.nonEmpty), keyDelimiter, valueDelimiter),
      "command line args"
    )
  }

  //  List(Record(Map(conf -> Record(Map(k3 -> Leaf(v3), k2 -> Leaf(v2))), k1 -> Leaf(v1))))

  val source = fromCommandLineArgs(argss.split(' '), Some('.'), Some(','))

  println(source.getConfigValue(Vector("conf", "k2")))

  import zio.config._, ConfigDescriptor._

  final case class UserPassword(k2: String, k3: String)

  object UserPassword {
    val desc = (string("username") |@| string("password"))(UserPassword.apply, UserPassword.unapply)
  }

  final case class DatabaseConfig(conf: UserPassword, url: String)

  object DatabaseConfig {
    val desc = nested("database") {
      (UserPassword.desc |@| string("url"))(DatabaseConfig.apply, DatabaseConfig.unapply)
    }
  }

  final case class VaultConfig(userPassword: UserPassword)

  object VaultConfig {
    val desc =
      nested("vault") {
        UserPassword.desc
      }(VaultConfig.apply, VaultConfig.unapply)
  }

  final case class AppConfig(databaseConfig: DatabaseConfig, vault: VaultConfig, users: String, region: List[String])

  object AppConfig {
    val desc: ConfigDescriptor[String, String, AppConfig] =
      (nested("psql") { DatabaseConfig.desc } |@| VaultConfig.desc |@| string("users") |@| list(string("region")))(
        AppConfig.apply,
        AppConfig.unapply
      )
  }

  println(
    read(AppConfig.desc from (source))
  )
}
