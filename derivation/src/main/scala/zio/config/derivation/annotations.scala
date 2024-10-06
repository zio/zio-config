package zio.config.derivation

import scala.annotation.StaticAnnotation

final case class describe(describe: String) extends StaticAnnotation
final case class name(name: String)         extends StaticAnnotation

/**
 * discriminator can be used for class names, such that the name of the class should be part of the product with keyName
 * as `keyName`.
 *
 * Example:
 * {{{
 *       @discriminator("type")
 *       sealed trait FooBar
 *       case class Bar(x: Int) extends FooBar
 *       case class Foo(y: String) extends FooBar
 *       case object Fooz extends FooBar
 *
 *       case class AppConfig(config :  FooBar)
 * }}}
 *
 * corresponds to
 *
 * {{{
 *    config : {
 *      type : Bar
 *      x : Int
 *     }
 * }}}
 *
 * or
 *
 * {{{
 *
 *    config : {
 *      type: Foo
 *      x: Int
 *     }
 *
 * }}}
 *
 * or
 *
 * {{{
 *
 *   config : Fooz
 *
 * }}}
 *
 * If annotation is `name` instead of `discriminator`, then name of the case class becomes a parent node
 *
 * {{{
 *    Foo : {
 *     x : Int
 *    }
 * }}}
 */
final case class discriminator(keyName: String = "type") extends StaticAnnotation
final case class kebabCase()                             extends StaticAnnotation
final case class snakeCase()                             extends StaticAnnotation
final case class prefix(prefix: String)                  extends StaticAnnotation
final case class postfix(postfix: String)                extends StaticAnnotation
