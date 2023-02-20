package zio.config.derivation

import scala.annotation.StaticAnnotation

final case class describe(describe: String) extends StaticAnnotation
final case class name(name: String)         extends StaticAnnotation

/**
 * nameWithLabel can be used for class names, such that the name of the class
 * should be part of the product with keyName as `keyName`.
 *
 * Example:
 *      {{{
 *       @nameWithLabel("type")
 *       sealed trait FooBar
 *       case class Bar(x: Int) extends FooBar
 *       case class Foo(y: String) extends FooBar
 *       case object Fooz extends FooBar
 *
 *       case class AppConfig(config :  FooBar)
 *      }}}
 *
 * corresponds to
 *
 *  {{{
 *    config : {
 *      type : Bar
 *      x : Int
 *     }
 *  }}}
 *
 * or
 *
 *  {{{
 *
 *    config : {
 *      type: Foo
 *      x: Int
 *     }
 *
 *  }}}
 *
 * or
 *
 * {{{
 *
 *   config : Fooz
 *
 * }}}
 *
 * If annotation is `name` instead of `nameWithLabel`, then name of the case class becomes a parent node
 *
 *  {{{
 *    Foo : {
 *     x : Int
 *    }
 *  }}}
 */
final case class nameWithLabel(keyName: String = "type") extends StaticAnnotation
