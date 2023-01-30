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
 *      }}}
 *
 * corresponds to
 *
 *  {{{
 *     {
 *      type : Bar
 *      x : Int
 *     }
 *  }}}
 *
 * or
 *
 *  {{{
 *
 *     {
 *      type: Foo
 *      x: Int
 *     }
 *
 *  }}}
 *
 * If given name instead of `nameWithLabel`, then the config should be
 *
 *  {{{
 *    Foo : {
 *     x : Int
 *    }
 *  }}}
 */
final case class nameWithLabel(keyName: String = "type") extends StaticAnnotation
