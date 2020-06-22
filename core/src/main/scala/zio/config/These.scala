package zio.config

import These._

private[config] sealed trait These[+A, +B] { self =>
  def fold[C](
    f: (A, B) => C,
    g: A => C,
    h: B => C
  ): C = self match {
    case This(left)        => g(left)
    case That(right)       => h(right)
    case Both(left, right) => f(left, right)
  }
}

object These {
  final case class Both[A, B](left: A, right: B) extends These[A, B]
  final case class This[A](left: A)              extends These[A, Nothing]
  final case class That[B](right: B)             extends These[Nothing, B]
}
