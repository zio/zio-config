package zio.config

import _root_.scalaz.Isomorphism.<=>

package object scalaz extends ScalazInstances {
  def fromIso[A, B](D: A <=> B)(implicit M: ConfigDescriptor[A]): ConfigDescriptor[B] =
    M.transform[B](D.to, D.from)
}
