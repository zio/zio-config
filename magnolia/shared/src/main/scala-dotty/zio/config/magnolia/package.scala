package zio.config

package object magnolia {
  def descriptor[A](implicit ev: Descriptor[A]) =
    ev.desc
}
