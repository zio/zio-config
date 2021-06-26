package zio.config.magnolia

import zio.config._

case class Descriptor[T](desc: ConfigDescriptor[T])

object Descriptor extends DeriveConfigDescriptor {
  def apply[A](implicit ev: Descriptor[A]): Descriptor[A] = ev

  sealed trait SealedTraitSubClassNameStrategy {
    def &&(
      sealedTraitNameStrategy: SealedTraitNameStrategy
    ): SealedTraitStrategy =
      SealedTraitStrategy(this, sealedTraitNameStrategy)
  }

  object SealedTraitSubClassNameStrategy {
    case object WrapSubClassName                    extends SealedTraitSubClassNameStrategy
    case object IgnoreSubClassName                  extends SealedTraitSubClassNameStrategy
    case class LabelSubClassName(fieldName: String) extends SealedTraitSubClassNameStrategy
  }

  sealed trait SealedTraitNameStrategy {
    def &&(
      subClassNameStrategy: SealedTraitSubClassNameStrategy
    ): SealedTraitStrategy =
      SealedTraitStrategy(subClassNameStrategy, this)
  }

  object SealedTraitNameStrategy {
    case object WrapSealedTraitName   extends SealedTraitNameStrategy
    case object IgnoreSealedTraitName extends SealedTraitNameStrategy
  }

  case class SealedTraitStrategy(
    subClass: SealedTraitSubClassNameStrategy,
    parentClass: SealedTraitNameStrategy
  )

  object SealedTraitStrategy {
    import SealedTraitNameStrategy._
    import SealedTraitSubClassNameStrategy._

    def wrapSealedTraitName: SealedTraitNameStrategy   = WrapSealedTraitName
    def ignoreSealedTraitName: SealedTraitNameStrategy = IgnoreSealedTraitName

    def wrapSubClassName: SealedTraitSubClassNameStrategy                     = WrapSubClassName
    def ignoreSubClassName: SealedTraitSubClassNameStrategy                   = IgnoreSubClassName
    def labelSubClassName(fieldName: String): SealedTraitSubClassNameStrategy = LabelSubClassName(fieldName)
  }
}
