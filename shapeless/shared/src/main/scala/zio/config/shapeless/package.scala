package zio.config

package object shapeless {
  type describe = derivation.describe
  val describe: derivation.describe.type = derivation.describe
  type name = derivation.name
  val name: derivation.name.type = derivation.name
}
