package zio.config.refined

import eu.timepit.refined.W
import eu.timepit.refined.numeric.{Divisible, Greater, GreaterEqual, Less, LessEqual, NonDivisible}

private[refined] trait NumericTestTypes {
  type Less10           = Less[W.`10`.T]
  type Greater10        = Greater[W.`10`.T]
  type GreaterOrEqual10 = GreaterEqual[W.`10`.T]
  type LessOrEqual10    = LessEqual[W.`10`.T]
  type DivisibleBy10    = Divisible[W.`10`.T]
  type NonDivisibleBy10 = NonDivisible[W.`10`.T]
}
