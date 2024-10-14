package zio.config.refined

import eu.timepit.refined.numeric.{Divisible, Greater, GreaterEqual, Less, LessEqual, NonDivisible}

private[refined] trait NumericTestTypes {
  type Less10           = Less[10]
  type Greater10        = Greater[10]
  type GreaterOrEqual10 = GreaterEqual[10]
  type LessOrEqual10    = LessEqual[10]
  type DivisibleBy10    = Divisible[10]
  type NonDivisibleBy10 = NonDivisible[10]
}
