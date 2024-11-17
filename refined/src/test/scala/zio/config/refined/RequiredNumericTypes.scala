package zio.config.refined

private[refined] trait RequiredNumericTypes { this: NumericTestTypes =>
  type Less10
  type Greater10
  type GreaterOrEqual10
  type LessOrEqual10
  type DivisibleBy10
  type NonDivisibleBy10
}
