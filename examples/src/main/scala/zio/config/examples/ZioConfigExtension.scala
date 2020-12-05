package zio.config.examples

import zio.config._

trait ZioConfigExtension {
  // Should be part of ConfigDescriptorModule, and it isn't tested extensively. Just ran a couple of scenarios.

  implicit class SourceConfigOps[A](config: ConfigDescriptor[A]) {
    import ConfigDescriptorAdt._

    def updateSourceForEachKey(f: List[(String => String, ConfigSource)]) =
      f.foldRight(config) { (b, a) =>
        def loop[B](config: ConfigDescriptor[B]): ConfigDescriptor[B] =
          config match {
            case Lazy(thunk)                             => Lazy(() => loop(thunk()))
            case existing @ Source(source, propertyType) => Source(b._2, propertyType)
            case DynamicMap(source, conf)                => DynamicMap(source, loop(conf))
            case exiting @ Nested(source, path, conf) =>
              exiting orElse Nested(source, b._1(path), loop(conf)) orElse Nested(b._2, b._1(path), loop(conf))
            case Optional(conf)          => Optional(loop(conf))
            case Sequence(source, conf)  => Sequence(source, loop(conf))
            case Describe(conf, message) => Describe(loop(conf), message)
            case Default(conf, value)    => Default(loop(conf), value)
            case TransformOrFail(conf, f, g) =>
              TransformOrFail(loop(conf), f, g)
            case Zip(conf1, conf2) => Zip(loop(conf1), loop(conf2))
            case OrElseEither(conf1, conf2) =>
              OrElseEither(loop(conf1), loop(conf2))
            case OrElse(value1, value2) =>
              OrElse(loop(value1), loop(value2))
          }

        loop(a)
      }
  }
}
