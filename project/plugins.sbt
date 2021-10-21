addSbtPlugin("ch.epfl.scala"                     % "sbt-bloop"                     % "1.4.10")
addSbtPlugin("ch.epfl.scala"                     % "sbt-scalafix"                  % "0.9.27")
addSbtPlugin("com.eed3si9n"                      % "sbt-buildinfo"                 % "0.10.0")
addSbtPlugin("com.eed3si9n"                      % "sbt-unidoc"                    % "0.4.3")
addSbtPlugin("com.geirsson"                      % "sbt-ci-release"                % "1.5.7")
addSbtPlugin("com.github.cb372"                  % "sbt-explicit-dependencies"     % "0.2.15")
addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings"              % "3.0.0")
addSbtPlugin("com.typesafe"                      % "sbt-mima-plugin"               % "0.9.2")
addSbtPlugin("de.heikoseeberger"                 % "sbt-header"                    % "5.6.0")
addSbtPlugin("org.portable-scala"                % "sbt-scala-native-crossproject" % "1.1.0")
addSbtPlugin("org.portable-scala"                % "sbt-scalajs-crossproject"      % "1.1.0")
addSbtPlugin("org.scala-js"                      % "sbt-scalajs"                   % "1.7.1")
addSbtPlugin("org.scala-native"                  % "sbt-scala-native"              % "0.4.1")
addSbtPlugin("org.scalameta"                     % "sbt-mdoc"                      % "2.2.24")
addSbtPlugin("org.scalameta"                     % "sbt-scalafmt"                  % "2.4.3")
addSbtPlugin("pl.project13.scala"                % "sbt-jcstress"                  % "0.2.0")
addSbtPlugin("pl.project13.scala"                % "sbt-jmh"                       % "0.4.3")

libraryDependencies += "org.snakeyaml" % "snakeyaml-engine" % "2.3"
