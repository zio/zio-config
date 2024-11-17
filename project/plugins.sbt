val zioSbtVersion = "0.0.0+529-d9aba4fa-SNAPSHOT"

addSbtPlugin("ch.epfl.scala"      % "sbt-bloop"         % "2.0.5")
addSbtPlugin("pl.project13.scala" % "sbt-jcstress"      % "0.2.0")
addSbtPlugin("pl.project13.scala" % "sbt-jmh"           % "0.4.7")
addSbtPlugin("nl.thijsbroersen"   % "zio-sbt-ci"        % zioSbtVersion)
addSbtPlugin("nl.thijsbroersen"   % "zio-sbt-ecosystem" % zioSbtVersion)
addSbtPlugin("nl.thijsbroersen"   % "zio-sbt-website"   % zioSbtVersion)

resolvers ++= Resolver.sonatypeOssRepos("snapshots")
