val zioSbtVersion = "0.3.10+46-2b967db5-SNAPSHOT"

addSbtPlugin("dev.zio"      % "zio-sbt-ecosystem" % zioSbtVersion)
addSbtPlugin("dev.zio"      % "zio-sbt-website"   % zioSbtVersion)
addSbtPlugin("dev.zio"      % "zio-sbt-ci"        % zioSbtVersion)
addSbtPlugin("com.typesafe"                      % "sbt-mima-plugin"               % "1.1.0")

resolvers ++= Resolver.sonatypeOssRepos("public")
