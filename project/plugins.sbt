val zioSbtVersion = "0.3.10+43-080b598b-SNAPSHOT"

addSbtPlugin("dev.zio"      % "zio-sbt-ecosystem" % zioSbtVersion)
addSbtPlugin("dev.zio"      % "zio-sbt-website"   % zioSbtVersion)
addSbtPlugin("dev.zio"      % "zio-sbt-ci"        % zioSbtVersion)
addSbtPlugin("com.typesafe"                      % "sbt-mima-plugin"               % "1.1.0")

resolvers ++= Resolver.sonatypeOssRepos("public")
