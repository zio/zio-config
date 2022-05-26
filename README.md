# ZIO Config

| Project Stage |  CI | Release | Issues | Scaladex | Discord | Twitter |
| --- | --- | --- | --- | --- | --- | --- |
| [![Project stage][Badge-Stage]][Link-Stage-Page] | ![CI][Badge-CI] | [![Release Artifacts][Badge-SonatypeReleases]][Link-SonatypeReleases] | [![Average time to resolve an issue][Badge-IsItMaintained]][Link-IsItMaintained] | [![Badge-Scaladex-page]][Link-Scaladex-page] | [![Badge-Discord]][Link-Discord] | [![Badge-Twitter]][Link-Twitter] |

A [ZIO](https://github.com/zio/zio)-based library for working with application configuration data.

_ZIO Config_ offloads all parsing and file formats to other libraries, and just focuses on being the _interface_ to configuration data throughout an application.

Using a single definition of configuration requirements, which can be derived automatically from your data types, _ZIO Config_ offers a bundle of features for free:

 * Read flat or nested config data from any format, with descriptive errors
 * Write flat or nested config data into any format
 * Compose sources of configuration, so you can have, e.g., environmental or command-line overrides
 * Automatically generate documentation so devs / devops know how to configure the application
 * Generate a report that shows where each piece of configuration data came from

Please find more details in the [website](https://zio.github.io/zio-config/). _The website is currently for the zio-config version 1.x only. For newer versions, please check the markdown files in `docs` directory here in GitHub_.
For example, the docs for 3.x is available [here](https://github.com/zio/zio-config/tree/master/docs). The updated website will be published soon for the newer versions.

If you are only interested in automatic derviation of configuration, find the details [here](https://zio.github.io/zio-config/docs/automatic/automatic_index).

Jump to [examples](examples/shared/src/main/scala/zio/config/examples) to see various usecases. Hop over to the [Discord](https://discord.gg/2ccFBr4) #zio-config channel to chat with developers and other users.

Try out _ZIO Config_ quickly in [Scastie](https://scastie.scala-lang.org/WMlkdQeZQvm4yDyZ0pigJA), which comes pre-loaded with an example in scala-3. We try to make sure the scastie-buildsettings are updated with latest version of _ZIO Config_.

[Badge-CI]: https://github.com/zio/zio-config/workflows/CI/badge.svg "ci"
[Badge-IsItMaintained]: http://isitmaintained.com/badge/resolution/zio/zio-config.svg "Average time to resolve an issue"
[Badge-Discord]: https://img.shields.io/discord/629491597070827530?logo=discord "Chat on discord"
[Badge-Scaladex-page]: https://index.scala-lang.org/zio/zio-config/zio-config/latest.svg "Scaladex"
[Badge-SonatypeReleases]: https://img.shields.io/nexus/r/https/oss.sonatype.org/dev.zio/zio-config_2.12.svg "Sonatype Releases"
[Badge-Twitter]: https://img.shields.io/twitter/follow/zioscala.svg?style=plastic&label=follow&logo=twitter
[Badge-Stage]: https://img.shields.io/badge/Project%20Stage-Production%20Ready-brightgreen.svg

[Link-IsItMaintained]: http://isitmaintained.com/project/zio/zio-config "Average time to resolve an issue"
[Link-Discord]: https://discord.gg/2ccFBr4 "Discord"
[Link-Scaladex-page]: https://index.scala-lang.org/zio/zio-config/zio-config "Scaladex"
[Link-SonatypeReleases]: https://oss.sonatype.org/content/repositories/releases/dev/zio/zio-config_2.12/ "Sonatype Releases"
[Link-Twitter]: https://twitter.com/zioscala
[Link-Stage-Page]: https://github.com/zio/zio/wiki/Project-Stages
