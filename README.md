# ZIO Config

|  CI | Release | Issues | Scaladex | Discord | Twitter |
| --- | --- | --- | --- | --- | --- |
| [![Build Status][Badge-Circle]][Link-Circle] | [![Release Artifacts][Badge-SonatypeReleases]][Link-SonatypeReleases] | [![Average time to resolve an issue][Badge-IsItMaintained]][Link-IsItMaintained] | [![Badge-Scaladex-page]][Link-Scaladex-page] | [![Badge-Discord]][Link-Discord] | [![Badge-Twitter]][Link-Twitter] |

A [ZIO](https://github.com/zio/zio)-based library for working with application configuration data.

_ZIO Config_ offloads all parsing and file formats to other libraries, and just focuses on being the _interface_ to configuration data throughout an application.

Using a single definition of configuration requirements, which can be derived automatically from your data types, _ZIO Config_ offers a bundle of features for free:

 * Read flat or nested config data from any format, with descriptive errors
 * Write flat or nested config data into any format
 * Compose sources of configuration, so you can have, e.g., environmental or command-line overrides
 * Automatically generate documentation so devs / devops know how to configure the application
 * Generate a report that shows where each piece of configuration data came from

Please find more details in the [website](https://zio.github.io/zio-config/).

If you are only interested in automatic derviation of configuration, find the details [here](https://zio.github.io/zio-config/docs/automatic/automatic_index).

Jump to [examples](examples/src/main/scala/zio/config/examples) to see various usecases. Hop over to the [Discord](https://discord.gg/2ccFBr4) #zio-config channel to chat with developers and other users.

Try out _ZIO Config_ quickly in [Scastie](https://scastie.scala-lang.org/afsalthaj/3ALODWLJQbWmFxqBJ2MYWA/90), which comes pre-loaded with an example. We try to make sure the scastie-buildsettings are updated with latest version of _ZIO Config_.

[Badge-Circle]: https://circleci.com/gh/zio/zio-config.svg?style=svg "circleci"
[Badge-IsItMaintained]: http://isitmaintained.com/badge/resolution/zio/zio-config.svg "Average time to resolve an issue"
[Badge-Discord]: https://img.shields.io/discord/629491597070827530?logo=discord "Chat on discord"
[Badge-Scaladex-page]: https://index.scala-lang.org/zio/zio-config/zio-config/latest.svg "Scaladex"
[Badge-SonatypeReleases]: https://img.shields.io/nexus/r/https/oss.sonatype.org/dev.zio/zio-config_2.12.svg "Sonatype Releases"
[Badge-Twitter]: https://img.shields.io/twitter/follow/zioscala.svg?style=plastic&label=follow&logo=twitter

[Link-Circle]: https://circleci.com/gh/zio/zio-config "circleci"
[Link-IsItMaintained]: http://isitmaintained.com/project/zio/zio-config "Average time to resolve an issue"
[Link-Discord]: https://discord.gg/2ccFBr4 "Discord"
[Link-Scaladex-page]: https://index.scala-lang.org/zio/zio-config/zio-config "Scaladex"
[Link-SonatypeReleases]: https://oss.sonatype.org/content/repositories/releases/dev/zio/zio-config_2.12/ "Sonatype Releases"
[Link-Twitter]: https://twitter.com/zioscala

