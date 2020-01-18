# ZIO Config

[![CircleCI](https://circleci.com/gh/zio/zio-config/tree/master.svg?style=svg)](https://circleci.com/gh/zio/zio-config/tree/master)
[![badge-discord]][link-discord]

A [ZIO](https://github.com/zio/zio)-based library for working with application configuration data.

_ZIO Config_ offloads all parsing and file formats to other libraries, and just focuses on being the _interface_ to configuration data throughout an application.

Using a single definition of configuration requirements, which can be derived automatically from your data types, _ZIO Config_ offers a bundle of features for free:

 * Read flat or nested config data from any format, with descriptive errors
 * Write flat or nested config data into any format
 * Compose sources of configuration, so you can have, e.g., environmental or command-line overrides
 * Automatically generate documentation so devs / devops know how to configure the application
 * Generate a report that shows where each piece of configuration data came from

Please find more details in the [website](https://zio.github.io/zio-config/).

Jump to [examples](examples/src/main/scala/zio/config/examples) to see various usecases. Hop over to the [Discord](https://discord.gg/2ccFBr4) #zio-config channel to chat with developers and other users.

Try out _ZIO Config_ quickly in [Scastie](https://scastie.scala-lang.org/afsalthaj/3ALODWLJQbWmFxqBJ2MYWA/90), which comes pre-loaded with an example. We try to make sure the scastie-buildsettings are updated with latest version of _ZIO Config_.

[badge-discord]: https://img.shields.io/discord/629491597070827530?logo=discord "Chat on discord"
[link-discord]: https://discord.gg/2ccFBr4 "Discord"
