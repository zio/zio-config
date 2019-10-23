---
id: read_index
title:  "Overview"
---

ZIO-Config is a small, unopiconfignated ZIO interface to Config. 

 - **[File Channel](files.md)** — For processing files that are available locally. For every operation a new fiber is started to perform operation
 - **[Socket Channel](sockets.md)** — Provides API for remote communication with `InetSocket`s 

## Installation

`ZIO-Config` is available via maven repo so import in `build.sbt` is sufficient:

```scala
libraryDependencies += "dev.zio" %% "zio-Config" % "0.1.3"
```

## References

 - [ZIO github page](http://github.com/zio/zio)
 - [Java Config docs](https://docs.oracle.com/javase/8/docs/api/java/Config/package-summary.html)
 - [Java Config wikipedia](https://en.wikipedia.org/wiki/Non-blocking_I/O_(Java))
