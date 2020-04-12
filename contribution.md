## To run microsite 

## Guidelines to raising a PR
1. Please raise an issue before you raise a PR on the code blocks.
2. Tag the issue in PR description.
3. Contributions to documentation/website is always welcome and can be raised without tagging an issue.

## SBT

```scala
cd zio-config
sbt +compile
sbt runAllExamples
sbt +test

```

## BLOOP

```scala
cd zio-config
bloop projects
bloop compile zio-config
bloop test zio-config

```

We recommend you run the examples in zio-config using `sbt runAllExamples` making sure
that all assertions in the example holds true even after your change.

## Build website

```
cd zio-config
rm -r website/build/
./sbt ++2.12.10! docs/docusaurusCreateSite
cd ./website/build/zio-config/
npm run start

```
