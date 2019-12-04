## To run microsite 

```
cd zio-config
rm -r website/build/
./sbt ++2.12.10! docs/docusaurusCreateSite
cd ./website/build/zio-config/
npm run start

```
