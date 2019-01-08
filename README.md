
First Install Java 8 as usual

Then install sbt :https://www.scala-sbt.org/1.0/docs/Setup.html use 1.2.7 at least

Then run in shell in the root folder: sbt assembly

Then run in shell in the root folder: java -jar ./target/scala-2.12/projet_top_12-assembly-0.1.0-SNAPSHOT.jar

Enjoy!

Made by Erwan KESSLER, Victor COUR and Camille COUE


Scala versions defined here is 2.12.8 (no need to download it the SBT toolchain does everything just relax)