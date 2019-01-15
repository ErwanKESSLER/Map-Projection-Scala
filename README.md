First Install Java 8 as usual

Then install sbt : https://www.scala-sbt.org/1.0/docs/Setup.html use 1.2.7 at least

Then run in shell in the root folder: 
```sh
sbt clean assembly
```

Then run in shell in the root folder:
```sh 
java -jar ./target/scala-2.12/projet_top_12-assembly-1.0-STABLE.jar
```

Enjoy!

Made by Erwan KESSLER, Victor COUR and Camille COUE


Scala versions defined here is 2.12.8 (no need to download it the SBT toolchain does everything just relax)



To run on docker pls first run on a POSIX (with X11) environment: 
```sh
sbt clean docker
```
then run : 
```sh
docker run -v /tmp/.X11-unix:/tmp/.X11-unix -e DISPLAY com.top12/projet_top_12
```