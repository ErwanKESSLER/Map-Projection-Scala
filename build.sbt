version := "1.0-STABLE"
organization := "com.top12"
scalaVersion := "2.12.8"
mainClass in assembly := Some("com.top12.Main")

  
libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "2.1.0"
assemblyMergeStrategy in assembly := {
  case PathList("javax", "servlet", xs @ _*)         => MergeStrategy.first
  case PathList(ps @ _*) if ps.last endsWith ".html" => MergeStrategy.first
  case "application.conf"                            => MergeStrategy.concat
  case "unwanted.txt"                                => MergeStrategy.discard
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

enablePlugins(DockerPlugin)

dockerfile in docker := {
  // The assembly task generates a fat JAR file
  val artifact: File = assembly.value
  val artifactTargetPath = s"/app/${artifact.name}"

  new Dockerfile {
    from("openjdk:8-jre")
    add(artifact, artifactTargetPath)
    entryPoint("java", "-Xmx2G", "-jar", artifactTargetPath)
  }
}

buildOptions in docker := BuildOptions(cache = false)