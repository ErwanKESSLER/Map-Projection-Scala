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

dockerAutoPackageJavaApplication()