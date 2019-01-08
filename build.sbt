lazy val commonSettings = Seq(
  version := "0.1-SNAPSHOT",
  organization := "com.top12",
  scalaVersion := "2.12.8",
  test in assembly := {},
  mainClass in assembly := Some("com.top12.Main"),
  assemblyJarName in assembly := "mapProjection.jar",
)
libraryDependencies += "org.scala-lang" % "scala-swing" % "2.10+"
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