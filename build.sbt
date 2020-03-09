name := "graph"

version := "0.1"

scalaVersion := "2.13.1"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio" % "1.0.0-RC18"
)

bloopExportJarClassifiers in Global := Some(Set("sources"))