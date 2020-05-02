name := "graph"

version := "0.1"

scalaVersion := "2.13.2"

libraryDependencies ++= Seq(
  "dev.zio"               %% "zio"     % "1.0.0-RC18",
  "com.github.ghostdogpr" %% "caliban" % "0.7.6",
  "com.lihaoyi"           %% "pprint"  % "0.5.6"
)

scalacOptions := Seq("-Xfatal-warnings")
bloopExportJarClassifiers in Global := Some(Set("sources"))
