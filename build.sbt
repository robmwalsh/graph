name := "graph"

version := "0.1"

val zioVersion = "1.0.1"

scalaVersion := "2.13.2"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio-streams" % "1.0.0",
  "com.github.ghostdogpr" %% "caliban" % "0.7.6",
  "dev.zio" %% "zio-test"          % zioVersion % "test",
  "dev.zio" %% "zio-test-sbt"      % zioVersion % "test",
  //"dev.zio" %% "zio-test-intellij"      % zioVersion % "test"
)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

scalacOptions := Seq("-Xfatal-warnings")
bloopExportJarClassifiers in Global := Some(Set("sources"))
