name := "scalaFP"

version := "1.0"

scalaVersion in ThisBuild := "2.12.1"

scalacOptions in ThisBuild ++= Seq(
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-Ypartial-unification"
  // "-Xlog-implicits"
)

lazy val meta = (project in file("meta")).settings(
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
)

lazy val core = (project in file("base")).settings(
  libraryDependencies += compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")
) dependsOn meta
