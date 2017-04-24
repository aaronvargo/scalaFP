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
  libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-reflect" % scalaVersion.value,
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  )
)

lazy val base = (project in file("base")).settings(
  libraryDependencies ++= Seq(
    compilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
    compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
  )
) dependsOn meta
