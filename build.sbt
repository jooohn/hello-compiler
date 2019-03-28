lazy val catsVersion = "1.5.0"
lazy val scalatestVersion = "3.0.5"
lazy val scalacheckVersion = "1.14.0"

lazy val root = (project in file("."))
  .settings(
    name := "hello-compiler",
    version := "0.1",
    scalaVersion := "2.12.8",
    scalacOptions ++= Seq(
      "-language:higherKinds",
      "-Ypartial-unification",
    ),
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % catsVersion,
      "org.scalatest" %% "scalatest" % scalatestVersion % "test",
      "org.scalacheck" %% "scalacheck" % scalacheckVersion % "test",
    ),
    resolvers += Resolver.sonatypeRepo("releases"),
    addSbtPlugin("com.geirsson" % "sbt-scalafmt" % "1.5.1"),
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8"),
    addCompilerPlugin(
      "org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  )
