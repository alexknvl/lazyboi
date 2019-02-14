autoCompilerPlugins := true

lazy val common = Seq(
  organization      := "io.steamcraft",
  version           := "0.0.1-SNAPSHOT",
  scalaVersion      := "2.12.6",
  resolvers ++= Seq(
    Resolver.sonatypeRepo("public"),
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots"),
    Resolver.mavenLocal),
  scalacOptions ++= List(
    "-deprecation",
    "-encoding", "UTF-8",
    "-explaintypes",
    "-Yrangepos",
    "-feature",
    "-Xfuture",
    "-Ypartial-unification",
    "-language:higherKinds",
    "-language:existentials",
    "-language:implicitConversions",
    "-language:experimental.macros",
    "-unchecked",
    "-Yno-adapted-args",
    "-opt-warnings",
    "-Xlint:_,-type-parameter-shadow",
    "-Xsource:2.13",
    "-Ywarn-dead-code",
    "-Ywarn-extra-implicit",
    "-Ywarn-inaccessible",
    "-Ywarn-infer-any",
    "-Ywarn-nullary-override",
    "-Ywarn-nullary-unit",
    "-Ywarn-numeric-widen",
    "-Ywarn-unused:_,-imports",
    "-Ywarn-value-discard",
    "-opt:l:inline",
    "-opt-inline-from:<source>",
  ),
  addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0-M4"),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4"),
  libraryDependencies += "com.lihaoyi" %% "acyclic" % "0.1.7" % "provided",
  addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.7"),
  scalacOptions += "-P:acyclic:force"
)

lazy val lazyboi = project.in(file("."))
  .settings(common)
  .settings(
    name := "lazyboi",
    organization := "com.alexknvl")
  .settings(libraryDependencies ++= List(
    "org.typelevel"  %% "cats-core"  % "1.5.0",
    "org.scalatest"  %% "scalatest"  % "3.0.5"  % "test",
    "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
  ))