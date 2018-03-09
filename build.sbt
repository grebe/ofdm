// See LICENSE for license details.

// Provide a managed dependency on X if -DXVersion="" is supplied on the command line.
val defaultVersions = Map(
  "chisel3" -> "3.0.2",
  "chisel-iotesters" -> "1.1.3-SNAPSHOT",
  "dsptools" -> "1.0.3-SNAPSHOT",
  "rocket-dsp-utils" -> "1.0-SNAPSHOT"
)

val commonSettings = Seq(
  organization := "edu.berkeley.cs",
  version      := "0.1",
  scalaVersion := "2.11.11",
  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("release")
  ),
  scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:reflectiveCalls")
)

val ofdmSettings = Seq(
  name := "ofdm",
  libraryDependencies ++= Seq("chisel3", "chisel-iotesters", "dsptools").map {
    dep: String => "edu.berkeley.cs" %% dep % sys.props.getOrElse(dep + "Version", defaultVersions(dep))
  },
  libraryDependencies += ("com.gilt" %% "handlebars-scala" % "2.1.1").exclude("org.slf4j", "slf4j-simple"),
  libraryDependencies += "org.vegas-viz" %% "vegas" % "0.3.11",
  libraryDependencies += "org.tukaani" % "xz" % "1.0"
)

val ofdmRocketSettings = Seq(
  name := "ofdm-rocket",
  libraryDependencies ++= Seq("rocket-dsp-utils").map {
    dep: String => "edu.berkeley.cs" %% dep % sys.props.getOrElse(dep + "Version", defaultVersions(dep))
  }
)

lazy val ofdm = (project in file(".")).
  settings(commonSettings: _*).
  settings(ofdmSettings: _*)

lazy val ofdmRocket = (project in file("rocket")).
  settings(commonSettings: _*).
  settings(ofdmRocketSettings: _*).
  dependsOn(ofdm).
  aggregate(ofdm)
