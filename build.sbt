// See LICENSE for license details.

name := "ofdm"

organization := "edu.berkeley.eecs"

version := "0.1"

scalaVersion := "2.11.11"

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("release")
)

libraryDependencies ++= Seq("chisel3", "chisel-iotesters", "dsptools").map {
  dep: String => "edu.berkeley.cs" %% dep % sys.props.getOrElse(dep + "Version", defaultVersions(dep))
}

libraryDependencies += ("com.gilt" %% "handlebars-scala" % "2.1.1").exclude("org.slf4j", "slf4j-simple")

libraryDependencies += "org.vegas-viz" %% "vegas" % "0.3.11"

libraryDependencies += "org.tukaani" % "xz" % "1.0"

// Provide a managed dependency on X if -DXVersion="" is supplied on the command line.
val defaultVersions = Map(
  "chisel3" -> "3.0.1",
  "chisel-iotesters" -> "1.1.1",
  "dsptools" -> "1.0.1"
)

def scalacOptionsVersion(scalaVersion: String): Seq[String] = {
  Seq() ++ {
    // If we're building with Scala > 2.11, enable the compile option
    //  switch to support our anonymous Bundle definitions:
    //  https://github.com/scala/bug/issues/10047
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, scalaMajor: Long)) if scalaMajor < 12 => Seq()
      case _ => Seq("-Xsource:2.11")
    }
  }
}

def javacOptionsVersion(scalaVersion: String): Seq[String] = {
  Seq() ++ {
    // Scala 2.12 requires Java 8. We continue to generate
    //  Java 7 compatible code for Scala 2.11
    //  for compatibility with old clients.
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, scalaMajor: Long)) if scalaMajor < 12 =>
        Seq("-source", "1.7", "-target", "1.7")
      case _ =>
        Seq("-source", "1.8", "-target", "1.8")
    }
  }
}

javacOptions ++= javacOptionsVersion(scalaVersion.value)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:reflectiveCalls") ++ scalacOptionsVersion(scalaVersion.value)
