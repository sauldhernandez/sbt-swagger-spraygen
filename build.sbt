lazy val root = (project in file(".")).enablePlugins(VersioningPlugin)

name := "sbt-swagger-spraygen"

organization := "com.sauldhernandez"

scalaVersion in Global := "2.10.5"

semanticVersion := Version(1, 0, 0)

sbtPlugin := true

//Dependencies
libraryDependencies ++= Seq(
  "io.swagger" % "swagger-parser" % "1.0.16",
  "com.eed3si9n" %% "treehugger" % "0.4.1",
  "org.slf4j" % "slf4j-jdk14" % "1.7.13"
)

// Settings to build a nice looking plugin site
site.settings

com.typesafe.sbt.SbtSite.SiteKeys.siteMappings <+= baseDirectory map { dir =>
  val nojekyll = dir / "src" / "site" / ".nojekyll"
  nojekyll -> ".nojekyll"
}

site.sphinxSupport()

site.includeScaladoc()

// enable github pages
ghpages.settings

git.remoteRepo := "git@github.com:sauldhernandez/sbt-swagger-spraygen.git"

// Scripted - sbt plugin tests
scriptedSettings

scriptedLaunchOpts <+= version apply { v => "-Dproject.version="+v }

useGpg := true

usePgpKeyHex("34de53dd")

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

pomIncludeRepository := { _ => false }

pomExtra := <url>https://github.com/sauldhernandez/sbt-swagger-spraygen</url>
  <licenses>
    <license>
      <name>MIT</name>
      <url>http://opensource.org/licenses/MIT</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:sauldhernandez/sbt-swagger-spraygen.git</url>
    <connection>scm:git:git@github.com:sauldhernandez/sbt-swagger-spraygen.git</connection>
  </scm>
  <developers>
    <developer>
      <id>sauldhernandez</id>
      <name>Saul Hernandez</name>
      <url>http://github.com/sauldhernandez</url>
    </developer>
  </developers>