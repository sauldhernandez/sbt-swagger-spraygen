
name := "sbt-swagger-spraygen"

organization := "com.sauldhernandez"

scalaVersion in Global := "2.10.5"

version := "0.1"

sbtPlugin := true

//Dependencies
libraryDependencies ++= Seq(
  "io.swagger" % "swagger-parser" % "1.0.10",
  "com.eed3si9n" %% "treehugger" % "0.4.1"
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