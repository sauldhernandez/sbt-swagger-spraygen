# Swagger code generation for Spray

[![Build Status](https://snap-ci.com/sauldhernandez/sbt-swagger-spraygen/branch/master/build_image)](https://snap-ci.com/sauldhernandez/sbt-swagger-spraygen/branch/master)

## Overview

This plugins helps you develop swagger-defined REST services with [Spray](http://spray.io).

Currently, the plugin only reads the `definitions` section from the swagger YAML file, and generates case classes and
implicit jsonFormats for them.

## Compatibility

The plugins was designed to take a file that complies with the [Swagger 2.0 spec](https://github.com/swagger-api/swagger-spec/blob/master/versions/2.0.md).
If jsonFormat generation is enabled, the generated formats can be used with [spray-json](https://github.com/spray/spray-json) 1.3 and up.

## Usage

- Add the plugin in your `plugins.sbt`:

```
addSbtPlugin("com.sauldhernandez" % "sbt-swagger-spraygen" % "1.1.0")
```

- This plugin is an AutoPlugin, so you must add it to your project in `build.sbt`:

```
lazy val root = (project in file(".")).enablePlugins(SpraySwaggerGenPlugin)
```

- Add the plugin to your sourceGenerators:

```
sourceGenerators in Compile <+= spraygen in Compile
```

- Modify the plugin settings to fit your needs.

