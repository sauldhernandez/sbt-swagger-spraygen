package com.sauldhernandez.spraygen

import sbt._
import Keys._
import sbt.IO._
import io.swagger.parser._

object SpraySwaggerGenPlugin extends AutoPlugin {

  /**
   * Defines all settings/tasks that get automatically imported,
   * when the plugin is enabled
   */
  object autoImport {
    type SprayAuthorizations = Map[String, (String, Seq[(String, String)])]
    case class SprayGeneratorConfig(source : File, packageName : String, ignoreModels : Set[String] = Set(), withJsonFormats : Boolean = true, authorizationHandlers : SprayAuthorizations = Map())
    lazy val spraygen = TaskKey[Seq[File]]("spraygen", "Generates model code from swagger")
    lazy val sprayGenerations = SettingKey[Seq[SprayGeneratorConfig]]("spray-generations", "Settings for the generated endpoints")
    lazy val extraImports = SettingKey[Seq[String]]("extra-imports", "Additional imports to use when auto generating spray endpoints.")
  }

  import autoImport._

  /**
   * Provide default settings
   */
  override lazy val projectSettings = Seq(
    sprayGenerations := Seq(SprayGeneratorConfig(
      source = (sourceDirectory in Compile).value / "resources" / "api.yaml",
      packageName = "swagger.spray"
    )),
    extraImports := Seq(),
    spraygen := gen(state.value, sprayGenerations.value, extraImports.value, sourceManaged.value)
  )


  def gen(state : State, generations : Seq[SprayGeneratorConfig], imports : Seq[String], sourceDir : File) : Seq[File] = {
    generations.flatMap { config =>
      val swaggerData = new SwaggerParser().read(config.source.getAbsolutePath)
      val packageOutput = sourceDir / "main" / "spraygen" / "models.scala"
      val endpointOutput = sourceDir / "main" / "spraygen" / "endpoints.scala"

      val generator = new ModelGenerator(swaggerData, config.packageName, config.withJsonFormats, config.ignoreModels)
      val endpointGenerator = new EndpointGenerator(state, swaggerData, config.packageName, config.authorizationHandlers, config.withJsonFormats, imports)

      write(packageOutput, generator.generate)
      write(endpointOutput, endpointGenerator.generate)

      Seq(packageOutput, endpointOutput)
    }
  }
}