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
    lazy val spraygen = TaskKey[Seq[File]]("spraygen", "Generates model code from swagger")
    lazy val swaggerSource = SettingKey[File]("swagger-source", "Source file")
    lazy val sprayPackageName = SettingKey[String]("package-name", "Package name for the generated files")
    lazy val ignoreModels = SettingKey[Seq[String]]("ignore-models", "Swagger models whose code will not be generated.")
    lazy val generateJsonFormats = SettingKey[Boolean]("generate-json-formats", "Generate json format implicits for the generated model classes")
    lazy val authorizationHandlers = SettingKey[Map[String, (String, Seq[(String, String)])]]("authorization-handler", "Mapping to use to associate a swagger securityDefinition to a spray ContextAuthenticator.")
    lazy val extraImports = SettingKey[Seq[String]]("extra-imports", "Additional imports to use when auto generating spray endpoints.")
  }

  import autoImport._

  /**
   * Provide default settings
   */
  override lazy val projectSettings = Seq(
    swaggerSource := (sourceDirectory in Compile).value / "resources" / "api.yaml",
    authorizationHandlers := Map(),
    extraImports := Seq(),
    sprayPackageName := "swagger.spray",
    generateJsonFormats := true,
    ignoreModels := Seq(),
    spraygen := gen(state.value, swaggerSource.value, sprayPackageName.value, generateJsonFormats.value, sourceManaged.value, ignoreModels.value.toSet, authorizationHandlers.value, extraImports.value)
  )


  def gen(state : State, sourceFile : File, packageName : String, jsonFormats : Boolean, sourceDir : File, ignore : Set[String], authHandler : Map[String, (String, Seq[(String, String)])], imports : Seq[String]) : Seq[File] = {

    val swaggerData = new SwaggerParser().read(sourceFile.getAbsolutePath)
    val packageOutput = sourceDir / "main" / "spraygen" / "models.scala"
    val endpointOutput = sourceDir / "main" / "spraygen" / "endpoints.scala"

    val generator = new ModelGenerator(swaggerData, packageName, jsonFormats, ignore)
    val endpointGenerator = new EndpointGenerator(state, swaggerData, packageName, authHandler, jsonFormats, imports)

    write(packageOutput, generator.generate)
    write(endpointOutput, endpointGenerator.generate)

    Seq(packageOutput, endpointOutput)
  }
}