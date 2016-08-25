package com.sauldhernandez.spraygen

import sbt._
import Keys._
import io.swagger.models.parameters.PathParameter
import sbt.IO._
import io.swagger.parser._

object SpraySwaggerGenPlugin extends AutoPlugin {

  /**
   * Defines all settings/tasks that get automatically imported,
   * when the plugin is enabled
   */
  object autoImport {
    /**
      * An abstract dependency to be added to an endpoint.
      *
      * First element is the name of the dependency
      * Second element is the type of the dependency
      */
    case class AbstractDependency(dependencyName : String, dependencyType: String)

    /**
      * A directive that will be used, but might have implicit abstract dependencies.
      * @param directive The directive to call. This string will be placed as-is in the code.
      * @param dependencies Dependencies to add to the endpoint as implicit abstracts
      */
    case class ExpandableDirective(directive : String, dependencies : Seq[AbstractDependency] = Seq())

    type DirectiveMapping = Map[String, ExpandableDirective]

    type PathParameterMapper = PathParameter => ExpandableDirective

    object PathParameterMapper {
      val default : PathParameterMapper = { pathParameter => pathParameter.getType match {
        case "string" => ExpandableDirective("Segment")
        case "boolean" => ExpandableDirective("Map(\"true\" -> true, \"false\" -> false)")
        case "integer" => ExpandableDirective("IntNumber")
        case "number" => ExpandableDirective("DoubleNumber")
      }
      }
    }

    case class SprayGeneratorConfig(source : File,
                                    packageName : String,
                                    ignoreModels : Set[String] = Set(),
                                    customEntityExtraction : Option[ExpandableDirective] = None,
                                    withJsonFormats : Boolean = true,
                                    withPrivateImplicits : Boolean = true,
                                    authorizationHandlers : DirectiveMapping = Map(),
                                    pathParameterMapper: PathParameterMapper = PathParameterMapper.default,
                                    customPathDirective : Option[ExpandableDirective] = None
                                   )

    lazy val spraygen = TaskKey[Seq[File]]("spraygen", "Generates model code from swagger")
    lazy val sprayGenerations = SettingKey[Seq[SprayGeneratorConfig]]("spray-generations", "Settings for the generated endpoints")
    lazy val extraImports = SettingKey[Seq[String]]("extra-imports", "Additional imports to use when auto generating spray endpoints.")
    lazy val modelAnnotations = SettingKey[Map[String, String]]("model-annotations", "Mapping for extensions to annotations conversion in the model generation process")
    lazy val customExtractions = SettingKey[DirectiveMapping]("custom-extractions", "Parameters for which a custom extraction directive will be applied")
  }

  import autoImport._

  /**
   * Provide default settings
   */
  override lazy val projectSettings = Seq(
    sprayGenerations := Seq(SprayGeneratorConfig(
      source = (resourceDirectory in Compile).value / "api.yaml",
      packageName = "swagger.spray"
    )),
    extraImports := Seq(),
    modelAnnotations := Map(),
    customExtractions := Map(),
    spraygen := gen(state.value, sprayGenerations.value, extraImports.value, sourceManaged.value, customExtractions.value, modelAnnotations.value)
  )


  def gen(state : State,
          generations : Seq[SprayGeneratorConfig],
          imports : Seq[String],
          sourceDir : File,
          extractions : DirectiveMapping,
          annotations : Map[String, String]
         ) : Seq[File] = {
    generations.flatMap { config =>

      val configName = config.source.getName
      val swaggerData = new SwaggerParser().read(config.source.getAbsolutePath)
      val packageOutput = sourceDir / "main" / "spraygen" / configName / "models.scala"
      val endpointOutput = sourceDir / "main" / "spraygen" / configName / "endpoints.scala"

      val generator = new ModelGenerator(swaggerData, config.packageName, config.withJsonFormats, config.ignoreModels, annotations, imports)
      val endpointGenerator = new EndpointGenerator(
        state,
        swaggerData,
        config.packageName,
        config.authorizationHandlers,
        config.withJsonFormats,
        config.withPrivateImplicits,
        imports,
        extractions,
        config.pathParameterMapper,
        config.customPathDirective,
        config.customEntityExtraction)

      write(packageOutput, generator.generate)
      write(endpointOutput, endpointGenerator.generate)

      Seq(packageOutput, endpointOutput)
    }
  }
}