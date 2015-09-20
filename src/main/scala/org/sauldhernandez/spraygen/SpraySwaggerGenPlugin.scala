package org.sauldhernandez.spraygen

import io.swagger.models.{ArrayModel, Model}
import io.swagger.models.properties.{ObjectProperty, ArrayProperty, RefProperty, Property}

import scala.collection.JavaConversions._
import org.json.JSONObject
import org.yaml.snakeyaml.Yaml
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
    lazy val generateJsonFormats = SettingKey[Boolean]("generate-json-formats", "Generate json format implicits for the generated model classes")
  }

  import autoImport._

  /**
   * Provide default settings
   */
  override lazy val projectSettings = Seq(
    swaggerSource := file("src/main/swagger"),
    sprayPackageName := "swagger.spray",
    generateJsonFormats := true,
    spraygen := gen(swaggerSource.value, sprayPackageName.value, generateJsonFormats.value, sourceManaged.value)
  )

  import treehugger.forest._
  import definitions._
  import treehuggerDSL._

  type PropertyProcessResult = (Type, Seq[Tree], Set[String])
  type ModelProcessResult = (Set[String], Seq[Tree])

  def gen(sourceFile : File, packageName : String, jsonFormats : Boolean, sourceDir : File) : Seq[File] = {
    //Hate to have a common state, but got no choice.
    val innerModelCount = collection.mutable.Map[String, Int]()

    //Convert the yaml to json
    val yaml = new Yaml()
    val yamlValues = yaml.load(read(sourceFile)).asInstanceOf[java.util.Map[String, Any]]

    val swaggerData = new SwaggerParser().parse(new JSONObject(yamlValues).toString)
    val definitions = swaggerData.getDefinitions

    def generateModelClass(name : String, currentModel : Model, alreadyProcessed : Set[String]) : ModelProcessResult = currentModel match {
      case arrayModel : ArrayModel =>
        //Array models are added as a type alias to the package object
        val (arrayType, arrayTree, arrayProcessed) = typeFromProperty(arrayModel.getItems, name, alreadyProcessed)
        (Set(name) ++ arrayProcessed, arrayTree ++ Seq(TYPEVAR(name) := TYPE_LIST(arrayType)))
      case objectModel =>
        val (resultTree, resultProcessed) = generateComplexObject(name, objectModel.getProperties.toMap, alreadyProcessed)
        (Set(name) ++ resultProcessed, resultTree)
    }

    def generateComplexObject(name : String, properties : Map[String, Property], alreadyProcessed : Set[String]) : (Seq[Tree], Set[String]) = {
      val (paramDefs, paramTrees, paramProcessed) = properties.foldLeft((Seq[ValDef](), Seq[Tree](), Set[String]())) { (prev, m) =>
        val (previousTypes, previousTree, previousProcessed) = prev
        val (paramName, paramInfo) = m
        val (paramType, paramTree, paramProcessed) = typeFromProperty(paramInfo, name, alreadyProcessed ++ previousProcessed)

        val resultType : ValDef = if(paramInfo.getRequired) PARAM(paramName, paramType) else PARAM(paramName, TYPE_OPTION(paramType))

        (previousTypes ++ Seq(resultType), previousTree ++ paramTree, previousProcessed ++ paramProcessed)
      }

      val classDef = CASECLASSDEF(name) withParams paramDefs
      val tree = if(jsonFormats)
        paramTrees ++ Seq[Tree](
          VAL(s"${name}Format") withFlags Flags.IMPLICIT := REF(s"jsonFormat${properties.size}") APPLY REF(name),
          classDef
        )
      else
        paramTrees ++ Seq[Tree](
          classDef
        )

      (tree, paramProcessed)
    }

    def typeFromProperty(property : Property, parentName : String, alreadyProcessed : Set[String]) : PropertyProcessResult = property match {
      case ref : RefProperty =>
        val targetName = ref.getSimpleRef
        //This is necessary so implicit jsonFormats are properly ordered.
        if(alreadyProcessed.contains(targetName))
          (RootClass.newClass(ref.getSimpleRef), Seq(), Set())
        else {
          val (processed, tree) = generateModelClass(targetName, definitions(targetName), alreadyProcessed)
          (RootClass.newClass(ref.getSimpleRef), tree, processed)
        }
      case array : ArrayProperty =>
        val (arrayType, tree, processed) = typeFromProperty(array.getItems, parentName, alreadyProcessed)
        (TYPE_LIST(arrayType), tree, processed)
      case obj : ObjectProperty =>
        val next = innerModelCount.get(parentName).map(_ + 1).getOrElse(1)
        innerModelCount.update(parentName, next)
        val childName = s"${parentName}Inner$next"
        val (complexTree, complexProcessed) = generateComplexObject(childName, obj.getProperties.toMap, alreadyProcessed)
        (RootClass.newClass(childName), complexTree, complexProcessed)
      case x => (convertBaseType(x.getType), Seq(), Set())
    }



    val packageOutput = sourceDir / "main" / "spraygen" / "package.scala"

    val resultingModels = definitions.foldLeft((Set[String](), Seq[Tree]())) { (prev, m) =>
      val (processed, result) = prev
      val (name, model) = m
      //If the name is in the processed list, just keep going.
      if(processed.contains(name)) prev
      else {
        val (newProcessed, tree) = generateModelClass(name, model, processed)
        (processed ++ newProcessed, result ++ tree)
      }
    }

    val packageObject = PACKAGE(packageName.substring(0, packageName.lastIndexOf("."))) := BLOCK(
      PACKAGEOBJECTDEF(packageName.substring(packageName.lastIndexOf(".") + 1)) := BLOCK(
        resultingModels._2
      )
    )

    if(jsonFormats)
      write(packageOutput, treeToString(IMPORT("spray.json.DefaultJsonProtocol._"), packageObject))
    else
      write(packageOutput, treeToString(packageObject))

    Seq(packageOutput)
  }

  def convertBaseType(typeName : String): Symbol = typeName match {
    case "string" => StringClass
    case "integer" => IntClass
    case "number" => DoubleClass
    case "boolean" => BooleanClass
  }
}