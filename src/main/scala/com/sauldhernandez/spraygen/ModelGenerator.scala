package com.sauldhernandez.spraygen

import io.swagger.models.properties.{ObjectProperty, ArrayProperty, RefProperty, Property}
import io.swagger.models.{ArrayModel, Model, Swagger}
import treehugger.forest._
import definitions._
import treehuggerDSL._

import scala.collection.JavaConversions._

/**
 * Automatically generates model classes and spray jsonFormats from a swagger definition object.
 */
class ModelGenerator(swaggerData : Swagger, packageName : String, generateJsonFormats : Boolean, ignoreModels : Set[String], annotations : Map[String, String], extraImports : Seq[String]) {

  type PropertyProcessResult = (Type, Seq[Tree], Set[String])
  type ModelProcessResult = (Set[String], Seq[Tree])

  //Hate to have a common state, but got no choice.
  val innerModelCount = collection.mutable.Map[String, Int]()

  val definitions = swaggerData.getDefinitions

  def generate : String = {
    innerModelCount.clear()

    val resultingModels = definitions.foldLeft((Set[String](), Seq[Tree]())) { (prev, m) =>
      val (processed, result) = prev
      val (name, model) = m
      //If the name is in the processed list or is marked to be ignored, just keep going.
      if(processed.contains(name) || ignoreModels.contains(name)) prev
      else {
        val (newProcessed, tree) = generateModelClass(name, model, processed)
        (processed ++ newProcessed, result ++ tree)
      }
    }

    val packageObject = PACKAGE(packageName) := BLOCK(
      extraImports.map(pkg => IMPORT(pkg)) ++ Seq(
        PACKAGEOBJECTDEF("models") := BLOCK(
        resultingModels._2
      ))
    )

    if(generateJsonFormats)
      treeToString(IMPORT("spray.json.DefaultJsonProtocol._"), packageObject)
    else
      treeToString(packageObject)
  }

  def generateModelClass(name : String, currentModel : Model, alreadyProcessed : Set[String]) : ModelProcessResult = currentModel match {
    case arrayModel : ArrayModel =>
      //Array models are added as a type alias to the package object
      val (arrayType, arrayTree, arrayProcessed) = typeFromProperty(arrayModel.getItems, name, alreadyProcessed)
      (Set(name) ++ arrayProcessed, arrayTree ++ Seq(TYPEVAR(name) := TYPE_SEQ(arrayType)))
    case objectModel =>
      //If an object model has no properties, make it an alias of JsObject (as it can have anything)
      val properties = Option(objectModel.getProperties).map(_.toMap).getOrElse(Map())
      if(properties.isEmpty) {
        (Set(name), Seq(TYPEVAR(name) := TYPE_REF("spray.json.JsObject")))
      }
      else {
        val (resultTree, resultProcessed) = generateComplexObject(name, properties, alreadyProcessed)
        (Set(name) ++ resultProcessed, resultTree)
      }
  }

  def generateComplexObject(name : String, properties : Map[String, Property], alreadyProcessed : Set[String]) : (Seq[Tree], Set[String]) = {
    val (paramDefs, paramTrees, paramProcessed) = properties.foldLeft((Seq[ValDef](), Seq[Tree](), Set[String]())) { (prev, m) =>
      val (previousTypes, previousTree, previousProcessed) = prev
      val (paramName, paramInfo) = m
      val (paramType, paramTree, paramProcessed) = typeFromProperty(paramInfo, name, alreadyProcessed ++ previousProcessed)

      val annotationsData = Option(paramInfo.getVendorExtensions).map(_.toMap).getOrElse(Map())
      val annotationsTree = for {
        (name, rawValue) <- annotationsData
        if rawValue.isInstanceOf[java.lang.Boolean] && rawValue.asInstanceOf[Boolean]
        mapped = annotations.get(name)
        if mapped.isDefined

      } yield ANNOT(TYPE_REF(mapped.get))

      val resultType : ValDef = if(paramInfo.getRequired) PARAM(paramName, paramType) withAnnots annotationsTree else PARAM(paramName, TYPE_OPTION(paramType)) withAnnots annotationsTree

      (previousTypes ++ Seq(resultType), previousTree ++ paramTree, previousProcessed ++ paramProcessed)
    }

    val classDef = CASECLASSDEF(name) withParams paramDefs
    val tree = if(generateJsonFormats)
      paramTrees ++ Seq[Tree](
        VAL(s"${name}Format") withFlags Flags.IMPLICIT := {
          REF(s"jsonFormat${properties.size}") APPLY REF(name)
        },
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
      (TYPE_SEQ(arrayType), tree, processed)
    case obj : ObjectProperty =>
      val next = innerModelCount.get(parentName).map(_ + 1).getOrElse(1)
      innerModelCount.update(parentName, next)
      val childName = s"${parentName}Inner$next"
      val properties = Option(obj.getProperties).map(_.toMap).getOrElse(Map())
      if(properties.isEmpty) {
        (TYPE_REF("spray.json.JsObject"), Seq(), Set())
      }
      else {
        val (complexTree, complexProcessed) = generateComplexObject(childName, properties, alreadyProcessed)
        (RootClass.newClass(childName), complexTree, complexProcessed)
      }
    case x => (convertBaseType(x.getType), Seq(), Set())
  }

  def convertBaseType(typeName : String): Symbol = typeName match {
    case "string" => StringClass
    case "integer" => IntClass
    case "number" => DoubleClass
    case "boolean" => BooleanClass
  }
}
