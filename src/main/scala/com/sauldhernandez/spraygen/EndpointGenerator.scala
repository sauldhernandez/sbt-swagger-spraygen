/* Copyright (C) 2015 Synergy-GB LLC.
 * All rights reserved.
 * This file is subject to the terms and conditions defined in
 * file 'LICENSE', which is part of this source code package.
 */
package com.sauldhernandez.spraygen

import java.util

import io.swagger.models.parameters.{BodyParameter, Parameter, PathParameter}
import io.swagger.models._
import treehugger.forest._
import definitions._
import treehuggerDSL._

import scala.collection.JavaConversions._

/**
 * Creates spray directive compositions that can be used to implement REST endpoints
 */
class EndpointGenerator( swaggerData : Swagger, packageName : String, jsonFormats : Boolean ) {

  private case class Op(name : String, path : Path, method : HttpMethod, operation : Operation)

  def generate : String = {

    val jsonImports = if(jsonFormats) Seq(IMPORT("spray.httpx.SprayJsonSupport._"), IMPORT(s"$packageName.models._")) else Seq()

    val endpointSources = swaggerData.getPaths.flatMap( x => x._2.getOperationMap.map( v => Op(x._1, x._2, v._1, v._2)))
      .groupBy(_.operation.getTags.headOption.getOrElse("uncategorized"))

    val packageObject = PACKAGE(packageName) := BLOCK(
      PACKAGEOBJECTDEF("endpoints") := BLOCK(
        Seq(IMPORT("spray.routing.Directives._")) ++
        jsonImports ++
        endpointSources.map( source => generatePathTrait(source._1, source._2.toSeq))
      )
    )

    treeToString(packageObject)
  }

  private def generatePathTrait(name : String, operations : Seq[Op]) = TRAITDEF(s"${name.capitalize}Endpoints") := BLOCK(
    operations.map(x => generateOperationEndpoint(x))
  )

  private def generateOperationEndpoint(op : Op) : Tree = {
    //Get the parameters
    val pathParams = Option(op.path.getParameters).getOrElse(new util.ArrayList[Parameter]()).toIndexedSeq

    //Split the path
    val pathPieces = op.name.split("/")
    val (method, operation) = (op.method, op.operation)

    val methodDirective = method match {
      case HttpMethod.GET => REF("get")
      case HttpMethod.POST => REF("post")
      case HttpMethod.DELETE => REF("delete")
      case HttpMethod.OPTIONS => REF("options")
      case HttpMethod.PATCH => REF("patch")
      case HttpMethod.PUT => REF("put")
    }
    //Parse the pathPieces into segments
    val pathElements = pathPieces.filter(_.length > 0).map { piece =>
      if(piece.startsWith("{")) {
        val paramName = stripBrackets(piece)
        //Find the param first in operation parameters, if not there look for it in path parameters.
        val param = operation.getParameters.find(param => param.getName == paramName && param.getIn == "path").getOrElse(
          pathParams.find(param => param.getName == paramName && param.getIn == "path").getOrElse {
            throw new UnsupportedOperationException(s"No parameter named $paramName was found for path ${op.name}.")
          }
        )

        param match {
          case x : PathParameter =>
            x.getType match {
              case "string" => REF("Segment")
              case "boolean" => REF("Segment") //TODO: Proper boolean manipulation
              case "integer" => REF("IntNumber")
              case "number" => REF("DoubleNumber")
            }
          case x => throw new UnsupportedOperationException(s"Expected a PathParameter.")
        }
      }
      else {
        LIT(piece)
      }
    }

    //Build the path Directive
    val pathDirective = REF("path") APPLY pathElements.reduceLeft { (a, b) => a INFIX("/") APPLY(b) }

    //If the operation has a body parameter, a parsing directive must be added.
    val bodyParam = operation.getParameters.find(_.getIn == "body").filter(_ => jsonFormats).flatMap {
      case x : BodyParameter => Some(x)
      case _ => None
    }

    val bodyDirective = bodyParam.map { param =>
      param.getSchema() match {
        case model : RefModel =>
          val modelName = model.get$ref().split("/").last
          REF("entity") APPLY(REF("as") APPLYTYPE(s"$packageName.models.$modelName")  )
        case _ =>
          //TODO: Add support for non-ref models
          throw new UnsupportedOperationException("Only ref models are currently supported.")
      }
    }

    //For now, return the combination of both
    val name = Seq(method.name().toLowerCase()) ++ pathPieces.map(name => if(name.startsWith("{")) stripBrackets(name) else name)
    val resultDirective = if(bodyDirective.isDefined)
      methodDirective INFIX("&") APPLY(pathDirective) INFIX("&") APPLY(bodyDirective.get)
    else
      methodDirective INFIX("&") APPLY(pathDirective)

    VAL(camelCase(name)) := resultDirective
  }

  def stripBrackets(name : String) = name.replace("{", "").replace("}", "")

  def camelCase(words : Seq[String]) = words.reduceLeft( (prev, n) => prev + n.capitalize)
}
