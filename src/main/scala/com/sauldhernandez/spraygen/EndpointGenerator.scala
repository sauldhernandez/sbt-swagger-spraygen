/* Copyright (C) 2015 Synergy-GB LLC.
 * All rights reserved.
 * This file is subject to the terms and conditions defined in
 * file 'LICENSE', which is part of this source code package.
 */
package com.sauldhernandez.spraygen

import java.util

import io.swagger.models.parameters.{Parameter, PathParameter}
import io.swagger.models.{HttpMethod, Path, Swagger}
import treehugger.forest._
import definitions._
import treehuggerDSL._

import scala.collection.JavaConversions._

/**
 * Creates spray directive compositions that can be used to implement REST endpoints
 */
class EndpointGenerator( swaggerData : Swagger, packageName : String ) {

  def generate : String = {

    val packageObject = PACKAGE(packageName) := BLOCK(
      PACKAGEOBJECTDEF("endpoints") := BLOCK(
        Seq(IMPORT("spray.routing.Directives._")) ++
        swaggerData.getPaths.flatMap( x => generatePathEndpoints(x._1, x._2))
      )
    )

    treeToString(packageObject)
  }

  private def generatePathEndpoints(endpoint : String, path : Path) = {
    //Get the parameters
    val pathParams = Option(path.getParameters).getOrElse(new util.ArrayList[Parameter]()).toIndexedSeq

    //Split the path
    val pathPieces = endpoint.split("/")

    path.getOperationMap.map { x =>
      val (method, operation) = x
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
              throw new UnsupportedOperationException(s"No parameter named $paramName was found for path $endpoint.")
            }
          )

          param match {
            case x : PathParameter =>
              x.getType match {
                case "string" => REF("Segment")
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

      //For now, return the combination of both
      val name = Seq(method.name().toLowerCase()) ++ pathPieces.map(name => if(name.startsWith("{")) stripBrackets(name) else name)
      VAL(camelCase(name)) := methodDirective INFIX("&") APPLY(pathDirective)
    }
  }

  def stripBrackets(name : String) = name.replace("{", "").replace("}", "")

  def camelCase(words : Seq[String]) = words.reduceLeft( (prev, n) => prev + n.toLowerCase.capitalize)
}
