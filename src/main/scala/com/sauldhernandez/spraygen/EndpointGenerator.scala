/* Copyright (C) 2015 Synergy-GB LLC.
 * All rights reserved.
 * This file is subject to the terms and conditions defined in
 * file 'LICENSE', which is part of this source code package.
 */
package com.sauldhernandez.spraygen

import java.util

import io.swagger.models.parameters.{QueryParameter, BodyParameter, Parameter, PathParameter}
import io.swagger.models._
import sbt.State
import treehugger.forest._
import definitions._
import treehuggerDSL._

import scala.collection.JavaConversions._
import scala.collection.mutable

/**
 * Creates spray directive compositions that can be used to implement REST endpoints
 */
class EndpointGenerator(state : State, swaggerData : Swagger, packageName : String, authenticateMappings : Map[String, (String, Seq[(String, String)])], jsonFormats : Boolean, extraImports : Seq[String] ) {

  private case class Op(name : String, path : Path, method : HttpMethod, operation : Operation)

  def generate : String = {

    val jsonImports = if(jsonFormats) Seq(IMPORT("spray.httpx.SprayJsonSupport._"), IMPORT(s"$packageName.models._")) else Seq()

    val endpointSources = swaggerData.getPaths.flatMap( x => x._2.getOperationMap.map( v => Op(x._1, x._2, v._1, v._2)))
      .groupBy(_.operation.getTags.headOption.getOrElse("uncategorized"))

    val packageObject = PACKAGE(packageName) := BLOCK(
      PACKAGEOBJECTDEF("endpoints") := BLOCK(
        Seq(IMPORT("spray.routing.Directives._"), IMPORT("scala.concurrent.ExecutionContext.Implicits.global")) ++
        extraImports.map(pkg => IMPORT(pkg)) ++
        jsonImports ++
        endpointSources.map( source => generatePathTrait(source._1, source._2.toSeq))
      )
    )

    treeToString(packageObject)
  }

  private def generatePathTrait(name : String, operations : Seq[Op]) = TRAITDEF(s"${name.capitalize}Endpoints") := BLOCK(
    generateAuthenticateImplicits(operations) ++
    operations.map(x => generateOperationEndpoint(x))

  )

  private def generateAuthenticateImplicits(operations : Seq[Op]) = operations
      .flatMap(op => Option(op.operation.getSecurity))
      .flatMap(sec => sec.flatten).map(_._1)
      .flatMap(authenticateMappings.get)
      .flatMap(_._2)
      .distinct
      .map { item =>
        val (name, t) = item
        DEF(name) withFlags Flags.IMPLICIT withType TYPE_REF(t) : Tree
      }

  private def generateOperationEndpoint(op : Op) : Tree = {
    //Get the parameters
    val pathParams = Option(op.path.getParameters).getOrElse(new util.ArrayList[Parameter]()).toIndexedSeq

    //Split the path
    val pathPieces = op.name.split("/")
    val (method, operation) = (op.method, op.operation)

    val methodDirective = method match {
      case HttpMethod.HEAD => REF("head")
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
              case "boolean" => REF("Map(\"true\" -> true, \"false\" -> false)")
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
    val basePath = Option(swaggerData.getBasePath).map(base => base.split("/").filter(!_.isEmpty).map(LIT(_)).toSeq)
    val pathDirective = REF("path") APPLY (basePath.getOrElse(Seq()) ++ pathElements).reduceLeft { (a, b) => a INFIX "/" APPLY b }

    //If the operation has a body parameter, a parsing directive must be added.
    val bodyParam = operation.getParameters.find(_.getIn == "body").filter(_ => jsonFormats).flatMap {
      case x : BodyParameter => Some(x)
      case _ => None
    }

    val bodyDirective = bodyParam.map { param =>
      param.getSchema() match {
        case model : RefModel =>
          val modelName = model.get$ref().split("/").last
          REF("entity") APPLY(REF("as") APPLYTYPE s"$packageName.models.$modelName")
        case _ =>
          //TODO: Add support for non-ref models
          throw new UnsupportedOperationException("Only ref models are currently supported.")
      }
    }

    //If the operation has an authorization method, an authenticate directive (or several) should be added
    val securityDirectives = Option(operation.getSecurity).map(sec => sec.flatMap { securityRequirement =>
      securityRequirement.map { x =>
        val (securityDefinitionName, oauthSchemes) =  x
        if(oauthSchemes.isEmpty) {
          //Non-oauth2 scheme
          val authenticatorName = authenticateMappings.get(securityDefinitionName)
          REF("authenticate") APPLY REF(authenticatorName.map(_._1).getOrElse {
            state.log.error(s"Could not find authenticator mapping for security definition $securityDefinitionName.")
            throw new IllegalArgumentException("authorizationHandler")
          })
        }
        else {
          //OAuth2 Scheme
          throw new UnsupportedOperationException("OAuth 2 security definitions not currently supported.")
        }
      }
    }).getOrElse(mutable.Buffer())

    //Add query params directive
    val queryParams = (pathParams ++ op.operation.getParameters).flatMap {
      case x: QueryParameter => Some(x)
      case x => None
    }.map { queryParam =>
      val value = queryParam.getType match {
        case "string" => LIT(queryParam.getName)
        case "integer" => LIT(queryParam.getName) DOT "as" APPLYTYPE IntClass
        case "number" => LIT(queryParam.getName) DOT "as" APPLYTYPE DoubleClass
      }

      if(queryParam.getRequired)
        value : Tree
      else
        value DOT "?" : Tree
    }

    val paramsDirective = if(queryParams.isEmpty) None else Some {
      REF("parameters") APPLY queryParams
    }

    val name = Seq(method.name().toLowerCase()) ++ pathPieces.map(n => camelCase(n.split("-"))).map(name => if(name.startsWith("{")) stripBrackets(name) else name)

    val base = methodDirective INFIX "&" APPLY pathDirective
    val withBody = bodyDirective.map{ d => base INFIX "&" APPLY d }.getOrElse(base)
    val withAuth = securityDirectives.foldLeft(withBody)( (prev, next) => prev INFIX "&" APPLY next)
    val withQueryParams = paramsDirective.map( d => withAuth INFIX "&" APPLY d).getOrElse(withAuth)

    DEF(camelCase(name)) := withQueryParams
  }

  def stripBrackets(name : String) = name.replace("{", "").replace("}", "")

  def camelCase(words : Seq[String]) = words.reduceLeft( (prev, n) => prev + n.capitalize)
}
