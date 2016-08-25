package com.sauldhernandez.spraygen

import java.util

import com.sauldhernandez.spraygen.SpraySwaggerGenPlugin.autoImport.{AbstractDependency, DirectiveMapping, ExpandableDirective, PathParameterMapper}
import io.swagger.models.parameters._
import io.swagger.models._
import sbt.State
import treehugger.forest._
import definitions._
import treehuggerDSL._

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.reflect.ClassTag

/**
 * Creates spray directive compositions that can be used to implement REST endpoints
 */
class EndpointGenerator(state : State,
                        swaggerData : Swagger,
                        packageName : String,
                        authenticateMappings : DirectiveMapping,
                        jsonFormats : Boolean,
                        privateImplicits : Boolean,
                        extraImports : Seq[String],
                        customExtractions : DirectiveMapping,
                        pathParamMapper : PathParameterMapper,
                        customPathDirective : Option[ExpandableDirective],
                        customEntityExtraction : Option[ExpandableDirective]) {

  private case class Op(name : String, path : Path, method : HttpMethod, operation : Operation)

  def generate : String = {
    val jsonImports =
      if(jsonFormats)
        (if (extraImports.isEmpty) Seq(IMPORT("spray.httpx.SprayJsonSupport._"), IMPORT("spray.json.DefaultJsonProtocol._"))
        else Seq()) ++
        Seq(IMPORT(s"$packageName.models._"))
    else Seq()

    val endpointSources = swaggerData.getPaths.flatMap( x => x._2.getOperationMap.map( v => Op(x._1, x._2, v._1, v._2)))
      .groupBy(op => Option(op.operation.getTags).flatMap(_.headOption).getOrElse("uncategorized"))

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
    {
      val authDependencies = generateAuthenticateDependencies(operations)
      val entityExtractionDependencies = generateEntityExtractionDependencies(operations)
      val customDirectiveDependencies = generateCustomDirectiveDependencies(operations)
      val pathParamDependencies = generatePathParamDependencies(operations)

      generateImplicits(authDependencies ++ entityExtractionDependencies ++ customDirectiveDependencies ++ pathParamDependencies)
    } ++
    operations.map(x => generateOperationEndpoint(x))
  )

  private def generateImplicits(dependencies : Seq[AbstractDependency]) : Seq[Tree] = dependencies
    .distinct
    .flatMap { item =>
      val (name, t) = (item.dependencyName, item.dependencyType)
      if(privateImplicits)
        Seq(
          DEF(name) withType TYPE_REF(t) : Tree,
          DEF(s"_$name") withFlags(Flags.IMPLICIT, Flags.PRIVATE) withType TYPE_REF(t) := REF(name) : Tree
        )
      else
        Seq(
          DEF(name) withFlags Flags.IMPLICIT withType TYPE_REF(t) : Tree
        )
    }

  private def extractDependencies(keys : Seq[String], source : DirectiveMapping) = keys.flatMap(source.get).flatMap(_.dependencies)

  private def generateCustomDirectiveDependencies(operations : Seq[Op]) =  {
    val allParams = operations.flatMap(op => findAllParams(op))
      .map(_.getName).filter(p => customExtractions.get(p).isDefined)

    extractDependencies(allParams, customExtractions)
  }

  private def generateEntityExtractionDependencies(operations : Seq[Op]) = customEntityExtraction.map { extraction =>
    val allParams = operations.flatMap(op => findAllParams(op))
    val bodyParams = filterParameters[BodyParameter](allParams)
    if(bodyParams.nonEmpty)
      extraction.dependencies
    else Seq()
  }.getOrElse(Seq())

  private def generatePathParamDependencies(operations : Seq[Op]) = {
    val allParams = operations.flatMap(op => findAllParams(op))
    val pathParams = filterParameters[PathParameter](allParams)

    pathParams.flatMap(p => pathParamMapper(p).dependencies) ++ customPathDirective.map(_.dependencies).getOrElse(Seq())
  }

  private def generateAuthenticateDependencies(operations : Seq[Op]) = {
    val keys = operations
      .flatMap(op => Option(op.operation.getSecurity))
      .flatMap(sec => sec.flatten).map(_._1)

    extractDependencies(keys, authenticateMappings)
  }

  private def retrieveRefParam(ref : RefParameter) = {
    val paramName = ref.get$ref().split("/").last
    val param = swaggerData.getParameters.toMap.get(paramName)

    param.getOrElse(throw new UnsupportedOperationException(s"Could not solve parameter ref for name $paramName"))
  }

  private def matchPathType(pathParam : PathParameter) = REF(pathParamMapper(pathParam).directive)

  private def filterParameters[T <: Parameter : ClassTag](params : Seq[Parameter]) : Seq[T] = params.flatMap {
      case x : T => Some(x)
      case x : RefParameter => retrieveRefParam(x) match {
        case refParam : T => Some(refParam)
        case _ => None
      }
      case x => None
    }.filter { param =>
      //If the param has a custom extraction, it should not be provided here.
      !customExtractions.contains(param.getName)
    }

  private def findAllParams(op : Op) = {
    //Get the parameters
    val pathParams = Option(op.path.getParameters).getOrElse(new util.ArrayList[Parameter]()).toIndexedSeq
    val opParams = Option(op.operation.getParameters).getOrElse(new util.ArrayList[Parameter]()).toIndexedSeq

    pathParams ++ opParams
  }

  private def generateOperationEndpoint(op : Op) : Tree = {

    val allParams  = findAllParams(op)

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
          allParams.find(param => param.getName == paramName && param.getIn == "path").getOrElse {
            throw new UnsupportedOperationException(s"No parameter named $paramName was found for path ${op.name}.")
          }
        )
        param match {
          case x : PathParameter => matchPathType(x)
          case x : RefParameter =>
            retrieveRefParam(x) match {
              case pathParam: PathParameter => matchPathType(pathParam)
              case _ => throw new scala.UnsupportedOperationException(s"Referenced param ${x.get$ref()} is not a path parameter.")
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
    val pathDirective = REF(customPathDirective.map(_.directive).getOrElse("path")) APPLY (basePath.getOrElse(Seq()) ++ pathElements).reduceLeft { (a, b) => a INFIX "/" APPLY b }

    //If the operation has a body parameter, a parsing directive must be added.
    val bodyDirective = filterParameters[BodyParameter](allParams).filter(_ => jsonFormats).map { param =>
        param.getSchema match {
          case model: RefModel =>
            val modelName = model.get$ref().split("/").last
            val resultType = TYPE_REF(s"$packageName.models.$modelName")
            REF(customEntityExtraction.map(_.directive).getOrElse("entity")) APPLY (REF("as") APPLYTYPE (if(param.getRequired) resultType else TYPE_OPTION(resultType)))
          case _ =>
            //TODO: Add support for non-ref models
            throw new UnsupportedOperationException("Only ref models are currently supported.")
        }
      }.headOption

    //If the operation has an authorization method, an authenticate directive (or several) should be added
    val securityDirectives = Option(operation.getSecurity).map(sec => sec.flatMap { securityRequirement =>
      securityRequirement.map { x =>
        val (securityDefinitionName, oauthSchemes) =  x
        if(oauthSchemes.isEmpty) {
          //Non-oauth2 scheme
          val authenticatorName = authenticateMappings.get(securityDefinitionName)
          REF("authenticate") APPLY REF(authenticatorName.map(_.directive).getOrElse {
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
    val queryParams = filterParameters[QueryParameter](allParams).map { queryParam =>
      val value = queryParam.getType match {
        case "string" => LIT(queryParam.getName)
        case "integer" => LIT(queryParam.getName) DOT "as" APPLYTYPE IntClass
        case "number" => LIT(queryParam.getName) DOT "as" APPLYTYPE DoubleClass
        case "boolean" => LIT(queryParam.getName) DOT "as" APPLYTYPE BooleanClass
      }

      if(queryParam.getRequired)
        value : Tree
      else
        value DOT "?" : Tree
    }

    val paramsDirective = if(queryParams.isEmpty) None else Some {
      REF("parameters") APPLY queryParams
    }

    //Add header params
    val headerDirectives = filterParameters[HeaderParameter](allParams).map { param =>
      val directive = if(param.getRequired) "headerValueByName" else "optionalHeaderValueByName"
      REF(directive) APPLY LIT(param.getName) : Tree
    }

    //Add custom extractors
    val customDirectives = allParams.flatMap { param =>
      customExtractions.get(param.getName)
    }.map ( v => REF(v.directive) )
    //TODO: custom extractor implicits

    val name = Seq(method.name().toLowerCase()) ++ pathPieces.map(n => camelCase(n.split("-"))).map(name => if(name.startsWith("{")) stripBrackets(name) else name)

    val base = methodDirective INFIX "&" APPLY pathDirective
    val withBody = bodyDirective.map( base INFIX "&" APPLY _ ).getOrElse(base)
    val withAuth = securityDirectives.foldLeft(withBody)( _ INFIX "&" APPLY _)
    val withQueryParams = paramsDirective.map( withAuth INFIX "&" APPLY _).getOrElse(withAuth)
    val withHeaders = headerDirectives.foldLeft(withQueryParams)( _ INFIX "&" APPLY _ )
    val withCustom = customDirectives.foldLeft(withHeaders)( _ INFIX "&" APPLY _)

    DEF(camelCase(name)) := withCustom
  }

  def stripBrackets(name : String) = name.replace("{", "").replace("}", "")

  def camelCase(words : Seq[String]) = words.reduceLeft( (prev, n) => prev + n.capitalize)
}