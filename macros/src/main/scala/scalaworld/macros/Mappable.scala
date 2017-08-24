package scalaworld.macros

import scala.annotation.compileTimeOnly
import scala.annotation.StaticAnnotation
import scala.collection.immutable.Seq
import scala.meta._
import scala.util.control.NoStackTrace
import org.scalameta.logger

// type classes that @mappable will generate for annotated classes
trait ToMap[A] {
  def apply(a: A): Map[String, Any]
}
trait FromMap[A] {
  def apply(keyValues: Map[String, Any]): Option[A]
}

case class SerialiserException(message: String, cause: Option[Throwable] = None)
  extends RuntimeException(message, cause.orNull)
    with NoStackTrace

/** map a class member to a custom name */
class mappedTo(name: String) extends StaticAnnotation

/** mark a class member as nullable (by default serialiser will complain and not set the value to null) */
class nullable extends StaticAnnotation

/** example usages: see MappableTest.scala */
@compileTimeOnly("@scalaworld.macros.Mappable not expanded")
class Mappable(annotationParams: Map[String, Any]) extends StaticAnnotation {
  inline def apply(defn: Any): Any = meta {

    // defined class may or may not have a companion object
    val (classDefn: Defn.Class, compDefnOption: Option[Defn.Object]) = defn match {
      case classDefn: Defn.Class => (classDefn, None) //only class, no companion
      case Term.Block((classDefn: Defn.Class) :: (compDefn: Defn.Object) :: Nil) => (classDefn, Option(compDefn)) // class + companion
      case _ => abort(defn.pos, "Invalid annottee")
    }

    // get existing companion object statements (if any)
    val compStats: Seq[Stat] = compDefnOption match {
      case None => Nil
      case Some(compDefn) => compDefn.templ.stats.getOrElse(Nil)
    }

    val q"..$mods class $tName[..$tParams] ..$ctorMods (...$paramss) extends $template" = classDefn
    val paramssFlat: Seq[Term.Param] = paramss.flatten

    val typeTermName = Term.Name(tName.value)

    val tParamTypes: Seq[Type] = tParams map Helpers.toType
    val tCompleteTerm: Term =
      if (tParamTypes.isEmpty) q"$typeTermName"
      else q"$typeTermName[..$tParamTypes]"
    val tCompleteType: Type = Helpers.toType(tCompleteTerm)
    val tCompleteTypeOption: Type = Helpers.toType(q"Option[$tCompleteType]")

    val customMappings: Map[Term.Param, String] = paramssFlat.map { param =>
      param.mods.collect {
        case mod"@mappedTo(${Lit.String(mappedTo)})" => (param -> mappedTo)
      }
    }.flatten.toMap.withDefault(_.name.value)

    val isNullable: Map[Term.Param, Boolean] = paramssFlat.map { param =>
      param.mods.collect {
        case mod"@nullable" => (param -> true)
      }
    }.flatten.toMap.withDefaultValue(false)

    object ToMapImpl {
      val instanceName: Term.Name = q"instance"
      def keyValues(instanceName: Term.Name): Seq[Term] = paramssFlat.map { param =>
        val propertyKey: String = customMappings(param)
        val nameTerm = Term.Name(param.name.value)
        param.decltpe.getOrElse{ throw new SerialiserException(s"type for $nameTerm not defined...") } match {
//          case t: Type.Name if t.value == "Nested" =>
//            println("nameTerm: " + nameTerm.value)
//            q"$propertyKey -> scala.collection.immutable.Map[String, Any](..${keyValues(nameTerm)})"
          case t: Type.Name => // simple type, e.g. String
            println(">>> " + t.value)
            q"$propertyKey -> $instanceName.$nameTerm"
          case Type.Apply(Type.Name(tpeName), _) if tpeName == "Option" => // Option[A]
            q"$propertyKey -> $instanceName.$nameTerm.getOrElse(null)"
          case other => throw new SerialiserException(s"unable to map $other (${other.getClass})... not (yet) supported")
        }
      }
    }

    object FromMapImpl {
      val ctorMapWithValues: Term.Name = q"values"

      // get default value and store those value as a map in object
      val defaultValue:  Seq[Term.ApplyInfix] = paramssFlat collect {
        case param if param.default.nonEmpty =>
          q"""${param.name.value} -> ${param.default.get}"""
      }

      val ctorParamsFirst: Seq[Term.Param] = paramss.headOption.getOrElse(Nil)
      def ctorArgs(mapWithValues: Term.Name): Seq[Term] = ctorParamsFirst.map { param =>
        val propertyKey: String = customMappings(param)
        val nameTerm = Term.Name(param.name.value)
        logger.elem(nameTerm)
        println(nameTerm)
        val fromMapWithExpectedType: Term = param.decltpe.getOrElse{ throw new SerialiserException(s"type for $nameTerm not defined...") } match {
          case tpe: Type.Name => // simple type, e.g. String
            if (!isNullable(param))
              q"""$mapWithValues($propertyKey).asInstanceOf[$tpe]"""
            else
              q"""$mapWithValues.get($propertyKey).map(_.asInstanceOf[$tpe]).orNull"""
          case completeTpe @ Type.Apply(Type.Name(tpeName), wrappedTpe :: Nil) if tpeName == "Option" => // Option[A]
            q"""Option($mapWithValues.get($propertyKey).orNull).asInstanceOf[$completeTpe]"""
          case other => throw new SerialiserException(s"unable to map $other (${other.getClass})... not (yet) supported")
        }
        q""" $nameTerm = $fromMapWithExpectedType"""
      }
    }

    object AnnotationParams {
      val debugKey = "_debug" //if set to `true`, we will print the generated code

      /* not having named arguments to @mappable limits extensibility, but this is currently the only way to
      * pass arguments to the macro annotation */
      val (debugEnabled: Boolean, paramsAsTerms: Seq[Term]) = {
        def illegalDefinition(unsupported: Tree) = throw new SerialiserException(
          "illegal definition of @mappable annotation. Valid examples are e.g.:" +
            " `@mappable` and `@mappable(List(\"param1\" -> \"value1\")`. See MappableTest.scala for more examples. " +
            s"Unsupported Tree: $unsupported of type ${unsupported.getClass}")

        val params: Seq[(Lit, Lit)] = this match {
          case q"new $_()" => Nil // no params defined
          case q"new $_(${Term.Apply(_, params)})" => params.map { // I'd rather match on a refinement type, but that's unchecked :(
            case q"${name: Lit} -> ${value: Lit}" => (name -> value)
            case unsupported => illegalDefinition(unsupported)
          }
          case unsupported => illegalDefinition(unsupported)
        }

        val paramsAsTerms: Seq[Term] = params.map {
          case (memberName: Lit, mappedName: Lit) => q"""$memberName -> $mappedName"""
        }
        val paramsAsStrings: Map[String, String] = params.map {
          case (memberName: Lit, mappedName: Lit) => (memberName.value.toString, mappedName.value.toString)
        }.toMap
        val debugEnabled: Boolean = paramsAsStrings.getOrElse(debugKey, "false").toBoolean
        (debugEnabled, paramsAsTerms)
      }
    }

    val res = q"""
      ..$mods class $tName[..$tParams](...$paramss) extends $template
      object $typeTermName {
        val defaultValueMap: scala.collection.immutable.Map[String, Any] =
          scala.collection.immutable.Map(..${FromMapImpl.defaultValue})
        /** parameters passed to annotation, e.g. @mappable(List("param1" -> "value1")) */
        val params: scala.collection.immutable.Map[String, Any] =
          scala.collection.immutable.Map(..${AnnotationParams.paramsAsTerms})
        implicit def toMap[..$tParams] = new scalaworld.macros.ToMap[$tCompleteType] {
          override def apply(${ToMapImpl.instanceName}: ${Option(tCompleteType)}): scala.collection.immutable.Map[String, Any] =
            scala.collection.immutable.Map[String, Any](..${ToMapImpl.keyValues(ToMapImpl.instanceName)})
        }
        implicit class ToMapOps[..$tParams](instance: $tCompleteType) {
          def toMap(implicit toMap: scalaworld.macros.ToMap[$tCompleteType]): scala.collection.immutable.Map[String, Any] =
            toMap(instance)
        }

        implicit def fromMap[..$tParams] = new scalaworld.macros.FromMap[$tCompleteType] {
          override def apply(v: scala.collection.immutable.Map[String, Any]): ${Option(tCompleteTypeOption)} = {
              val values = defaultValueMap ++ v
              scala.util.Try {
                ${tCompleteTerm}(..${FromMapImpl.ctorArgs(FromMapImpl.ctorMapWithValues)})
              }.toOption
            }
        }
        ..$compStats
      }
    """

    /*if (AnnotationParams.debugEnabled)*/ println(res)
    res
  }
}

object Helpers {
  def toType(term: Term): Type = term match {
    case name: Term.Name => Type.Name(name.value)
    case applyType: Term.ApplyType => Type.Apply(toType(applyType.fun), applyType.targs)
  }

  def toType(tparam: Type.Param): Type = Type.Name(tparam.name.value)
}

