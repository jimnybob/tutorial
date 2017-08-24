package scalaworld.macros

import scala.meta._
import scala.annotation.StaticAnnotation
import scala.collection.immutable.Seq

trait Redactable[A] {
  def apply(a: A): A
}

class Redactor extends StaticAnnotation {
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

    object RedactorImpl {
      def ctorArgs(mapWithValues: Term.Name): Seq[Term] = ???
    }

    defn match {
      case cls @ Defn.Class(_, name, _, ctor, _) =>
        val companion = q"""object ${Term.Name(name.value)} { def umm: String = "eh oh" }"""
//        Term.Block(Seq(cls, companion))
        companion
    }

    def redactArgs(ps: Seq[Term.Param]): Term = {

      val newParams: Seq[Term] = ps.map { param =>
        val nameTerm = Term.Name(param.name.value)
        param.decltpe.getOrElse(throw new SerialiserException(s"type for $nameTerm not defined...")) match {
          case t: Type.Name => param // simple type, e.g. Strings
          case t @ Type.Apply(Type.Name(tpeName), _) =>
        }
        q""" $nameTerm = instance.$nameTerm"""
      }

      q"$typeTermName(..$newParams)"
    }

    val res = q"""
      ..$mods class $tName[..$tParams](...$paramss) extends $template
      object $typeTermName {

        implicit class RedactOps(instance: $tCompleteType) {
           def redact: String = {
             ${redactArgs(paramssFlat)}
             ""
             }
         }
      }"""

    println(res)
    res
  }
}
