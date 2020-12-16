package wheel.to

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

trait GeneratedInto[A, B] {
  def to(A: A): B

  def from(b: B): A
}

object Macro {
  implicit def genInto[Prod, Tup]: GeneratedInto[Prod, Tup] = macro genIntoImpl[Prod, Tup]

  def genIntoImpl[Prod: c.WeakTypeTag, Tup: c.WeakTypeTag](c: whitebox.Context): c.Expr[GeneratedInto[Prod, Tup]] = {
    import c.universe._

    val prodTpe = c.weakTypeOf[Prod]

    if (!prodTpe.typeSymbol.isClass ||
      !prodTpe.typeSymbol.asClass.isCaseClass) {
      c.abort(c.enclosingPosition, s"Type ${prodTpe.typeSymbol} is not a case class")
    }

    val paramLists = prodTpe.typeSymbol.asClass.primaryConstructor.asMethod.typeSignatureIn(prodTpe).paramLists
    val result = paramLists match {
      case List(params) =>
        val tupleName = definitions.TupleClass(params.size).name.toTypeName
        val tupleParams = params.map { sym =>
          val symLit = Literal(Constant(sym.name.toString))
          tq"_root_.wheel.to.Labelled[${sym.typeSignatureIn(prodTpe).finalResultType}, $symLit]"
        }
        val tup = tq"$tupleName[..$tupleParams]"

        val packers =
          params.map { sym =>
            val symLit = Literal(Constant(sym.name.toString))
            val symTerm = sym.name.toTermName
            q"_root_.wheel.to.Labelled.apply[${sym.typeSignatureIn(prodTpe).finalResultType}, $symLit](a.$symTerm)"
          }

        val unpackers =
          params.indices.map { idx =>
            val accessor = TermName(s"_${idx+1}")
            q"b.$accessor.value"
          }

        q"""new _root_.wheel.to.GeneratedInto[$prodTpe, $tup] {
            override def to(a: $prodTpe): $tup =
              (..$packers)

            override def from(b: $tup): $prodTpe =
               ${prodTpe.typeSymbol.companion}.apply(..$unpackers)
        }
        """
      case Nil =>
        q"""new _root_.wheel.to.GeneratedInto[$prodTpe, Unit] {
            override def to(a: $prodTpe): Unit = ()

            override def from(b: Unit): $prodTpe =
               $prodTpe()
        }
    """
      case _ => c.abort(c.enclosingPosition, s"Type ${prodTpe.typeSymbol} has multiple parameter lists which is currently not supported")
    }
    println(result)
    println(scala.util.Try(c.typecheck(result)))

    c.Expr(result)
  }
}
