package com.nickrobison.inland.derive

import com.nickrobison.inland.types.{Layout, Layout2}

import scala.compiletime.{erasedValue, error, summonInline}
import scala.deriving.Mirror
import scala.quoted.{Expr, Quotes, Type}

object LayoutDerivers {

//  inline def summonInstances[T, Elem <: Tuple]: List[Layout2[T]] = {
//    inline
//  }
//
//  inline def deriveOrSummon[T, Elem]: Layout2[Elem] = {
//    inline erasedValue[Elem] match {
//      case _: T => deriveRec[T, Elem]
//      case _ => summonInline[Layout2[Elem]]
//    }
//  }
//
//  inline def deriveRec[T, Elem]: Layout2[Elem] = {
//    inline erasedValue[T] match {
//      case _: Elem => error("inifinite recursive derivation")
//      case _ => LayoutDerivers.derived[Elem](using summonInline[Mirror.Of[Elem]])
//    }
//  }
//

  def derivedMacro[T: Type](using Quotes): Expr[Layout2[T]] = ???

  inline def derived[A]: Layout2[A] = ${ derivedMacro[A] }

}
