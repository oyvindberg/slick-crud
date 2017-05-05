package com.olvind.crud
package frontend

import japgolly.scalajs.react.extra.{LogLifecycle, Reusability, ReusableFn}
import japgolly.scalajs.react.{CallbackTo, ReactComponentB, TopNode}

import scala.concurrent.Future

object ComponentUpdates {
  private val enableDebug          = false
  private val enableDifferentDebug = false

  object InferredReusability {
    def apply[P <: Product]: Reusability[P] = Reusability.fn[P](f)

    def logIfDifferent(hint: String, a1: Any, a2: Any)(ok: Boolean): Unit =
      if (!ok && enableDifferentDebug) println(s"different ($hint): a1: $a1, a2: $a2")

    private val f = new ((Product, Product) ⇒ Boolean){
      override def apply(p1: Product, p2: Product): Boolean = {

        val areSame = (p1, p2) match {
          case (a1: AnyRef, a2: AnyRef) if a1 eq a2 ⇒ true
          case _                                    ⇒ false
        }
        if (areSame) return true

        val differentClass = p1.getClass.getName != p2.getClass.getName
        if (differentClass) {
          logIfDifferent("different class", p1, p2)(false)
          return false
        }

        p1.productIterator.zip(p2.productIterator).forall {
          case (v1: Product,                        v2: Product)                        ⇒ apply(v1, v2)              <| logIfDifferent("Product", v1, v2)
          case (v1: Mutable,                        v2: Mutable)                        ⇒ (v1 == v2)                 <| logIfDifferent("Mutable", v1, v2)
          case (v1: CallbackTo[_],                  v2: CallbackTo[_])                  ⇒ true
          case (v1: Future[_],                      v2: Future[_])                      ⇒ v1.value == v2.value
          case (v1: Traversable[_],                 v2: Traversable[_])                 ⇒ ((v1 eq v2) || (v1 == v2)) <| logIfDifferent("Traversable", v1, v2)
          case (v1: ReusableFn[_, _],               v2: ReusableFn[_, _])               ⇒ true
          case (v1: Function0[_],                   v2: Function0[_])                   ⇒ true //<-- keep functions after Traversable
          case (v1: Function1[_, _],                v2: Function1[_, _])                ⇒ true
          case (v1: Function2[_, _, _],             v2: Function2[_, _, _])             ⇒ true
          case (v1: Function3[_, _, _, _],          v2: Function3[_, _, _, _])          ⇒ true
          case (v1: Function4[_, _, _, _, _],       v2: Function4[_, _, _, _, _])       ⇒ true
          case (v1: Function5[_, _, _, _, _, _],    v2: Function5[_, _, _, _, _, _])    ⇒ true
          case (v1: Function6[_, _, _, _, _, _, _], v2: Function6[_, _, _, _, _, _, _]) ⇒ true
          case (v1: String,                         v2: String)                         ⇒ ((v1 eq v2) || v1 == v2)   <| logIfDifferent("String", v1, v2)
          case (v1: AnyRef,                         v2: AnyRef)                         ⇒ (v1 eq v2)                 <| logIfDifferent("Anyref", v1, v2)
        }
      }
    }
  }

  def apply[P: Reusability, S: Reusability, B, N <: TopNode]
           (name: String, forceDebug: Boolean = false) =
    (c: ReactComponentB[P, S, B, N]) ⇒
      if (enableDebug || forceDebug)
        c.configure(Reusability.shouldComponentUpdateWithOverlay)
         .configure(Reusability.shouldComponentUpdateAndLog(name))
         .configure(LogLifecycle.short)
      else
        c.configure(Reusability.shouldComponentUpdate)

  def inferred[P <: Product, S <: Product, B, N <: TopNode]
              (name: String, forceDebug: Boolean = false) =
    apply[P, S, B, N](name, forceDebug)(InferredReusability[P], InferredReusability[S])

  def inferredNoState[P <: Product, B, N <: TopNode]
                     (name: String, forceDebug: Boolean = false) =
    apply[P, Unit, B, N](name, forceDebug)(InferredReusability[P], Reusability[Unit])
}
