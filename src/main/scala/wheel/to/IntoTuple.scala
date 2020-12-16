package wheel.to

import wheel.to.GeneratedInto

trait IntoTuple[Prod] {
  type Tup

  def to(A: Prod): Tup

  def from(b: Tup): Prod
}

object IntoTuple {
  type Aux[Prod, Tup0] = IntoTuple[Prod] {type Tup = Tup0}

  def apply[Prod](implicit intoTuple: IntoTuple[Prod]): IntoTuple[Prod] = intoTuple

  implicit def fromInto[Prod, Tup0](implicit into: GeneratedInto[Prod, Tup0]): Aux[Prod, Tup0] =
    new IntoTuple[Prod] {
      override type Tup = Tup0

      override def to(A: Prod): Tup = into.to(A)

      override def from(b: Tup): Prod = into.from(b)
    }
}
