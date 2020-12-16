// Generalises all those "toFoo" and "toBar" methods everybody writes.
//
// Also includes a Labelled type to accomodate case class and sealed trait
// conversions into tuples and unions that retain field names.

package wheel.to

object `package` {

  implicit class ontoOps[A](val a: A) extends AnyVal {
    def to[B](implicit O: Onto[A, B]): B = O.to(a)
  }
}



// Intended to be used with infix, e.g. 'Foo Labelled "foo"'
final class Labelled[A, L <: String](val value: A) extends AnyVal
object Labelled {
  def apply[A, L <: String](a: A): A Labelled L = new Labelled(a)
}

// surjective (one way)
trait Onto[A, B] {
  type Domain = A
  type Target = B
  def to(A: A): B
}

// injection (invertible)
//
// from(to(a)) == a
trait Into[A, B] extends Onto[A, B] {
  def from(b: B): A
}

object Onto extends OntoLowPriority {
  def apply[A, B](implicit O: Onto[A, B]): Onto[A, B] = O

  // everything is an injection to itself
  implicit def id[A]: Into[A, A] = new Into[A, A] {
    def to(a: A): A = a
    def from(a: A): A = a
  }
  // injections are surjections in both directions
  implicit def back[A, B](implicit I: Into[A, B]): Onto[B, A] = new Onto[B, A] {
    def to(b: B): A = I.from(b)
  }

  // primitives can be upscaled but not downscaled
  implicit val float: Onto[Float, Double] = new Onto[Float, Double] {
    def to(a: Float): Double = a.toDouble
  }
  implicit val int: Onto[Int, Long] = new Onto[Int, Long] {
    def to(a: Int): Long = a.toLong
  }

  // TODO evidence for <:< and Functor / Contravariant
  // TODO tuple generators (Into)
}

private[to] trait OntoLowPriority {
  implicit def transitive[A, B, C](implicit F1: Onto[A, B], F2: Onto[B, C]): Onto[A, C] = new Onto[A, C] {
    def to(a: A): C = F2.to(F1.to(a))
  }
}

object Into {
  def apply[A, B](implicit I: Into[A, B]): Into[A, B] = I

  // all the instances and evidence are on the companion of Onto because
  // otherwise we'd never see them when looking for Into instances.

}
