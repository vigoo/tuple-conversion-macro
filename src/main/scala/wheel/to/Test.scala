package wheel.to

import Macro._

object Test extends App {

  case class Something(name: String, value: Int)

  object Something {
    val tup = IntoTuple[Something]
  }

  println(Something.tup.to(Something("hello", 10)))
}
