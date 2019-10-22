package data

import FunctorInstances._
import FunctorSyntax._

object FunctionExample extends App {

  type IntTo[A] = Int => A

  val int2String: IntTo[String] = _.toString

  val parseInt: String => Int = _.toInt

  val identity = int2String map parseInt

  println(identity(123))

}
