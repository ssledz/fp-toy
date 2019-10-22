package data

import control.{Try, Writer}

object EqExample extends App {

  import EqInstances._
  import EqSyntax._
  import MonoidInstances._

  println(List[String]("") === List[String](""))

  println(List[String]() == List.empty)

  println(Either.pure("Hello") === Either.pure("Hello"))

  println(Option.pure("Hello") === Option.pure("Hello"))

  println(Try.pure("Hello") === Try.pure("Hello"))

  println(Writer.pure[String, String]("Hello") === Writer.pure[String, String]("Hello"))

}
