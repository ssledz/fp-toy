package monad.intro

import scala.annotation.tailrec

trait Eq[A] {

  def ===(x: A, y: A): Boolean

}

object Eq {
  def apply[A](implicit eq: Eq[A]): Eq[A] = eq

  def fromFun[A](f: (Any, Any) => Boolean): Eq[A] = (x: A, y: A) => f(x, y)

  def fromEquals[A]: Eq[A] = (x: A, y: A) => x.equals(y)

}

object EqSyntax {

  implicit class EqOps[A](val a: A) extends AnyVal {
    def ===(b: A)(implicit eq: Eq[A]): Boolean = eq === (a, b)
  }

}

object EqInstances {

  implicit def eqListInstance[A]: Eq[List[A]] = (x: List[A], y: List[A]) => {
    @tailrec
    def eq(l: List[A], r: List[A]): Boolean = (l, r) match {
      case (hl :: tl, hr :: tr) if hl == hr => eq(tl, tr)
      case (Nil, Nil) => true
      case _ => false
    }

    eq(x, y)
  }

  implicit def eqEitherInstance[A, B]: Eq[Either[A, B]] = Eq.fromEquals

  implicit def eqOptionInstance[A]: Eq[Option[A]] = Eq.fromEquals

  implicit def eqTryInstance[A]: Eq[Try[A]] = Eq.fromEquals

  implicit def eqWriterInstance[L, A]: Eq[Writer[L, A]] = Eq.fromEquals

}

object TestEq extends App {

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