package control

import control.Try._

sealed trait Try[+A] {

  def flatMap[B](f: A => Try[B]): Try[B] = this match {
    case Success(a) => f(a)
    case failure => failure.asInstanceOf[Try[B]]
  }

  def map[B](f: A => B): Try[B] = flatMap(a => Try.pure(f(a)))

  def getOrElse[B >: A](default: B): B = this match {
    case Failure(_) => default
    case Success(a) => a
  }

}

object Try {

  def apply[A](a: => A): Try[A] =
    try {
      Success(a)
    } catch {
      case e: Throwable => Failure(e)
    }

  def pure[A](a: => A): Try[A] = apply(a)

  case class Success[A](a: A) extends Try[A]

  case class Failure(err: Throwable) extends Try[Nothing]

}
