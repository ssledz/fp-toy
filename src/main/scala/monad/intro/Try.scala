package monad.intro

trait Try[+A] {

  def flatMap[B](f: A => Try[B]): Try[B] = this match {
    case Success(a) => f(a)
    case failure => failure.asInstanceOf[Try[B]]
  }

  def map[B](f: A => B): Try[B] = flatMap(a => Try.pure(f(a)))

}

object Try {

  def apply[A](a: => A): Try[A] =
    try {
      Success(a)
    } catch {
      case e: Exception => Failure(e)
    }

  def pure[A](a: => A): Try[A] = apply(a)

}

case class Success[A](a: A) extends Try[A]

case class Failure(exception: Exception) extends Try[Nothing]