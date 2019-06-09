package monad.intro

trait Either[A, B] {

  def flatMap[C](f: B => Either[A, C]): Either[A, C] = this match {
    case l@Left(_) => l.asInstanceOf[Either[A, C]]
    case Right(b) => f(b)
  }

  def map[C](f: B => C): Either[A, C] = flatMap(b => Either.pure(f(b)))

}

object Either {

  def pure[A, B](b: B): Either[A, B] = Right(b)

}

case class Left[A, B](left: A) extends Either[A, B]

case class Right[A, B](right: B) extends Either[A, B]
