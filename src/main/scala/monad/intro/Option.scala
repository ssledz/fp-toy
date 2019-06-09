package monad.intro

trait Option[+A] {

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }

  def map[B](f: A => B): Option[B] = flatMap(a => Some(f(a)))

  def filter(p : A => Boolean) : Option[A] = flatMap(a => if(p(a)) Option.pure(a) else None)

}

object Option {

  def empty[A]: Option[A] = None

  def pure[A](a: A): Option[A] = if (a == null) None else Some(a)

}

case class Some[A](a: A) extends Option[A]

case object None extends Option[Nothing]