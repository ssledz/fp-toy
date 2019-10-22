package applicative

import data.{Functor, Option}

trait Applicative[F[_]] extends Functor[F] {

  def ap[A, B](fab: F[A => B])(fa: F[A]): F[B]

  def pure[A](a: A): F[A]

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {

    val fabc: F[A => B => C] = pure(f.curried)

    val fbc: F[B => C] = ap(fabc)(fa)

    val fc: F[C] = ap(fbc)(fb)

    fc
  }

  def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, pure(()))((a, _) => f(a))

}

object Applicative {
  def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = F
}

object ApplicativeSyntax {

  implicit class ApplicativeOps[A, B, F[_]](val fab: F[A => B]) extends AnyVal {
    def ap(fa: F[A])(implicit F: Applicative[F]): F[B] = F.ap(fab)(fa)
  }

  implicit class ApplicativeTupleOps[A, B, F[_]](val tuple: (F[A], F[B])) extends AnyVal {
    def mapN[C](f: (A, B) => C)(implicit F: Applicative[F]): F[C] = F.map2(tuple._1, tuple._2)(f)
  }

}

object ApplicativeInstances {

  implicit val optionApplicative: Applicative[Option] = new Applicative[Option] {

    override def ap[A, B](fab: Option[A => B])(fa: Option[A]): Option[B] =
      for {
        f <- fab
        a <- fa
      } yield f(a)

    override def pure[A](a: A): Option[A] = Option.pure(a)
  }

}
