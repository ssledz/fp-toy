package functor

trait Functor[F[_]] {

  def map[A, B](fa: F[A])(f: A => B): F[B]

}

object Functor {
  def apply[F[_]](implicit F: Functor[F]): Functor[F] = F
}

object FunctorSyntax {

  implicit class FunctorOps[A, F[_]](val fa: F[A]) extends AnyVal {

    def map[B](f: A => B)(implicit F: Functor[F]): F[B] = F.map(fa)(f)

  }

}

object FunctorInstances {

  implicit def funFunctorInstance[A] = new Functor[({type λ[B] = A => B})#λ] {
    override def map[B, C](fa: A => B)(f: B => C): A => C = fa andThen f
  }

}
