package control.free

import control.Monad
import control.free.Free.FlatMap
import control.free.NaturalTransformation.~>

sealed trait Free[F[_], A] {

  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)

  def map[B](f: A => B): Free[F, B] = flatMap(a => Free.pure(f(a)))

}

trait NaturalTransformation[F[_], G[_]] {
  def apply[A](fa: F[A]): G[A]
}

object NaturalTransformation {
  type ~>[F[_], G[_]] = NaturalTransformation[F, G]

  def identity[F[_]] = new Identity[F]

  class Identity[F[_]] extends (F ~> F) {
    def apply[A](fa: F[A]): F[A] = fa
  }

}

object Free {

  def defer[F[_], A](a: => A): Free[F, A] = pure(()).flatMap(_ => pure(a))

  def suspend[F[_], A](fa: F[A]): Free[F, A] = Suspend(fa)

  def pure[F[_], A](a: A): Free[F, A] = Return(a)

  def runF[A, F[_], G[_]: Monad](program: Free[F, A])(t: F ~> G): G[A] =
    program match {
      case Return(value) => Monad[G].pure(value)
      case Suspend(resume) => t(resume)
      case FlatMap(fa, f: (Any => Free[F, A])) =>
        fa match {
          case FlatMap(fb, g: (Any => Free[F, Any])) => runF(fb.flatMap(b => g(b).flatMap(f)))(t)
          case Return(value) => runF(f(value))(t)
          case Suspend(resume) => Monad[G].flatMap(t(resume))(a => runF(f(a))(t))
        }
    }

  implicit def monadFreeInstance[F[_]]: Monad[Free[F, *]] = new Monad[Free[F, *]] {
    def pure[A](a: A): Free[F, A] = Free.pure(a)
    def flatMap[A, B](xs: Free[F, A])(f: A => Free[F, B]): Free[F, B] = xs.flatMap(f)
  }

  case class Return[F[_], A](value: A) extends Free[F, A]
  case class Suspend[F[_], A](resume: F[A]) extends Free[F, A]
  case class FlatMap[F[_], A, B](fa: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

}
