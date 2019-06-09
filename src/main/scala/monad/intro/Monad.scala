package monad.intro

import applicative.Applicative
import monad.intro.IdInstances.Id

import scala.annotation.tailrec

trait Monad[M[_]] extends Applicative[M] {

  def pure[A](x: A): M[A]

  def flatMap[A, B](xs: M[A])(f: A => M[B]): M[B]

  def ap[A, B](fab: M[A => B])(fa: M[A]): M[B] = flatMap(fab) { f =>
    flatMap(fa) { a =>
      pure(f(a))
    }
  }

  override def map[A, B](xs: M[A])(f: A => B): M[B] = flatMap(xs)(x => pure(f(x)))

}

object Monad {

  def apply[M[_]](implicit m: Monad[M]): Monad[M] = m

  def sequenceM[M[_], A](xs: List[M[A]])(implicit M: Monad[M]): M[List[A]] = xs match {
    case Nil => M.pure(List.empty)
    case h :: t => M.flatMap(h) { x =>
      M.map(sequenceM(t)) { xxs =>
        ::(x, xxs)
      }
    }
  }

  def replicateM[M[_], A](n: Int)(x: M[A])(implicit M: Monad[M]): M[List[A]] = {
    @tailrec
    def go(count: Int, acc: List[M[A]]): List[M[A]] =
      if (count == 0) acc
      else go(count - 1, ::(x, acc))

    sequenceM(go(n, List.empty))
  }

}

object MonadSyntax {

  implicit class MonadOps[A, M[_]](val xs: M[A]) extends AnyVal {

    def flatMap[B](f: A => M[B])(implicit m: Monad[M]): M[B] = m.flatMap(xs)(f)

    def map[B](f: A => B)(implicit m: Monad[M]): M[B] = m.map(xs)(f)

    def **[B](ys: M[B])(implicit m: Monad[M]): M[(A, B)] = m.map2(xs, ys)((_, _))
  }

}

object MonadInstances {

  implicit val monadOptionInstance: Monad[Option] = new Monad[Option] {
    override def pure[A](x: A): Option[A] = Option.pure(x)

    override def flatMap[A, B](xs: Option[A])(f: A => Option[B]): Option[B] =
      xs.flatMap(f)
  }

  implicit val monadTryInstance: Monad[Try] = new Monad[Try] {
    override def pure[A](x: A): Try[A] = Try.pure(x)

    override def flatMap[A, B](xs: Try[A])(f: A => Try[B]): Try[B] =
      xs.flatMap(f)
  }

  implicit val monadListInstance: Monad[List] = new Monad[List] {
    override def pure[A](x: A): List[A] = List.pure(x)

    override def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] =
      xs.flatMap(f)
  }

  implicit def monadEitherInstance[A]: Monad[({type λ[B] = Either[A, B]})#λ] = new Monad[({type λ[B] = Either[A, B]})#λ] {
    override def pure[B](x: B): Either[A, B] = Either.pure(x)

    override def flatMap[B, C](xs: Either[A, B])(f: B => Either[A, C]): Either[A, C] = xs.flatMap(f)
  }

  implicit def monadWriterInstance[L](implicit m: Monoid[L]): Monad[({type λ[A] = Writer[L, A]})#λ] = new Monad[({type λ[A] = Writer[L, A]})#λ] {
    override def pure[A](x: A): Writer[L, A] = Writer.pure(x)

    override def flatMap[A, B](xs: Writer[L, A])(f: A => Writer[L, B]): Writer[L, B] = xs.flatMap(f)
  }

  implicit def monadStateInstance[S]: Monad[({type λ[A] = State[S, A]})#λ] = new Monad[({type λ[A] = State[S, A]})#λ] {
    override def pure[A](a: A): State[S, A] = State.pure(a)

    override def flatMap[A, B](xs: State[S, A])(f: A => State[S, B]): State[S, B] = xs.flatMap(f)
  }

  implicit def monadReaderInstance[E]: Monad[({type λ[A] = Reader[E, A]})#λ] = new Monad[({type λ[A] = Reader[E, A]})#λ] {
    override def pure[A](x: A): Reader[E, A] = Reader.pure(x)

    override def flatMap[A, B](xs: Reader[E, A])(f: A => Reader[E, B]): Reader[E, B] = xs.flatMap(f)
  }

  implicit val monadIdInstance: Monad[Id] = IdInstances.id

  implicit val monadIOInstance: Monad[IO] = new Monad[IO] {
    override def pure[A](x: A): IO[A] = IO.pure(x)

    override def flatMap[A, B](xs: IO[A])(f: A => IO[B]): IO[B] =
      xs.flatMap(f)
  }

}
