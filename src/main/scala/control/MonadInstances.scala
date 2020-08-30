package control

import data.IdInstances.Id
import data.{IdInstances, Monoid}

object MonadInstances {

  implicit val monadOptionInstance: Monad[data.Option] = new Monad[data.Option] {
    override def pure[A](x: A): data.Option[A] = data.Option.pure(x)

    override def flatMap[A, B](xs: data.Option[A])(f: A => data.Option[B]): data.Option[B] =
      xs.flatMap(f)
  }

  implicit val monadTryInstance: Monad[Try] = new Monad[Try] {
    override def pure[A](x: A): Try[A] = Try.pure(x)

    override def flatMap[A, B](xs: Try[A])(f: A => Try[B]): Try[B] =
      xs.flatMap(f)
  }

  implicit val monadListInstance: Monad[data.List] = new Monad[data.List] {
    override def pure[A](x: A): data.List[A] = data.List.pure(x)

    override def flatMap[A, B](xs: data.List[A])(f: A => data.List[B]): data.List[B] =
      xs.flatMap(f)
  }

  implicit def monadEitherInstance[A]: Monad[({ type λ[B] = data.Either[A, B] })#λ] = new Monad[({ type λ[B] = data.Either[A, B] })#λ] {
    override def pure[B](x: B): data.Either[A, B] = data.Either.pure(x)

    override def flatMap[B, C](xs: data.Either[A, B])(f: B => data.Either[A, C]): data.Either[A, C] = xs.flatMap(f)
  }

  implicit def monadWriterInstance[L](implicit m: Monoid[L]): Monad[({ type λ[A] = Writer[L, A] })#λ] = new Monad[({ type λ[A] = Writer[L, A] })#λ] {
    override def pure[A](x: A): Writer[L, A] = Writer.pure(x)

    override def flatMap[A, B](xs: Writer[L, A])(f: A => Writer[L, B]): Writer[L, B] = xs.flatMap(f)
  }

  implicit def monadStateInstance[S]: Monad[({ type λ[A] = State[S, A] })#λ] = new Monad[({ type λ[A] = State[S, A] })#λ] {
    override def pure[A](a: A): State[S, A] = State.pure(a)

    override def flatMap[A, B](xs: State[S, A])(f: A => State[S, B]): State[S, B] = xs.flatMap(f)
  }

  implicit def monadReaderInstance[E]: Monad[({ type λ[A] = Reader[E, A] })#λ] = new Monad[({ type λ[A] = Reader[E, A] })#λ] {
    override def pure[A](x: A): Reader[E, A] = Reader.pure(x)

    override def flatMap[A, B](xs: Reader[E, A])(f: A => Reader[E, B]): Reader[E, B] = xs.flatMap(f)
  }

  implicit val monadIdInstance: Monad[Id] = IdInstances.id

  implicit val monadIOInstance: Monad[IO] = new Monad[IO] {
    override def pure[A](x: A): IO[A] = IO.pure(x)

    override def flatMap[A, B](xs: IO[A])(f: A => IO[B]): IO[B] =
      xs.flatMap(f)
  }

  implicit val function0Instance: Monad[Function0] = new Monad[Function0] {
    override def pure[A](x: A): () => A = () => x

    override def flatMap[A, B](xs: () => A)(f: A => () => B): () => B = f(xs())
  }
}
