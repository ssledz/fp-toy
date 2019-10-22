package control

import data._

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

  def sequenceM[M[_], A](xs: data.List[M[A]])(implicit M: Monad[M]): M[data.List[A]] = xs match {
    case Nil => M.pure(data.List.empty)
    case h :: t =>
      M.flatMap(h) { x =>
        M.map(sequenceM(t)) { xxs =>
          data.::(x, xxs)
        }
      }
  }

  def replicateM[M[_], A](n: Int)(x: M[A])(implicit M: Monad[M]): M[data.List[A]] = {
    @tailrec
    def go(count: Int, acc: data.List[M[A]]): data.List[M[A]] =
      if (count == 0) acc
      else go(count - 1, data.::(x, acc))

    sequenceM(go(n, data.List.empty))
  }

}

object MonadSyntax {

  implicit class MonadOps[A, M[_]](val xs: M[A]) extends AnyVal {

    def flatMap[B](f: A => M[B])(implicit m: Monad[M]): M[B] = m.flatMap(xs)(f)

    def map[B](f: A => B)(implicit m: Monad[M]): M[B] = m.map(xs)(f)

    def **[B](ys: M[B])(implicit m: Monad[M]): M[(A, B)] = m.map2(xs, ys)((_, _))
  }

}
