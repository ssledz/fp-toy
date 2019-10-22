package control

import control.TailRec._

trait TailRec[A] {

  def flatMap[B](f: A => TailRec[B]): TailRec[B] =
    FlatMap(this, f)

  def map[B](f: A => B): TailRec[B] = flatMap(x => Return(f(x)))
}

object TailRec {

  def unit[A](a: A): TailRec[A] = Return(a)
  def suspend[A](r: => TailRec[A]): TailRec[A] =
    Suspend(() => ()).flatMap(_ => r)

  def run[A](tr: TailRec[A]): A = tr match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) =>
      x match {
        case Return(a) => run(f(a))
        case Suspend(r) => run(f(r()))
        case FlatMap(y, g) => run(y.flatMap(a => g(a).flatMap(f)))
      }
  }

  case class Return[A](a: A) extends TailRec[A]
  case class Suspend[A](resume: () => A) extends TailRec[A]
  case class FlatMap[A, B](sub: TailRec[A], f: A => TailRec[B]) extends TailRec[B]
}
