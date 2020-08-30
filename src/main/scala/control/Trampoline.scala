package control

import control.Trampoline.{Return, Suspend}

import scala.annotation.tailrec

sealed trait Trampoline[A] {

  def runT: A = {
    @tailrec
    def go(curr: Trampoline[A]): A = curr match {
      case Return(value) => value
      case Suspend(f) => go(f())
    }
    go(this)
  }

}
object Trampoline {

  case class Return[A](value: A) extends Trampoline[A]
  case class Suspend[A](f: () => Trampoline[A]) extends Trampoline[A]

}
