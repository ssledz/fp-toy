package control

import control.Trampoline.{Return, Suspend}

object TrampolineExample extends App {

  def stackUnsafeEven[A](xs: List[A]): Boolean = {
    def even(xs: List[A]): Boolean = xs match {
      case _ :: ys => odd(ys)
      case Nil => true
    }

    def odd(xs: List[A]): Boolean = xs match {
      case _ :: ys => even(ys)
      case Nil => false
    }
    even(xs)
  }

  def stackSafeEven[A](xs: List[A]): Boolean = {
    def even(xs: List[A]): Trampoline[Boolean] = xs match {
      case _ :: ys => Suspend(() => odd(ys))
      case Nil => Return(true)
    }

    def odd(xs: List[A]): Trampoline[Boolean] = xs match {
      case _ :: ys => Suspend(() => even(ys))
      case Nil => Return(false)
    }
    even(xs).runT
  }

  println(Try(stackUnsafeEven(List.fill(10000)(1))))

  println(stackSafeEven(List.fill(10000)(1)))

}
