package control

import utils.Assertion._

object StateExample {

  type Registers = List[Int]

  type StackState[A] = State[(Registers, List[String]), A]

  def main(args: Array[String]): Unit = {

    def ret: StackState[Int] = State {
      case (regs, stack) =>
        val res = if (stack.isEmpty) regs.headOption.getOrElse(0) else stack.head.toInt
        ((regs, stack), res)
    }

    def continue(program: StackState[Int]): StackState[Int] =
      for {
        stack <- State.get[(Registers, List[String])].map(_._2)
        res <- if (stack.size == 0) ret else program
      } yield res

    def opF(f: Registers => (Int, Registers)): StackState[Int] =
      for {
        state <- State.get[(Registers, List[String])]
        (regs, stack) = state
        (res, regsN) = f(regs)
        _ <- State.set((regsN, res.toString :: stack.tail))
      } yield res

    def op(f: Int => Int): StackState[Int] = opF {
      case a :: tail => (f(a), tail)
      case _ => throw new IllegalStateException()
    }

    def biOp(f: (Int, Int) => Int): StackState[Int] = opF {
      case a :: b :: tail => (f(a, b), tail)
      case _ => throw new IllegalStateException()
    }

    val numberOp: StackState[Int] = for {
      state <- State.get[(Registers, List[String])]
      (regs, stack) = state
      res = stack.head.toInt
      _ <- State.set((res :: regs, stack.tail))
    } yield res

    lazy val program: StackState[Int] = for {
      state <- State.get[(Registers, List[String])]
      _ <- state match {
        case (_, h :: _) =>
          h match {
            case "+" => biOp(_ + _)
            case "*" => biOp(_ * _)
            case "-" => op(x => -x)
            case _ => numberOp
          }
        case _ => ret
      }
      res <- continue(program)
    } yield res

    program.runA((List.empty, List.empty)) === 0
    program.runA((List.empty, List("1", "2"))) === 2
    program.runA((List.empty, List("1", "2", "+"))) === 3
    program.runA((List.empty, List("1", "2", "+", "1", "+"))) === 4
    program.runA((List.empty, List("3", "1", "2", "+", "*"))) === 9
    program.runA((List.empty, List("3", "1", "2", "+", "*", "-"))) === -9
    program.runA((List.empty, List("3", "1", "2", "+", "-", "*"))) === -9

  }

}
