package control

import control.free.Free
import control.free.Free.runF
import control.free.NaturalTransformation.~>
import control.MonadInstances._

object FreeMonadExample extends App {

  sealed trait ConsoleApi[A] {
    def toFun0: () => A
  }
  case object ReadLine extends ConsoleApi[String] {
    def toFun0: () => String = () => scala.io.StdIn.readLine()
  }
  case class PrintLine(line: String) extends ConsoleApi[Unit] {
    override def toFun0: () => Unit = () => println(line)
  }

  val readLine: Free[ConsoleApi, String] = Free.suspend(ReadLine)

  def printLine(out: String): Free[ConsoleApi, Unit] = Free.suspend(PrintLine(out))

  object ConsoleApiFunction0 extends (ConsoleApi ~> Function0) {
    def apply[A](fa: ConsoleApi[A]): () => A = fa.toFun0
  }

  val program = for {
    _ <- printLine("What is your age ?")
    name <- readLine
  } yield name

  println(program.map(_.toInt))

  val value = runF(program.map("Your age is: " + _))(ConsoleApiFunction0)

  println(value())
}
