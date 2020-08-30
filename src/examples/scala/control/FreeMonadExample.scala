package control

import control.MonadInstances._
import control.free.Free
import control.free.NaturalTransformation.~>

import scala.collection.mutable

object FreeMonadExample extends App {

  sealed trait ConsoleApi[A] {
    def toFun0: () => A
    def toReader: Reader[TestEnvironment, A]
  }
  case object ReadLine extends ConsoleApi[String] {
    def toFun0: () => String = () => scala.io.StdIn.readLine()

    def toReader: Reader[TestEnvironment, String] = Reader { env =>
      val input = env.in.next()
      env.logger.append(s"readLine($input)")
      input
    }
  }
  case class PrintLine(line: String) extends ConsoleApi[Unit] {
    def toFun0: () => Unit = () => println(line)

    def toReader: Reader[TestEnvironment, Unit] = Reader { env =>
      env.logger.append(s"println($line)")
      ()
    }
  }

  val readLine: Free[ConsoleApi, String] = Free.suspend(ReadLine)

  def printLine(out: String): Free[ConsoleApi, Unit] = Free.suspend(PrintLine(out))

  object ConsoleApiFunction0 extends (ConsoleApi ~> Function0) {
    def apply[A](fa: ConsoleApi[A]): () => A = fa.toFun0
  }

  case class TestEnvironment(in: Iterator[String], logger: mutable.Buffer[String] = mutable.Buffer.empty)

  val readerInterpreter = new (ConsoleApi ~> Reader[TestEnvironment, *]) {
    def apply[A](fa: ConsoleApi[A]): Reader[TestEnvironment, A] = fa.toReader
  }

  val program = for {
    _ <- printLine("What is your age ?")
    name <- readLine
  } yield name

  println(program.map(_.toInt))

//  val value = Free.runF(program.map("Your age is: " + _))(ConsoleApiFunction0)
//  println(value())

  val xs: Reader[TestEnvironment, String] = Free.runF(program.map("Your age is: " + _))(readerInterpreter)
  val env = TestEnvironment(Iterator("12"))
  println(xs.run(env))
  println(env)

}
