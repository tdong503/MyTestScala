package teampractise

import scala.annotation.tailrec
import scala.io.StdIn.readLine

case class MyIO[A](trunk: () => A) {
  def flatMap[B](f: A => MyIO[B]): MyIO[B] = MyIO(() => f(trunk()).trunk())
  //
  //  val a: String => Int = {
  //    println(123)
  //    (a:String) => a.length
  //  }
  def map[B](f: A => B): MyIO[B] = MyIO(() => f(trunk()))

  def loopWhile(condition: A => Boolean): MyIO[Unit] = {
    val input = trunk()
    if (condition(input)) {
      MyIO(() => loopWhile(condition).trunk())
    } else
      MyIO(() => ())
  }

  def loopWhile1(condition: A => Boolean): MyIO[Unit] = {
    @tailrec
    def go(condition: A => Boolean, input: A): MyIO[Unit] = {
      if (condition(input))
        go(condition, run)
      else
        MyIO(() => ())
    }

    MyIO(() => go(condition, run))
  }

  def run: A = trunk()
}

//val a
//
//object MyIO {
//  def apply[A](a: => A): MyIO[A] = new MyIO(() => a)
//  def apply[A](a:  A): MyIO[A] = new MyIO(() => a)
//
//  def run[A](a: => A): MyIO[Unit] =
//
//}
//
//first have a method that used to read input if not empty then continue, otherwise stop
//def printOkUntilEmptyInput(): Unit = {
//  val input = scala.io.StdIn.readLine()
//
//  if (input != null && input.trim.nonEmpty) {
//    println("ok")
//    printOkUntilEmptyInput()
//  }
//}
//
//then modify to this method, add condition as param. it works. Then delete
//val input = scala.io.StdIn.readLine()
//and use program as param to have a test
//def printOkUntilEmptyInput1(condition: String => Boolean)(program: => MyIO[String]): MyIO[Unit] = {
//    val input = program.run()
//    if (condition(input)) {
//      printOkUntilEmptyInput1(condition)(program)
//    }
//    else
//      MyIO(() => ())
//}

object NoCats extends App {
  // put this line at the top, can know when print execute
  println("======start=======")

  // first time add Option, it works but for-comprehension does not work. that need to write a new object and rewrite flatMap and map
  //  def write(message: String): Option[Unit] = {
  //    Some(println(message))
  //  }
  //
  //  def read: Option[String] = {
  //    Some(readLine())
  //  }

  // then add these methods to return MyIO type
  def write(message: String): MyIO[Unit] = {
    MyIO(() => println(message))
  }

  def read: MyIO[String] = {
    MyIO(() => readLine())
  }

  val program: MyIO[String] =
    for
      _ <- write("Input")
      input <- read
      _ <- write("Output:")
      _ <- write(input)
    yield input
  // 此行以上没有任何Effect

//  program.run
  // Console输出: Input:
  // Console等待输入
  // Console输出: Output:
  // Console输出: 上一行输入结果
//  println("======end=======")

//  def condition(input: String): Boolean = input.trim.nonEmpty
  //  loopWhile(condition)(program).run()
  //  printOkUntilEmptyInput1(_.trim.nonEmpty)(program).run() // can work
  program.loopWhile1(_.trim.nonEmpty).run // can work
//  write("test").loopWhile(_ => true).run // test for stack unsafe
//  write("test").loopWhile1(_ => true).run // test for stack safe
  // 进行上述行为的循环直到输入空值

}
