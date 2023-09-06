package normalpractise

import scala.annotation.tailrec

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)

  def product(ds: List[Double]): Double = ds match
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
}

def tail[A](ds: List[A]): List[A] = ds match
  case Nil => Nil // sys.error("tail of empty list")
//  case Cons(head, Nil)  => Nil
  case Cons(_, tail) => tail

def setHead[A](ds: List[A], newHead: A): List[A] = ds match
  case Nil => Cons(newHead, Nil)
//  case Cons(head, Nil)  => Cons(newHead, Nil)
  case Cons(_, tail) => Cons(newHead, tail)

// my answer
//def drop[A](l: List[A], n: Int): List[A] = {
//  if (n <= 0 | l == Nil) l
//  else drop(tail(l), n - 1)
//}
// book answer
def drop[A](l: List[A], n: Int): List[A] =
  if n <= 0 then l
  else
    l match
      case Nil        => Nil
      case Cons(_, t) => drop(t, n - 1)

// my answer can remove all matched data
//def dropWhile[A](ds: List[A], f: A => Boolean): List[A] = ds match
//  case Nil                         => Nil
//  case Cons(head, tail) if f(head) => dropWile(tail, f)
//  case Cons(head, tail)            => Cons(head, dropWile(tail, f))
// book answer only can remove the matched data until no matched
def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
  l match
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _                  => l

def append[A](ds: List[A], ds2: List[A]): List[A] = ds match
  case Nil              => ds2
  case Cons(head, tail) => Cons(head, append(tail, ds2))

//my answer
//def init[A](ds: List[A]): List[A] = ds match
//  case Nil              => Nil
//  case Cons(_, Nil)  => Nil
//  case Cons(head, tail) => Cons(head, init(tail))
// book answer
def init[A](ds: List[A]): List[A] = {
  import collection.mutable.ListBuffer
  val buf = new ListBuffer[A]

  @tailrec
  def go(cur: List[A]): List[A] = cur match
    case Nil              => Nil
    case Cons(_, Nil)     => List(buf.toList*)
    case Cons(head, tail) => buf += head; go(tail)

  go(ds)
}

def sum2(ints: List[Int]): Int =
  foldRight(ints, 0)((x, y) => x + y)

def product2(ds: List[Double]): Double =
  foldRight(ds, 1.0)((x, y) => x * y)

def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match
  case Nil         => z
  case Cons(x, xs) => f(x, foldRight(xs, z)(f))

//my answer, it is not follow requirement
//def length[A](as: List[A]): Int = as match
//  case Nil           => 0
//  case Cons(_, tail) => 1 + length(tail)
//book answer
def length[A](l: List[A]): Int =
  foldRight(l, 0)((x, acc) => acc + 1)

// my answer
/*
1. I consider add a factor to store the result from 1st element to last element
2. so go method looks like def go(as: List[A], z: B)(f: (B, A) => B)(acc: B): B
3. it needs to superpose base on factor,
for example, if List is List(1, 2, 3, 4, 5) and z is 0,
so 1st round is 0 + 1,
2nd round the result is 1 + 2 .....
the last round is 10 + 5
then can found z is not used in go method since it is treated as factor when call go method. so remove z from params of go method
* */
def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
  @tailrec
  def go(as: List[A])(f: (B, A) => B)(acc: B): B = as match
    case Nil => acc
    case Cons(head, tail) => go(tail)(f)(f(acc, head))

  go(as)(f)(z)
}
// book answer
//def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match
//  case Nil         => z
//  case Cons(h,t) => foldLeft(t, f(z,h))(f)

object chapter3 extends App {
  val testList = List(1, 2, 3, 4, 5);
  val testListWithoutOrder = List(1, 4, 3, 2, 5);
// 3.1
//  val x = testList match {
//    case Cons(x, Cons(2, Cons(4, _))) => x
//    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
//    case Cons(h, t) => h + List.sum(t)
//    case _ => 101
//  }
//
//  println(x)

//3.2
//  println(tail(Nil)) // Nil //throw exception
//  println(tail(List(1))) // Nil
//  println(tail(testList)) // Cons(2,Cons(3,Cons(4,Cons(5,Nil))))

//3.3
//  println(setHead(Nil, 9)) // Cons(9,Nil)
//  println(setHead(List(1), 9)) // Cons(9,Nil)
//  println(setHead(testList, 9)) // Cons(9,Cons(2,Cons(3,Cons(4,Cons(5,Nil)))))

//3.4
//  println(drop(Nil, 3)) // Nil
//  println(drop(List(1), 3)) // Nil
//  println(drop(testList, 3)) // Cons(4,Cons(5,Nil))

//3.5
//  val condition: Int => Boolean = x => x < 3
//  println(dropWhile(Nil, condition)) // Nil
//  println(dropWhile(List(1), condition)) // Nil
//  println(dropWhile(testListWithoutOrder, condition)) // Cons(3,Cons(4,Cons(5,Nil))) // Cons(4,Cons(3,Cons(2,Cons(5,Nil))))

//3.6
//  println(init(Nil)) // Nil
//  println(init(List(1))) // Nil
//  println(init(testListWithoutOrder)) // Cons(1,Cons(4,Cons(3,Cons(2,Nil))))

//3.8
/*
if execute foldRight(testList, 0)((x, y) => x + y) the steps like below
1. Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Nil))))) is not Nil, then pass x = 1, y = Cons(2,Cons(3,Cons(4,Cons(5,Nil)))) to next iteration
2. 1 + foldRight(Cons(2,Cons(3,Cons(4,Cons(5,Nil))))) is not Nil, then x = 2, y = Cons(3,Cons(4,Cons(5,Nil)))
3. 1 + (2 + foldRight(Cons(3,Cons(4,Cons(5,Nil)))) is not Nil, then x = 3, y = Cons(4,Cons(5,Nil))
4. 1 + (2 + (3 + foldRight(Cons(4,Cons(5,Nil))) is not Nil, then x = 4, y = Cons(5,Nil)
5. 1 + (2 + (3 + (4 + (5 + foldRight(Cons(Nil)))), then x = 5, y = Cons(Nil)
6. 1 + (2 + (3 + (4 + (5 + 0)))), then stop iteration
7. 1 + (2 + (3 + (4 + (5 + 0)))) = 15
* */
//  println(foldRight(testList, Nil: List[Int])(Cons(_, _))) //Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Nil)))))

//3.9
//  println(length(testList))

//3.10
/* Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Nil))))) is not Nil, then pass x = Cons(1,Cons(2,Cons(3,Cons(4,Cons(5,Nil)))))
if execute foldLeft(testList, 0)((x, y) => x + y) the steps like below
1.
* */
//  println(foldLeft(testList, 0)((x, y) => x + y)) // 15
}
