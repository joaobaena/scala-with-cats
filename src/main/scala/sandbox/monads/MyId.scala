package sandbox.monads

import cats.Id

object MyId extends App {
  def pure[A](value: A): Id[A] = value

  def map[A, B](initial: Id[A])(func: A => B): Id[B] = pure(func(initial))

  def flatMap[A, B](initial: Id[A])(func: A => Id[B]): Id[B] = func(initial)

  val str: Id[String] = pure("Hello")
  println(map(str)(x => x.toUpperCase))
  val int: Id[Int] = pure(42)
  println(map(int)(x => x / 7))
  println(map(int)(x => x / 2))
}
