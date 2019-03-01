package sandbox.introduction

trait Printable[A] {
  def format(value: A): String
}

object Printable {

  implicit class PrintableOps[A](value: A) {
    def format(implicit p: Printable[A]): String = p.format(value)

    def print(implicit p: Printable[A]): Unit = println(p.format(value))
  }

}

object PrintableInstances {
  implicit val stringPrintable: Printable[String] = new Printable[String] {
    override def format(value: String): String = value
  }
  implicit val intPrintable: Printable[Int] = new Printable[Int] {
    override def format(value: Int): String = value.toString
  }
  implicit val catPrintable: Printable[Cat] = new Printable[Cat] {
    override def format(value: Cat): String = s"${value.name} is a ${value.age} year-old ${value.color} cat."
  }
}

final case class Cat(name: String, age: Int, color: String)

object PrintableTest extends App {

  import Printable._
  import PrintableInstances._

  "Hello kitten".print
  Cat("Pink", 4, "white").print
}


object PrintableWithCats extends App {

  import cats.Show
  import cats.instances.int._
  import cats.instances.string._
  import cats.syntax.show._

  val showInt: Show[Int] = Show.apply[Int]
  val showString: Show[String] = Show.apply[String]
  implicit val showCat: Show[Cat] = Show.show(value => s"${value.name} is a ${value.age} year-old ${value.color} cat.")
  println(Cat("Pink", 4, "white").show)
}
