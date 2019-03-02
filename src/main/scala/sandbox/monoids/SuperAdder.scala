package sandbox.monoids

object SuperAdder extends App {

  import cats.Monoid
  import cats.instances.int._
  import cats.instances.double._
  import cats.syntax.semigroup._

  implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
    override def empty: Order = Order(Monoid[Double].empty, Monoid[Double].empty)

    override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }

  def add[A: Monoid](items: List[A]): A = items.foldLeft(Monoid[A].empty)(_ |+| _)

  println(add(List(1, 2, 3, 4, 5)))
  println(add(List(Order(10, 2), Order(5, 4))))
}

case class Order(totalCost: Double, quantity: Double)
