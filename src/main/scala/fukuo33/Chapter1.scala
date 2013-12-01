package fukuo33

import scala.collection.immutable.{List => ScalaList}


/**
 * Created with IntelliJ IDEA.
 * User: fukuo33
 * Date: 2013/10/14
 * Time: 15:44
 * To change this template use File | Settings | File Templates.
 */
object Chapter1 extends App {

  println("Hello Chapter1 !")

}

class Coffee(val price: Double = 460)
case class CreditCard()

// --- side effect example 1 ---
/*
class Cafe {
  def buyCoffee(cc: CreditCard): Coffee = {
    val cup = new Coffee()
    cc.charge(cup.price)
    cup }
}
*/

// --- side effect example 2 ---
/*
class Cafe {
  def buyCoffee(cc: CreditCard, p: Payments): Coffee = {
    val cup = new Coffee()
    p.charge(cc, cup.price)
    cup
  }
}
*/

// --- Functional solution ---
case class Charge(cc: CreditCard, amount: Double) {
  def combine(other: Charge): Charge =
    if (cc == other.cc)
      Charge(cc, amount + other.amount)
    else
      throw new Exception("Can't combine charges for multiple cards")
}

class Cafe {

  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = new Coffee()
    (cup, Charge(cc, cup.price))
  }

  def buyCoffees(cc: CreditCard, n: Int): (ScalaList[Coffee], Charge) = {
    val purchases: ScalaList[(Coffee, Charge)] = ScalaList.fill(n)(buyCoffee(cc))
    val (coffees, charges) = purchases.unzip
    (coffees, charges.reduce((c1,c2) => c1.combine(c2)))
  }

}