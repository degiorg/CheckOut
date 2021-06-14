package CheckOut

import scala.collection.immutable

class CheckOut(
    offers: Seq[(Fruit, Any)],
    priceList: Map[Fruit, BigDecimal]
) {

  def calculateTotal(items: String*): BigDecimal = {

    val fruits = items.groupMapReduce(Fruit)(_ => 1)(_ + _)

    val checkOutLines = for {
      (fruit, qty) <- fruits
      price <- priceList.get(fruit)
      offersProduct = offers.filter(x => x._1 == fruit).toList
      discount = getDiscount(offersProduct, qty, price)
    } yield CheckOutLine(fruit, qty, price, offersProduct, discount)

    val basket = Basket(checkOutLines.toList)

    if (basket.lineList != Basket.empty) {

      val discBundle = for {
        line <- basket.lineList
        (fruit1, fruit2) <- Bundle.list
        if line.fruit == fruit1
      } yield (fruit1, fruit2)

      val discountBundle: BigDecimal = {
        if (discBundle.nonEmpty) {
          discBundle.foldLeft(SubTotal.empty) {
            case (tot, (fruit1, fruit2)) =>
              val line1 = basket.lineList.filter(x => x.fruit == fruit1)
              val line1Price: BigDecimal =
                if (line1.nonEmpty) line1.head.subTotal else 0
              val line2 = basket.lineList.filter(x => x.fruit == fruit2)
              val line2Price: BigDecimal =
                if (line2.nonEmpty) line2.head.subTotal else 0
              if (line1Price > 0 && line2Price > 0) {
                if (line1Price > line2Price) tot + line2Price
                else tot + line1Price
              } else {
                0
              }
          }
        } else {
          0
        }
      }
      val lastDiscount: BigDecimal = {
        if (discountBundle > 0) discountBundle else basket.total * 0.1
      }
      basket.total - lastDiscount
    } else {
      0
    }
  }

  def getPrice(fruit: Fruit): BigDecimal =
    priceList.getOrElse(fruit, PriceList.notFound)

  def discountBuyX(qty: Int, opLeft: Int, price: BigDecimal): BigDecimal = {
    if (opLeft > 0)
      qty / opLeft match {
        case res if res >= 1 => res * price
        case _               => 0
      }
    else {
      0
    }
  }
  private def getDiscount(
      offers: List[(Fruit, Any)],
      qty: Int,
      price: BigDecimal
  ): BigDecimal = {
    val discount = offers.foldLeft[BigDecimal](0.0) {
      case (tot, (_, discType)) =>
        discType match {
          case i: Int =>
            discountBuyX(qty, i, price) + tot
          case _ => 0 + tot
        }
    }
    discount
  }
}

case class SubTotal(amount: BigDecimal)
object SubTotal {
  val empty: BigDecimal = 0.0
}

case class Fruit(name: String)

case class Basket(lineList: List[CheckOutLine]) {
  val total: BigDecimal = lineList.foldLeft(PriceList.notFound)(_ + _.subTotal)
  val offerList: List[(Fruit, Any)] = lineList.foldLeft(Deal.empty) {
    case (listDisc, line) =>
      (listDisc :: line.offers :: Nil).flatten
  }

}
object Basket {
  val empty: List[CheckOutLine] = List(
    CheckOutLine(Fruit(""), 0, 0.0, List((Fruit(""), "")), 0.0)
  )
}
case class CheckOutLine(
    fruit: Fruit,
    qty: Int,
    unitPrice: BigDecimal,
    offers: List[(Fruit, Any)],
    discount: BigDecimal
) {
  val subTotal: BigDecimal = (qty * unitPrice) - discount
}

object PriceList {

  val notFound: BigDecimal = 0.0
}

trait Deal {
  def calcDiscount
}
object Deal {
  val empty: List[(Fruit, Any)] = List((Fruit(""), ""))
  val discountList: Seq[(Fruit, Any)] = {
    (BuyXGet1Free.list.toList :: Bundle.list.toList :: Nil).flatten
  }
}
case class BuyXGet1Free(fruit: Fruit, x: Int) extends Deal
object BuyXGet1Free {
  val list: Map[Fruit, Int] = Map(
    Fruit("Apple") -> 2,
    Fruit("Orange") -> 3,
    Fruit("Banana") -> 2
  )
}
case class Bundle(offers: Map[Fruit, Fruit]) extends Deal
object Bundle {
  val list: Map[Fruit, Fruit] = Map(
    Fruit("Apple") -> Fruit("Banana")
  )
}
