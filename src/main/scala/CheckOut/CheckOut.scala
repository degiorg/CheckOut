package CheckOut

object CheckOut {
  def process(items: String*): BigDecimal = {

    val basket = Basket(items.groupMapReduce(Fruit)(_ => 1)(_ + _))
    //check if product exist in price list
    val checkOut = CheckOutList(
      basket.fruits.foldLeft(CheckOutList.empty) {
        case (list, (name, qty)) =>
          val price = getPrice(name)
          if (price != PriceList.notFound) {
            val offers = Deal.discountList.filter(x => x._1 == name).toList
            CheckOutLine(name, qty, price, offers) :: list
          } else {
            list
          }
      }
    )
    if (checkOut.lineList != CheckOutList.empty) {
      val total = checkOut.lineList.foldLeft(SubTotal.empty) {
        case (tot, checkOutLine) =>
          val discount = checkOutLine.offers.foldLeft(SubTotal.empty) {
            case (totDisc, (_, discountType)) =>
              discountType match {
                case i: Int =>
                  val value =
                    discountBuyX(
                      checkOutLine.qty,
                      i,
                      checkOutLine.price
                    )
                  totDisc + value

                case fruitBundle: Fruit =>
                  val findFruit = basket.fruits.getOrElse(fruitBundle, "")
                  val value: BigDecimal = findFruit match {
                    case "" => 0
                    case _ =>
                      val fruitBundleQty: Int =
                        basket.fruits.getOrElse(fruitBundle, 0)
                      val valueDisc: BigDecimal = fruitBundleQty match {
                        case 0 => 0
                        case _ =>
                          calcBundle(
                            (checkOutLine.fruit, checkOutLine.qty),
                            (fruitBundle, fruitBundleQty)
                          )
                      }
                      valueDisc
                  }
                  totDisc + value

                case _ => totDisc
              }
          }
          tot + checkOutLine.subTotal - discount
      }

      val bundleFound =
        for (
          line <- checkOut.lineList; bundle <- Bundle.list
          if (line.fruit == bundle._1) && (basket.fruits.getOrElse(
            bundle._2,
            ""
          ) != "")
        ) yield line

      if (bundleFound.isEmpty) {
        val finalTotal = total - (total * 10 / 100)
        finalTotal
      } else total
    } else 0

  }

  def subtotal(qty: Int, price: BigDecimal): BigDecimal = qty * price
  def getPrice(fruit: Fruit): BigDecimal =
    PriceList.priceList.getOrElse(fruit, PriceList.notFound)
  def getDiscount(fruit: Fruit): Int =
    BuyXGet1Free.list.getOrElse(fruit, 0)
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
  def calcBundle(fruit1: (Fruit, Int), fruit2: (Fruit, Int)): BigDecimal = {
    val fruit1LinePrice = bundleBuffer(fruit1)
    val fruit2LinePrice = bundleBuffer(fruit2)
    if (fruit1LinePrice > fruit2LinePrice) fruit2LinePrice else fruit1LinePrice
  }
  def bundleBuffer(fruit: (Fruit, Int)): BigDecimal = {
    val price = getPrice(fruit._1)
    val discount = getDiscount(fruit._1)
    subtotal(fruit._2, price) - discountBuyX(fruit._2, discount, price)
  }
}

case class SubTotal(amount: BigDecimal)
object SubTotal {
  val empty: BigDecimal = 0.0
}

case class Fruit(name: String)

case class Basket(fruits: Map[Fruit, Int])

case class CheckOutList(lineList: List[CheckOutLine])
object CheckOutList {
  val empty: List[CheckOutLine] = List(
    CheckOutLine(Fruit(""), 0, 0.0, List((Fruit(""), "")))
  )
}
case class CheckOutLine(
    fruit: Fruit,
    qty: Int,
    price: BigDecimal,
    offers: List[(Fruit, Any)]
) {
  val subTotal: BigDecimal = qty * price
}

object PriceList {
  val priceList: Map[Fruit, BigDecimal] = Map(
    Fruit("Apple") -> 0.6,
    Fruit("Orange") -> 0.25,
    Fruit("Banana") -> 0.20
  )
  val notFound: BigDecimal = 0.0
}

class Deal
object Deal {
  val discountList: Seq[(Fruit, Any)] = {
    (BuyXGet1Free.list.toList :: Bundle.list.toList :: Nil).flatten
  }
}
case class BuyXGet1Free(offers: Map[Fruit, Int]) extends Deal
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
