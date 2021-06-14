package checkout

import checkout.basket.{Basket, CheckOutLine}
import checkout.promotions.Promotion

class CheckOut(
    promotions: List[Promotion],
    priceList: Map[Fruit, BigDecimal]
) {

  def calculateTotal(items: String*): BigDecimal = {

    val fruits = items.groupMapReduce(Fruit)(_ => 1)(_ + _)

    val checkOutLines = for {
      (fruit, qty) <- fruits
      price <- priceList.get(fruit)
    } yield checkout.basket.CheckOutLine(fruit, qty, price, 0)

    val basket = Basket(checkOutLines.toList)

    val discountedBasket = promotions.foldLeft(basket)((b, promotion) => promotion.calculateDiscount(b))

    discountedBasket.total
  }
}









