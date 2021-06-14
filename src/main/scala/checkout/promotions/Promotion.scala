package checkout.promotions

import checkout._

trait Promotion {
  def calculateDiscount(basket: Basket): Basket
}

case class BuyXGet1Free(fruit: Fruit, xBuy: Int) extends Promotion {
  override def calculateDiscount(basket: Basket): Basket = {
    val updatedOrderLines = for {
      line <- basket.lineList
      newLine =
        if (line.fruit == fruit) {
          val qtyToDiscount = line.qty / xBuy
          if (qtyToDiscount >= 1) {
            val discount = (qtyToDiscount * line.unitPrice) + line.discount
            val updatedLine = line.copy(discount = discount)
            if (updatedLine.total >= 0) updatedLine else line
          } else line
        } else line
    } yield newLine

    Basket(updatedOrderLines)
  }
}

case class Bundle(bundleFruits: (Fruit, Fruit)) extends Promotion {
  override def calculateDiscount(basket: Basket): Basket = ???
}
