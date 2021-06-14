package checkout.promotions

import checkout._
import checkout.basket.{Basket, CheckOutLine}

trait Promotion {
  def calculateDiscount(basket: Basket): Basket
}

case class BuyXGet1Free(fruit: Fruit, xBuy: Int) extends Promotion {
  override def calculateDiscount(basket: Basket): Basket = {
    val updatedOrderLines = for {
      line <- basket.orderLines
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
  override def calculateDiscount(basket: Basket): Basket = {
    val bundlesOrderLines = basket.orderLines.filter(isABundle)
      val sortedByUnitPrice = bundlesOrderLines.sortWith(_.unitPrice < _.unitPrice)
      sortedByUnitPrice match {
        case List(cheapest, expensive) => {
          val qtyToDiscount = List(cheapest.qty, expensive.qty).min
          val discount = cheapest.discount + (cheapest.unitPrice * qtyToDiscount)
          val cheapestDiscounted = cheapest.copy(discount = discount)
          val updatedOrderLines = for {
            line <- basket.orderLines
            newLine =
              if (line.fruit == cheapestDiscounted.fruit) {
                cheapestDiscounted
              } else line
          } yield newLine
          checkout.basket.Basket(updatedOrderLines)
        }
        case _ => basket
      }
  }

  private def isABundle(line: CheckOutLine) : Boolean = line.fruit == bundleFruits._1 || line.fruit == bundleFruits._2
}
