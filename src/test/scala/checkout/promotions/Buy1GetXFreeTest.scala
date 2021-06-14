package checkout.promotions

import checkout.{Basket, CheckOutLine, Fruit}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class Buy1GetXFreeTest extends AnyFlatSpec {
  private val apple = Fruit("Apple")
  private val basket = Basket(List(CheckOutLine(apple, 2, 0.20, List(), 0)))
  private val promo = BuyXGet1Free(apple, 2)

  "basket total with 2 Apple" should
    "be 0,20 " in {
    val newBasket = promo.calculateDiscount(basket)
    newBasket.total shouldEqual 0.20
  }
}
