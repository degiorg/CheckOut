package checkout.promotions

import checkout.{Basket, CheckOutLine, Fruit}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class BundleTest extends AnyFlatSpec {
  private val apple = Fruit("Apple")
  private val banana = Fruit("Banana")
  private val basket = Basket(
    List(
      CheckOutLine(apple, 1, 0.40, List(), 0),
      CheckOutLine(banana, 1, 0.20, List(), 0)
    )
  )
  private val promo = Bundle(apple, banana)

  "basket total with 1 Apple and 1 Banana" should
    "be 0,40 Apple line" in {
    val newBasket = promo.calculateDiscount(basket)
    newBasket.total shouldEqual 0.40
  }

}
