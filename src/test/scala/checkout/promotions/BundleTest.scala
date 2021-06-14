package checkout.promotions

import checkout.basket.{Basket, CheckOutLine}
import checkout.Fruit
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class BundleTest extends AnyFlatSpec {
  private val apple = Fruit("Apple")
  private val banana = Fruit("Banana")

  private val promo = Bundle(apple, banana)

  "basket total with 1 Apple and 1 Banana" should
    "be 0,40 Apple line" in {
    val basket = Basket(
      List(
        checkout.basket.CheckOutLine(apple, 1, 0.40, 0),
        checkout.basket.CheckOutLine(banana, 1, 0.20, 0)
      )
    )
    val newBasket = promo.calculateDiscount(basket)
    newBasket.total shouldEqual 0.40
  }

  "basket total with 1 Apple and 3 Banana" should "be 0,80 Apple line" in {
    val basket = Basket(
      List(
        checkout.basket.CheckOutLine(apple, 1, 0.40, 0),
        checkout.basket.CheckOutLine(banana, 3, 0.20, 0)
      )
    )
      val newBasket = promo.calculateDiscount(basket)
      newBasket.total shouldEqual 0.80
    }

  "basket total with 2 Apple and 1 Banana" should "be 0,80 Apple line" in {
    val basket = Basket(
      List(
        checkout.basket.CheckOutLine(apple, 2, 0.40, 0),
        checkout.basket.CheckOutLine(banana, 1, 0.20, 0)
      )
    )
    val newBasket = promo.calculateDiscount(basket)
    newBasket.total shouldEqual 0.80
  }
}
