package checkout

import checkout.promotions.{Bundle, BuyXGet1Free}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CheckoutTest extends AnyFlatSpec with Matchers {

  private val apple = Fruit("Apple")
  private val orange = Fruit("Orange")
  private val banana = Fruit("Banana")

  private val priceList: Map[Fruit, BigDecimal] = Map(
    apple -> 0.6,
    orange -> 0.25,
    banana -> 0.20
  )
  private val promotions = List(
    BuyXGet1Free(apple, 2),
    BuyXGet1Free(orange, 3),
    BuyXGet1Free(banana, 2),
    Bundle(apple, banana)
  )

  private val checkout = new CheckOut(promotions, priceList)
  "Checkout empty" should
    """be 0""" in {
    checkout.calculateTotal("") shouldEqual 0
  }

  "Checkout Apple" should
    "be 0.6" in {
    checkout.calculateTotal("Apple") shouldEqual 0.6
  }

  "Checkout Pear" should
    "be 0.0" in {
    checkout.calculateTotal("Pear") shouldEqual 0.0
  }

  "Checkout Apple Orange" should
    "be Apple 0,6 + Orange 0,25 = 0.85" in {
    checkout.calculateTotal("Apple", "Orange") shouldEqual 0.85
  }

  "Checkout 3 Orange" should
    "apply 3x2 on Orange = 0,5" in {
    checkout.calculateTotal("Orange", "Orange", "Orange") shouldEqual 0.5
  }

  "Checkout 2 Apple " should
    "apply 2x1 on Apple = 0,6" in {
    checkout.calculateTotal("Apple", "Apple") shouldEqual 0.6
  }

  "Checkout 2 Banana 1 Orange " should
    "apply 2x1 on Banana = 0,20 + Orange 0,25 = 0,45" in {
    checkout.calculateTotal("Banana", "Orange", "Banana") shouldEqual 0.45
  }

  "Checkout 2 Apple 1 Banana" should
    "apply 2x1 on Apple and Bundle on Banana = 0,6" in {
    checkout.calculateTotal("Apple", "Apple", "Banana") shouldEqual 0.60
  }

}
