package checkout

import CheckOut._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CheckoutTest extends AnyFlatSpec with Matchers {

  private val discountList =
    (BuyXGet1Free.list.toList :: Bundle.list.toList :: Nil).flatten
  private val priceList: Map[Fruit, BigDecimal] = Map(
    Fruit("Apple") -> 0.6,
    Fruit("Orange") -> 0.25,
    Fruit("Banana") -> 0.20
  )
  private val checkout = new CheckOut(discountList, priceList)
  "Checkout empty" should
    """be 0""" in {
    checkout.calculateTotal("") shouldEqual 0
  }

  "Checkout Apple" should
    "be 0.6 - 10% = 0,54" in {
    checkout.calculateTotal("Apple") shouldEqual 0.54
  }

  "Checkout Pear" should
    "be 0.0" in {
    checkout.calculateTotal("Pear") shouldEqual 0.0
  }

  "Checkout Apple Orange" should
    "be Apple 0,6 + Orange 0,25 = 0.85 - 10% = 0,765 " in {
    checkout.calculateTotal("Apple", "Orange") shouldEqual 0.765
  }

  "Checkout 3 Orange" should
    "apply 3x2 on Orange = 0,5 - 10% = 0,45" in {
    checkout.calculateTotal("Orange", "Orange", "Orange") shouldEqual 0.45
  }

  "Checkout 2 Apple " should
    "apply 2x1 on Apple = 0,6 -10% = 0,54" in {
    checkout.calculateTotal("Apple", "Apple") shouldEqual 0.54
  }

  "Checkout 2 Banana 1 Orange " should
    "apply 2x1 on Banana = 0,20 + Orange 0,25 = 0,45 -10% = 0,405" in {
    checkout.calculateTotal("Banana", "Orange", "Banana") shouldEqual 0.405
  }

  "Checkout 2 Apple 1 Banana" should
    "apply 2x1 on Apple and Bundle on Banana = 0,6" in {
    checkout.calculateTotal("Apple", "Apple", "Banana") shouldEqual 0.60
  }

}
