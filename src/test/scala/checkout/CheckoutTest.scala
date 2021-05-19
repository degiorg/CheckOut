package checkout

import CheckOut._

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class CheckoutTest extends AnyFlatSpec with Matchers {

  """Checkout empty""" should """be 0""" in {
    CheckOut.process("")  shouldEqual 0
  }

  """Checkout Apple""" should """be 0.6""" in {
    CheckOut.process("Apple")  shouldEqual 0.6
  }

  """Checkout Pear""" should """be 0.0""" in {
    CheckOut.process("Pear")  shouldEqual 0.0
  }

  """Checkout Apple Orange""" should """be Apple 0,6 + Orange 0,25 = 0.85""" in {
    CheckOut.process("Apple" , "Orange")  shouldEqual 0.85
  }

  """Checkout Orange""" should """apply 3x2 on Orange = 0,5""" in {
    CheckOut.process("Orange" , "Orange", "Orange")  shouldEqual 0.5
  }
  """Checkout 2 Apple """ should """apply 2x1 on Apple = 0,6""" in {
    CheckOut.process("Apple" , "Apple")  shouldEqual 0.60
  }
  """Checkout 2 Apple 1 Banana""" should """apply 2x1 on Apple and Bundle on Banana = 0,6""" in {
    CheckOut.process("Apple" , "Apple", "Banana")  shouldEqual 0.60
  }

}
