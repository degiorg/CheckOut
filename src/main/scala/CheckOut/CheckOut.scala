package CheckOut

object CheckOut  {
  def process(items: String*) : BigDecimal = {

    val basket = Basket(items.groupMapReduce(Fruit)(_ => 1)(_ + _))
    val checkOut = new Total

    // STEP1
    checkOut addSubTotal basket.fruits.foldLeft(SubTotal.empty) {
      case (tot, currFruit) =>
        val price = getPrice(currFruit._1)
        price match {
          case PriceList.notFound => tot
          case _ => tot + subtotal(currFruit._2 , price)
        }
    }

    //STEP2
    checkOut applyDiscount basket.fruits.foldLeft(SubTotal.empty) {
      case (tot, currFruit) =>
        val discount = getDiscount(currFruit._1)
        discount match {
          case Deal.notFound => tot
          case _ => tot + discountBuyX(currFruit._2, discount, getPrice(currFruit._1))
        }
    }

    //STEP 3
    checkOut applyDiscount basket.fruits.foldLeft(SubTotal.empty) {
      case (tot, currFruit) =>
        val fruitBundle : Fruit = Deal.bundle.getOrElse(currFruit._1, Deal.bundleNotFound)
        fruitBundle match {
          case Deal.bundleNotFound => tot
          case _ =>
            val fruitBundleNum: Int = basket.fruits.getOrElse(fruitBundle, 0)
            fruitBundleNum match {
              case 0 => tot
              case _ => calcBundle(currFruit, (fruitBundle, fruitBundleNum))
            }
        }
    }

    checkOut.total
  }
  def subtotal(num: Int, price: BigDecimal) : BigDecimal = num * price
  def getPrice(fruit: Fruit) : BigDecimal = PriceList.priceList.getOrElse(fruit, PriceList.notFound)
  def getDiscount(fruit: Fruit) : Int = Deal.buyXGetYFree.getOrElse(fruit, Deal.notFound)
  def discountBuyX(num: Int, opLeft: Int, price: BigDecimal): BigDecimal = {
    if (opLeft > 0)
    num / opLeft match {
      case res if res >= 1 => res * price
      case _ => 0
    } else {
      0
    }
  }
  def calcBundle(fruit1: (Fruit, Int), fruit2: (Fruit, Int)): BigDecimal = {
    val fruit1Price = bundleBuffer(fruit1)
    val fruit2Price = bundleBuffer(fruit2)
    if (fruit1Price > fruit2Price) fruit2Price else fruit1Price
  }
  def bundleBuffer(fruit: (Fruit, Int)) : BigDecimal = {
    val price = getPrice(fruit._1)
    val discount = getDiscount(fruit._1)
    subtotal(fruit._2, price) - discountBuyX(fruit._2, discount, price)
  }
}


case class SubTotal(amount: BigDecimal)
object SubTotal {
  val empty: BigDecimal = 0.0
}
class Total {
  private var tot : BigDecimal = 0.0
  def total: BigDecimal = tot

  def addSubTotal(amount: BigDecimal): Unit = tot = tot + amount
  def applyDiscount(discount: BigDecimal): Unit = tot = tot - discount
}

case class Fruit(name: String)

case class Basket(fruits: Map[Fruit, Int])
object Basket {
  val empty: Map[Nothing, Nothing] = Map.empty
}

object PriceList  {
  val priceList: Map[Fruit, BigDecimal] = Map(
    Fruit("Apple") -> 0.6,
    Fruit("Orange") -> 0.25,
    Fruit("Banana") -> 0.20
  )
  val notFound: BigDecimal = 0.0
}

object Deal  {
  val buyXGetYFree: Map[Fruit, Int] = Map(
    Fruit("Apple") -> 2,
    Fruit("Orange") -> 3,
    Fruit("Banana") -> 2
  )
  val notFound: Int = 0

  val bundle: Map[Fruit, Fruit] = Map(
    Fruit("Apple") -> Fruit("Banana")
  )
  val bundleNotFound: Fruit = Fruit("")
}
