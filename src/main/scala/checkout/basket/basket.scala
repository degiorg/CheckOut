package checkout.basket

import checkout.Fruit

case class Basket(orderLines: List[CheckOutLine]) {
  val total: BigDecimal = orderLines.foldLeft[BigDecimal](0.0)(_ + _.total)
}

case class CheckOutLine(
                         fruit: Fruit,
                         qty: Int,
                         unitPrice: BigDecimal,
                         discount: BigDecimal
                       ) {
  val total: BigDecimal = (qty * unitPrice) - discount
}
