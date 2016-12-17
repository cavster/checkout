

trait ShoppingItems {
  val name: String
  val price: Int
}

case class Apples(name: String = "APPLE", price: Int = 60) extends ShoppingItems

case class Oranges(name: String = "ORANGE", price: Int = 25) extends ShoppingItems

case class DiscountRule(nameOfItem: String, amountOfItemForDiscount: Int, discountedPrice: Int)

class Checkout {

  def calculateTotalPrice(shoppingItems: List[String], discounts: Option[List[DiscountRule]] = None): Int = {
    val shoppingItemsAsOurData = shoppingItems.flatMap(matchItems)
    val normalPrice = shoppingItemsAsOurData.foldLeft(0)((totalPrice, item) => totalPrice + item.price)
    discounts match {
      case Some(discount) => normalPrice - applyDiscounts(shoppingItemsAsOurData, discount)
      case None => normalPrice
    }
  }

  private def applyDiscounts(shoppingItems: List[ShoppingItems], discountRules: List[DiscountRule]): Int = {
    val mapOfShoppingItems = shoppingItems.groupBy(_.name)
    discountRules.map(dr => calculateAmountToDiscount(mapOfShoppingItems(dr.nameOfItem), dr.amountOfItemForDiscount, dr.discountedPrice)).sum
  }

  def calculateAmountToDiscount(listOfOneItem : List[ShoppingItems], amountNeededForDiscount: Int, setPrice: Int): Int = {
    val numberOfDiscounts = listOfOneItem.size / amountNeededForDiscount
    val normalCost = (listOfOneItem.headOption.map(_.price).getOrElse(0) * amountNeededForDiscount) * numberOfDiscounts
    val totalDiscount = numberOfDiscounts * setPrice
    normalCost - totalDiscount
  }

  private def matchItems(itemName: String): Option[ShoppingItems] = {
    itemName.toUpperCase.trim match {
      case "APPLE" => Some(Apples.apply())
      case "ORANGE" => Some(Oranges.apply())
      case _ => None //I am not going to worry about logging or exceptions for this problem ;)
    }
  }

}
