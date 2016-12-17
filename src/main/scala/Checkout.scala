

trait ShoppingItems{
  val name  : String
  val price : Int
}

case class Apples(name: String = "APPLES", price: Int = 60) extends ShoppingItems

case class Oranges(name: String = "ORANGES", price: Int = 25) extends ShoppingItems

class Checkout {

  def calculateTotalPrice(shoppingItems: List[String]): Int = {
     shoppingItems.flatMap(matchItems).foldLeft(0)((totalPrice, item) => totalPrice +  item.price)
  }
 private def matchItems (itemName : String) : Option[ShoppingItems] = {
       itemName.toUpperCase.trim match {
      case "APPLE" => Some(Apples.apply())
      case "ORANGE" =>  Some(Oranges.apply())
      case _ => None  //I am not going to worry about logging or exceptions fot this excerzixe ;)
    }
  }

}
