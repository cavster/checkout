import org.scalatest._
class CheckoutSpec extends FlatSpec {

  val checkoutService = new Checkout
  val exampleInput = List("APPLE", "APPLE", "ORANGE", "APPLE")


  "checkoutService " should "return zero if their are no items" in {
     assert(checkoutService.calculateTotalPrice(List()) == 0)
  }
  it should "calculate the price of a list of items in" in {
    assert(checkoutService.calculateTotalPrice(exampleInput) == 205)
  }
  it should "ignore Items who we do not sell in" in {
    val listWithVoidItem = exampleInput ++ List("Bnanna")
    assert(checkoutService.calculateTotalPrice(listWithVoidItem) == 205)
 }
  it should "take into account  discounts" in {
    val listOfApples = List("APPLE","APPLE")
    val discounts1 = Some(List(DiscountRule("APPLE" , 2 , 60)))
    assert(checkoutService.calculateTotalPrice(listOfApples , discounts1) == 60)
    val listOfApples2 = List("APPLE","APPLE", "APPLE")
    assert(checkoutService.calculateTotalPrice(listOfApples2 , discounts1) == 120)
    val listOfOrange =  List("ORANGE","ORANGE","ORANGE","ORANGE","ORANGE","ORANGE","ORANGE","ORANGE","ORANGE")
    val discounts2 = Some(List(DiscountRule("ORANGE" , 3 , 50)))
    assert(checkoutService.calculateTotalPrice(listOfOrange , discounts2) == 150)
    val discounts3 = Some(List(DiscountRule("ORANGE" , 3 , 50),DiscountRule("APPLE" , 2 , 60)))
    assert(checkoutService.calculateTotalPrice(listOfOrange ++ listOfApples, discounts3) == 210)
  }

}
