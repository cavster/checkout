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
}
