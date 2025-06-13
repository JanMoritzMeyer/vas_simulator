import scala.util.Random

case class FixedCostDatacenter(name: String, cost: Int, alpha: Double, price: Int) extends Datacenter {

  var revenueFromDelegatedOffer: Option[Int] = None
  var receivedOffers: List[Offer] = List()
  var selectedOffer: Option[Offer] = None

  override def evaluate(others: List[Datacenter]): Unit = {
    // implement offer strategy
    val chosen = Random.between(0, others.size-1)
    others(chosen).receive(Offer(cost, (newCost) => {
      revenueFromDelegatedOffer = Some(
        // win + additional win
        price - cost + Math.ceil((cost - newCost) * alpha).toInt
      )
    }))
  }

  override def receive(offer: Offer): Unit = {
    // gather offers
    receivedOffers = receivedOffers :+ offer
  }

  override def checkOffers(): Unit = {
    // sort offers, choose the one with minimal costs
    selectedOffer = receivedOffers
      .sortBy(_.originalCost)
      .reverse
      .headOption

    selectedOffer
      .map(_.accept)

  }

  override def calculateRevenue(): Int = {
    val ownRevenue: Int = revenueFromDelegatedOffer.getOrElse(price - cost)

    val secondaryRevenue: Double = selectedOffer
      .map(offer => (offer.originalCost - cost) * (1 - alpha))
      .getOrElse(0.0)

    println(s"$name evalutes to ownRevenue: $ownRevenue, secondaryRevenue: $secondaryRevenue")

    ownRevenue + Math.ceil(secondaryRevenue).toInt
  }
}
