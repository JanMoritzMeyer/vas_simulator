import scala.util.Random

case class FixedCostDatacenter(name: String, cost: Int, alpha: Double) extends Datacenter {

  var revenueFromAcceptedOffer: Option[Int] = None
  var offers: List[Offer] = List()
  var selectedOffer: Option[Offer] = None
  var paidPrice = 0

  override def evaluate(price: Int, others: List[Datacenter]): Unit = {
    paidPrice = price

    // implement offer strategy
    val otherDatacenters = others.filter(_ != this)
    val chosen = Random.between(0, otherDatacenters.size-1)
    otherDatacenters(chosen).receive(Offer(cost, (newCost) => {
      revenueFromAcceptedOffer = Some(
        // win + additional win
        price - cost + Math.ceil((cost - newCost) * alpha).toInt
      )
    }))
  }

  override def receive(offer: Offer): Unit = {
    offers = offers :+ offer
  }

  override def checkOffers(): Unit = {
    selectedOffer = offers
      .sortBy(_.originalCost)
      .reverse
      .headOption


    selectedOffer
      .map(_.accept)

  }

  override def calculateRevenue(): Int = {
    val ownRevenue: Int = revenueFromAcceptedOffer.getOrElse(paidPrice - cost)

    val secondaryRevenue: Double = selectedOffer
      .map(offer => (offer.originalCost - cost) * (1 - alpha))
      .getOrElse(0.0)

    ownRevenue + Math.ceil(secondaryRevenue).toInt
  }
}
