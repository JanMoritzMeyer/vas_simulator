import org.apache.commons.math3.distribution.NormalDistribution

case class FlexibleCostDatacenter(name: String, mean: Int, variance: Int, alpha: Double, price: Int) extends Datacenter {
  var actualCost: Int = 0
  var offers: List[Offer] = List.empty
  var selectedOffer: Option[Offer] = None
  var revenueFromDelegatedOffer: Option[Int] = None

  def calculateProbabilityForMostExpensive(others: List[FlexibleCostDatacenter]): Double = {
    val probas: List[Double] = others.map(datacenter => {
      val dist = NormalDistribution(datacenter.mean, datacenter.variance)
      val cheaper = dist.cumulativeProbability(actualCost)
      1 - cheaper
    })
    // Multiply
    probas.foldLeft(1.0)((x, y) => {
      x*y
    })
  }

  override def evaluate(others: List[Datacenter]): Unit = {
    actualCost = NormalDistribution(mean, variance).sample().round.toInt

    val rcs = others.collect{
      case x: FlexibleCostDatacenter => x
    }

    val probas = rcs.map(datacenter => {
      (datacenter, calculateProbabilityForMostExpensive(rcs.filter(_ != datacenter)))
    })
      .sortBy(_._2)

    probas
      .reverse
      .headOption
      .map(_._1)
      .foreach(_.receive(Offer(actualCost, (newCost) => {
        revenueFromDelegatedOffer = Some(
          // win + additional win
          price - actualCost + Math.ceil((actualCost - newCost) * alpha).toInt
        )
      }, name)))
  }

  override def receive(offer: Offer): Unit = {
    offers = offers :+ offer
  }

  override def checkOffers(): Unit = {
    // sort offers, choose the one with maximum costs
    selectedOffer = offers
      .filter(offer => offer.originalCost > actualCost)
      .sortBy(_.originalCost)
      .reverse
      .headOption

    //Console.println(s"$name received offers from ${offers.map(_.name).mkString(",")} and decided for offer from ${selectedOffer.map(_.name).getOrElse("none")}")

    selectedOffer
      .map(_.accept)
  }

  override def calculateRevenue(): Int = {
    val ownRevenue: Int = revenueFromDelegatedOffer.getOrElse(price - actualCost)

    val secondaryRevenue: Double = selectedOffer
      .map(offer => (offer.originalCost - actualCost) * (1 - alpha))
      .getOrElse(0.0)

    //println(s"$name evalutes to ownRevenue: $ownRevenue, secondaryRevenue: $secondaryRevenue")

    ownRevenue + Math.ceil(secondaryRevenue).toInt
  }
}
