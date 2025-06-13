object Simulator {

  def main(args: Array[String]): Unit = {
    val rechenzentren = List(
      FixedCostDatacenter("Rechenzentrum 1", 10, 0.5),
      FixedCostDatacenter("Rechenzentrum 2", 20, 0.5),
      FixedCostDatacenter("Rechenzentrum 3", 30, 0.5),
      FixedCostDatacenter("Rechenzentrum 4", 40, 0.5),
      FixedCostDatacenter("Rechenzentrum 5", 50, 0.5),
    )
    val price = 30

    rechenzentren
      .foreach(_.evaluate(price, rechenzentren))

    rechenzentren
      .foreach(_.checkOffers())

    val overall = rechenzentren
      .map(_.calculateRevenue())

    println(s"the revenue is $overall with an overall of ${overall.sum}")

  }

}
