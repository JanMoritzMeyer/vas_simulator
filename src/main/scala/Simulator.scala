object Simulator {

  def main(args: Array[String]): Unit = {
    val price = 30

    val datacenters = List(
      FixedCostDatacenter("Rechenzentrum 1", 10, 0.5, price),
      FixedCostDatacenter("Rechenzentrum 2", 20, 0.5, price),
      FixedCostDatacenter("Rechenzentrum 3", 30, 0.5, price),
      FixedCostDatacenter("Rechenzentrum 4", 40, 0.5, price),
      FixedCostDatacenter("Rechenzentrum 5", 50, 0.5, price),
    )

    datacenters
      .foreach(_.evaluate(datacenters))

    datacenters
      .foreach(_.checkOffers())

    val overall = datacenters
      .map(_.calculateRevenue())

    println(s"the revenue is $overall with an overall of ${overall.sum}")

  }

}
