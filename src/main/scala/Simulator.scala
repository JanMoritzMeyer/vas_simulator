object Simulator {

  def main(args: Array[String]): Unit = {
    val results = (1 to 1_000)
      .map(x => singleRun())

    val m1 = results.map(_._1)
    val m11 = m1.sum / m1.size

    val m2 = results.map(_._2)
    val m22 = m2.sum / m2.size

    val wins = results.map(_._3)
    val mean = wins.sum / wins.size


    results.map(x => s"(${x._1}, ${x._2}),").foreach(println(_))
    Console.println(s"average win due to strategy is $mean, where average win with is: $m11, without is: $m22")

  }

  def singleRun(): (Int, Int, Int) = {
    val price = 50

    val datacenters = List(
      FlexibleCostDatacenter("Rechenzentrum 1", 10, 10, 0.5, price),
      FlexibleCostDatacenter("Rechenzentrum 2", 20, 10, 0.5, price),
      FlexibleCostDatacenter("Rechenzentrum 3", 30, 10, 0.5, price),
      FlexibleCostDatacenter("Rechenzentrum 4", 40, 10, 0.5, price),
      FlexibleCostDatacenter("Rechenzentrum 5", 50, 10, 0.5, price),
    )

    datacenters
      .foreach(current => current.evaluate(datacenters.filter(_ != current)))

    datacenters
      .foreach(_.checkOffers())

    val overall = datacenters
      .map(_.calculateRevenue())

    val altRev = price * datacenters.size - datacenters.map(_.actualCost).sum

    (overall.sum, altRev, (overall.sum - altRev))
  }

}
