trait Datacenter {
  
  def evaluate(others: List[Datacenter]): Unit
  def receive(offer: Offer): Unit
  def checkOffers(): Unit
  def calculateRevenue(): Int
  
}

case class Offer(originalCost: Int, accept: (newCost: Int) => Unit, name: String = "")
