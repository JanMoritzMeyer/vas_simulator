trait Datacenter {
  
  def evaluate(price: Int, others: List[Datacenter]): Unit
  def receive(offer: Offer): Unit
  def checkOffers(): Unit
  def calculateRevenue(): Int
  
}

case class Offer(originalCost: Int, accept: (newCost: Int) => Unit)
