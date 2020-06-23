package rocketlawyer

object VendingSolution extends App {

  case class Item(code: Int, name: String, price: Double) {
    override def toString: String = s"""[$code] $name for $$${price}"""
  }
  case class Inventory(item: Item, count: Int) {
    override def toString: String = s"$item - $count in stock"
  }

  case class Vending(inventories: List[Inventory],
                     selectedItem: Option[Item] = None,
                     collectedPayment: Double = 0.0) {

    def listInventories: Vending = {
      println("Welcome")
      inventories.filter(_.count > 0).foreach(println)
      this
    }

    def selectItem(code: Int): Vending = {
      inventories.find(_.item.code == code).map(_.item) match {
        case Some(item) =>
          println(s"${item.name} is selected")
          println(s"Please pay ${item.price - collectedPayment}")
          this.copy(selectedItem = Some(item))
        case None =>
          println(s"Item not found")
          this
      }
    }

    def collectPayment(amount: Double): Vending = {
      selectedItem match {
        case Some(item) =>
          val collected = collectedPayment + amount
          val overstep = collected - item.price
          if (overstep >= 0) {
            dispenseItem(item)
            returnChange(overstep)
            this.copy(inventories = updateInventory(item), selectedItem = None, collectedPayment = 0.0)
          } else {
            println(s"Please pay ${-overstep}")
            this.copy(collectedPayment = collected)
          }
        case None =>
          println(s"Please select item")
          this
      }
    }

    def dispenseItem(item: Item): Unit = {
      println(s"Please enjoy ${item.name}")
    }

    def updateInventory(item: Item): List[Inventory] = {
      inventories.collect {
        case Inventory(i, count) if i.code == item.code => Inventory(i, count - 1)
        case other => other
      }
    }

    def returnChange(change: Double): Unit = {
      selectedItem match {
        case Some(_) =>
          if (change > 0) {
            println(s"Here is your change $change")
          }
        case None =>
      }
    }

    def cancel: Vending = {
      selectedItem match {
        case Some(_) =>
          returnChange(collectedPayment)
          println("Thank you! Hope to see you soon.")
          this.copy(selectedItem = None, collectedPayment = 0.0)
        case None =>
          this
      }
    }
  }


  val pepsi = Item(1, "pepsi", 2.0)
  val water = Item(2, "water", 1.5)
  val energyBar = Item(4, "energyBar", 4.0)
  val vm = Vending(List(Inventory(pepsi, 1), Inventory(water, 3)))

  vm
    .listInventories
    .selectItem(1)
    .collectPayment(1.0)
    .collectPayment(1.5)
    .listInventories
    .selectItem(1)
    .collectPayment(2.0)
    .listInventories
    .selectItem(2)
    .cancel
    .listInventories
    .selectItem(2)
    .collectPayment(1)
    .cancel
    .listInventories
    .selectItem(2)
    .collectPayment(0.5)
    .collectPayment(0.5)
    .collectPayment(0.5)
    .listInventories
    .cancel // nothing gonna happen
    .collectPayment(10.0) // select item first
}
