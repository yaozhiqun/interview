package rocketlawyer

import java.util.Scanner

object VendingMachineSolution extends App {

  case class CreditCard(cardNumber: String)
  case class Item(code: Int, name: String, price: Double) {
    override def toString: String = {
      s"$name for $price"
    }
  }
  case class Inventory(item: Item, count: Int)

  sealed trait PaymentMethod
  case object Cash extends PaymentMethod
  case object CreditCard extends PaymentMethod

  sealed trait State
  case object Ready extends State
  case object AwaitingPaymentSelection extends State
  case object CollectingPayment extends State

  case class VendingMachine(var inventories: List[Inventory], var state: State = Ready) {
    var selectedItem: Option[Item] = None
    var balance: Double = 0.0

    def listInventories: List[Inventory] = {
      val availableItems = inventories.filter(_.count > 0)
      println("VM is showing the items:")
      availableItems.foreach(println)
      println()

      availableItems
    }

    def itemSelected(item: Item): Unit = {
      state match {
        case Ready =>
          vm.state = AwaitingPaymentSelection
          selectedItem = Some(item)
          println(s"Item $item is selected, VM is now $state, showing payment methods:")
          listPaymentMethods.foreach(println)
          println()
        case _ =>
          println("Error: Cannot select item now.")
      }

    }

    def paymentMethodSelected(paymentMethod: PaymentMethod): Unit = {
      state match {
        case AwaitingPaymentSelection =>
          vm.state = CollectingPayment
          println(s"PaymentMethod $paymentMethod is selected")
          println(s"VM is now $state with balance $balance")
          println()
        case _ =>
          println("Error: Cannot select payment method now.")
      }
    }

    def listPaymentMethods: List[PaymentMethod] = {
      List(Cash, CreditCard)
    }

    def receivePayment(creditCard: CreditCard): Boolean = {
      chargeCreditCard(creditCard)
      dispenseItems()
      removeItem()
      reset()
      true
    }

    private def chargeCreditCard(creditCard: CreditCard): Unit = {
        println(s"${selectedItem.get.price} is charged successfully for ${selectedItem.get.name}")
        println()
        balance += selectedItem.get.price
    }

    private def dispenseItems(): Unit = {
      println(s"Dispense ${selectedItem.get} to customer")
    }

    private def removeItem(): Unit = {
      println(s"Remove 1 ${selectedItem.get.name} from inventory")
      inventories = inventories collect {
        case Inventory(it@Item(code, _, _), count) if code == selectedItem.get.code => Inventory(it, count - 1)
        case inventory => inventory
      }
    }

    def cancelTx(): Unit = {
      println("VM cancelled transaction")
      println()
      reset()
    }

    private def reset(): Unit = {
      selectedItem = None
      state = Ready
      println(s"VM is now in state of ${vm.state} with balance $balance")
      println()
    }
  }

  case class Customer(name: String) {
    def selectItem(vm: VendingMachine, itemCode: Int): Option[Item] = {
      vm.inventories.find(_.item.code == itemCode).map(_.item) match {
        case Some(item) =>
          println(s"$name selected $item")
          println()
          vm.itemSelected(item)
          Some(item)
        case _ => None
      }
    }

    def selectPaymentMethod(vm: VendingMachine, paymentMethod: PaymentMethod): Unit = {
      vm.paymentMethodSelected(paymentMethod)
    }

    def payByCreditCard(vm: VendingMachine, card: CreditCard): Boolean = {
      println(s"$name provided CreditCard ####")
      vm.receivePayment(card)
    }

    def cancel(vm: VendingMachine): Unit = {
      println(s"$name cancelled the purchase")
      println()
      vm.cancelTx()
    }

    def payByCash(vm: VendingMachine, amount: Double): Boolean = ???
    def collectChange(vm: VendingMachine): Option[Double] = ???
    def collectItem(): Unit = {
      println(s"$name picked up the item")
    }
  }

  val pepsi = Item(1, "pepsi", 2.0)
  val water = Item(2, "water", 1.5)
  val energyBar = Item(4, "energyBar", 4.0)

  val vm = VendingMachine(List(Inventory(pepsi, 1), Inventory(water, 3)))

  object Vending {
    def start(vm: VendingMachine): Unit = {
      vm.listInventories.map(_.item).foreach(println)
      val scanner = new java.util.Scanner(System.in)
      print("Choose item? ")
      val input = scanner.nextLine()
      vm.inventories.find(_.item.code == input.toInt).map(_.item) match {
        case Some(i) => println(s"$i.name is selected, please insert quarters or bills")
        case None => println(s"Item $input is not found")
      }
    }
  }

  Vending.start(vm)
  new Scanner(System.in)
//  val zach = Customer("Zach")
//
//  vm.listInventories
//  zach.selectItem(vm, 2)
//  zach.selectPaymentMethod(vm, CreditCard)
//  zach.payByCreditCard(vm, CreditCard("123456"))
//  zach.collectItem()
//
//  zach.selectItem(vm, 2)
//  zach.cancel(vm)
//  vm.listInventories
}
