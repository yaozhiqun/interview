package leetcode

object MaxProfit extends App {

  trait TradeType {
    override def toString: String = this.getClass.getSimpleName
  }
  case object Buy extends TradeType
  case object Sell extends TradeType
  case object Hold extends TradeType

  case class Trade(tradeType: TradeType, prevTrade: Option[Trade]) {
    def depth: Int = {
      prevTrade.map(_.depth + 1).getOrElse(1)
    }

    override def toString: String = {
      prevTrade.map(preNode => s"${preNode.toString} > $tradeType").getOrElse(tradeType.toString)
    }

    def canSell: Boolean = {
      prevTrade match {
        case Some(Trade(Buy, _)) => true
        case Some(Trade(Sell, _)) => false
        case Some(node@Trade(Hold, _)) => node.canSell
        case None => false
      }
    }

    def canBuy: Boolean = {
      prevTrade match {
        case Some(Trade(Buy, _)) => false
        case Some(Trade(Sell, _)) => true
        case Some(node@Trade(Hold, _)) => node.canBuy
        case None => true
      }
    }
  }

  def maxProfit(prices: List[Int]): Int = {

    def genTrades(prevTrade: Option[Trade] = None, trades: List[Trade] = Nil): List[Trade] = {
      prevTrade match {
        case Some(trade@Trade(_, _)) if trade.depth == prices.length =>
          trade :: trades
        case Some(Trade(Buy, _)) =>
          genTrades(Some(Trade(Sell, prevTrade)), trades) ::: genTrades(Some(Trade(Hold, prevTrade)), trades)
        case Some(Trade(Sell, _)) =>
          genTrades(Some(Trade(Hold, prevTrade)), trades)
        case Some(node@Trade(Hold, _)) =>
          val sells = if (node.canSell) { genTrades(Some(Trade(Sell, prevTrade)), trades) } else Nil
          val buys = if (node.canBuy) { genTrades(Some(Trade(Buy, prevTrade)), trades) } else Nil

          sells ::: buys ::: genTrades(Some(Trade(Hold, prevTrade)), trades)
        case None =>
          genTrades(Some(Trade(Buy, prevTrade)), trades) ::: genTrades(Some(Trade(Hold, prevTrade)), trades)
      }
    }

    def profit(trade: Trade): Int = {
      trade.tradeType match {
        case Buy  => trade.prevTrade.map(profit).getOrElse(0) - prices(trade.depth - 1)
        case Sell => trade.prevTrade.map(profit).getOrElse(0) + prices(trade.depth - 1)
        case Hold => trade.prevTrade.map(profit).getOrElse(0)
      }
    }

    val mostBeneficial = genTrades().maxBy(profit)

    println(mostBeneficial)
    profit(mostBeneficial)
  }

  println(maxProfit(List(1, 2, 3, 1, 3)))
}
