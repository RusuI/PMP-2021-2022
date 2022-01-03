package Lab9

import com.cra.figaro.language.{Select, Apply, Constant, Element, Chain, Universe}
import com.cra.figaro.library.compound.{If, CPD, RichCPD, OneOf, *, ^^}
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.algorithm.filtering.ParticleFilter

object Ex5 {
  def main(args: Array[String]) {


    val months = 13
    val fraction = 0.3

    var investment: Array[Element[Double]] = Array.fill(months)(Constant(0.0))
    var profit: Array[Element[Double]] = Array.fill(months)(Constant(0.0))
    var capital: Array[Element[Double]] = Array.fill(months)(Constant(0.0))

    capital(0) = Constant(1200.0)

    def transition(capital: Double, profit:Double, investment: Double): (Element[(Double, Double, Double)]) = 
	{


        val newInvestment = Apply(Constant(capital), (cap: Double) => cap * fraction) // investeste cu 30 % din capitalul anterior

		val newProfit = Apply(newInvestment, Constant(capital), (inv: Double, cap: Double) =>
        if (inv >= 0.5 * cap) Select(0.1 -> (0.4 * cap), 0.3 -> (0.5 * cap), 0.6 -> (0.7 * cap)); // daca investitia este mai mare de 50 % din capital atunci va avea un profit de 70 % din capital cu probabilitatea de 0.6
        else if (inv >= 0.3 * cap) Select(0.2 -> (0.25 * cap), 0.6 -> (0.5 * cap), 0.2 -> (0.35 * cap)); // daca investitia este mai mare de 30 % din capital atunci va avea un profit de 50 % din capital cu probabilitatea de 0.6
        else Select(0.6 -> (0.3 * cap), 0.3 -> (0.2 * cap), 0.1 -> (0.1 * cap))) // daca investitia este mai mica de 30 % din capital atunci va avea un profit de 30 % din capital cu probabilitatea de 0.6
		
    
		val newCapital = Apply(Constant(profit), Constant(capital), Constant(investment),
                        (prof: Double, cap: Double, invest: Double) => cap + prof - invest)

		^^(newCapital, newProfit, newInvestment)
	}


    for {month <- 1 until months} {
    val newState =
      Chain(capital(month - 1), profit(month - 1), investment(month - 1)(capital: Double, profit:Double, investment: Double) => transition(capital,profit,investment))
    capital(month) = newState._1
    profit(month) = newState._2
    investment(month) = newState._3
  }

    val alg = Importance(10000, profit(month - 1))
    alg.start()
    println(alg.probability(profit(month - 1), (i: Int) => i > 4))

    // println(Importance.probability(capital(10), (c: Double) => c > 1200.0))
  }
}