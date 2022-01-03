package Lab9
import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.Normal
import com.cra.figaro.library.atomic.discrete.{FromRange, Poisson}
import com.cra.figaro.library.compound._
import com.cra.figaro.algorithm.filtering.ParticleFilter

object Ex5 {

val months = 13
val fraction = 0.3

val initial = Universe.createNew()
Constant(200)("capital", initial)
Constant(500)("investment", initial)
Constant(50)("profit", initial)


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

def nextUniverse(previous: Universe): Universe = {
  val next = Universe.createNew()
  val previousInvestment = previous.get[Double]("investment")
  val previousCapital = previous.get[Double]("capital")
  val previousProfit = previous.get[Double]("profit")
  val newState = Chain(^^(previousCapital, previousProfit, previousInvestment), (transition _).tupled)
  Apply(newState, (s: (Double, Double, Double)) => s._1)("investment", next)
  Apply(newState, (s: (Double, Double, Double)) => s._2)("profit", next)
  Apply(newState, (s: (Double, Double, Double)) => s._3)("capital", next)
  next
}

def main(args: Array[String]) {
 val capitalObservation = List(200, None, None, None, None, None, None,None, None, None)
 val alg = ParticleFilter(initial, nextUniverse, 10000)
 alg.start()
 for { time <- 1 to 10 } {
   val evidence = {
      capitalObservation(time) match {
      case None => List()
      case Some(n) => List(NamedEvidence("capital", Observation(n)))
    }
  }
  alg.advanceTime(evidence)
  print("Time " + time + ": ")
  print("capital = " + alg.currentExpectation("capital", (c: Int) => c))
  println(", investment = " + alg.currentExpectation("investment", (i: Int) => i))
  println(", profit = " + alg.currentExpectation("profit", (p: Int) => p))
  }
 }

}