package Lab7

import com.cra.figaro.language.{Element, Select, Flip, Apply, Chain}
import com.cra.figaro.language.Constant
import com.cra.figaro.library.compound.{^^}
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.library.atomic.continuous.Uniform
 import com.cra.figaro.library.atomic.continuous.AtomicUniform

object Departments {

	class ResearchAndDevelopment {
		 val state = Uniform(1,100)
	}

	class HumanResources {
		val state = Uniform(1,100)
	}

	class Production(val rd: ResearchAndDevelopment, val hr: HumanResources) {
        val mean = Apply(rd.state, hr.state, (i1: Double, i2: Double) => (i1+i2)/2 )
		val state = Chain(mean, (mean: Double) => if (mean > 100.0) Constant(100.0); else Constant(mean))
	}

	class Sales(val p: Production) {
		 val state = Chain(p.state,
         (p: Double) => 
          if(p > 60.0) Uniform(75,100)
          else Uniform(30,50)
          )
	}

	class Finance(val hr: HumanResources, val s: Sales) {
        val money = Apply(hr.state, s.state, (i1: Double, i2: Double) => (100 - i1 + i2))
        val state =  Chain( money,
                               (money: Double) =>
                                   if(money > 100.0)
                                      Constant(100.0)
                                      else Constant(money)
        )
	}

	class Firm(val rd: ResearchAndDevelopment, val hr: HumanResources, val p: Production, val s: Sales, val f: Finance) {        
		  val health = Apply(rd.state, hr.state, p.state, s.state, f.state,
                       (rd: Double, hr: Double, p: Double, s: Double, f:Double)=>
                       (rd+hr+p+s+f)/5)
	}

	def main(args: Array[String]) {
		val rd = new ResearchAndDevelopment()
		val hr = new HumanResources()
		val p = new Production(rd, hr)
		val s = new Sales(p)
		val f = new Finance(hr, s)
		val firm = new Firm(rd, hr, p, s, f)
    

       val alg1 = VariableElimination(firm)
       alg1.start()
       alg1.stop()
       println(alg2.mean(firm.health))
		
	}
}