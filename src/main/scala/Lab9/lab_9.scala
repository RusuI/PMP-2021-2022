
package Lab9
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.discrete.Uniform

object Ex4 {
	def main(args: Array[String]) {
		val length = 10

        val capital: Array[Element[Double]] = Array.fill(length)(Constant(0.0))
        val profit: Array[Element[Double]] = Array.fill(length)(Constant(0.0))
        val investitie: Array[Element[Double]] =Array.fill(length)(Constant(0.0))

        capital(0) = Constant(1000.0)
        profit(0) = Constant(0.0)
        val politica= Constant(0.1)
        investitie(0) = politica*capital(0)

        for { year <- 1 until length }
         {

             capital(year) = Apply(capital(year-1), investitie(year-1), profit(year-1), (capital: Double, investitie: Double, profit: Double) => capital + profit - investitie)
		     investitie(year) = Apply(capital(year-1), politica, (capital: Double, politica: Double) => capital * politica)
             val profit_maxim = Apply(investitie(year-1),(investitie:Double) => 5000 + investitie)
             val profit_minim = Apply(investitie(year-1),(investitie:Double) => investitie - 5000)
             profit(year) = Uniform(profit_minim, profit_maxim)

         }

         val alg = VariableElimination(capital(1))
         alg.start()
         alg.stop()
         println("Capital pentru investitii de 10%: " + alg.mean(capital(1)))
         


         val alg1 = VariableElimination(profit(5))
         alg1.start()
         alg1.stop()
         println("Profit in al 5-lea an: " + alg1.mean(profit(5)))
        

        }
}
