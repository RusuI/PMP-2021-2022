import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language._
import com.cra.figaro.library.compound.CPD
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.atomic.continuous.Uniform
import com.cra.figaro.library.process.FixedSizeArray._
import scala.util.Random  
object Lab7 {
	def main(args: Array[String]) {
  
  val valPar=List(3,4,5)
   val random_var = new Random
  val par = new FixedSizeArray(i => valPar(random.nextInt(valPar.length)))
  val s = Uniform(0,(8/13))
  val scorePar = Array.tabulate(18)((hole: Int) => 
        Select (s/8 -> par(i)-2, s/2 -> par(i)-1, s -> par(i), 4/5*(1 - 13*s/8) -> par(i)+1, 1/5*(1 - 13*s/8) -> par(i)+2)
    )
 
}
}
