package Lab8

import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._

object Ex3 {
	def main(args: Array[String]) {
		val length = 10
        val learn: Array[Element[String]] = Array.fill(length)(Constant(""))
        val pass: Array[Element[Boolean]] = Array.fill(length)(Constant(false))

        learn(0) = Select(0.1 -> "putin", 0.3 -> "mediu", 0.6 -> "mult")


        for { test <- 1 until length }
         {
             learn(test) = CPD( learn(test -1),
             ("putin")-> Select(0.6 -> "putin", 0.2 -> "mediu", 0.2 -> "mult"),
             ("mediu")-> Select(0.2 -> "putin", 0.6 -> "mediu", 0.2 -> "mult"),
             ("mult")-> Select(0.2 -> "putin", 0.2 -> "mediu", 0.6 -> "mult")
             )
         } 


         for { test <- 0 until length }
          {
              pass(test) = CPD( learn(test),
               ("putin")-> Flip(0.1),
               ("mediu")-> Flip(0.3),
               ("mult")-> Flip(0.6))
          }

      pass(0).observe(true)
      pass(1).observe(true)
      pass(2).observe(true)

      println("After observing : " +
      VariableElimination.probability(pass(9), true))
	}
}