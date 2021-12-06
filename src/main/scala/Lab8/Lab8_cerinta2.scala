package Lab8

import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.discrete.Uniform

object Ex3 {
	def main(args: Array[String]) {
		val length = 10
        val learn: Array[Element[String]] = Array.fill(length)(Constant(""))
        val pass: Array[Element[Boolean]] = Array.fill(length)(Constant(false))
        val points: Array[Element[Int]] =Array.fill(length)(Constant(0))

        learn(0) = Select(0.1 -> "putin", 0.3 -> "mediu", 0.6 -> "mult")


        for { test <- 1 until length }
         {
             learn(test) = CPD( learn(test -1),
             ("putin")-> Select(0.6 -> "putin", 0.2 -> "mediu", 0.2 -> "mult"),
             ("mediu")-> Select(0.2 -> "putin", 0.6 -> "mediu", 0.2 -> "mult"),
             ("mult")-> Select(0.2 -> "putin", 0.2 -> "mediu", 0.6 -> "mult")
             )
         } 


         for { test <- 1 until length }
         {
             points(test) = CPD( learn(test),
             ("putin")-> Uniform(1,2,3,4),
             ("mediu")-> Uniform(4,5,6,7),
             ("mult")-> Uniform(8,9,10)
            )
         } 


         for { test <- 0 until length }
          {
              pass(test) = Apply(points(test), 
              (i: Int) => if (i >= 5) true else false)
          }


       learn(2).observe("mult")
       println("After observing : " +
       VariableElimination.probability(points(3),3))

       learn(2).unobserve



        pass(5).observe(true)
        learn(5).observe("mult")
        // learn(5).unobserve
        // pass(5).unobserve
       println("After observing : " +
       VariableElimination.probability(pass(3),true))


      pass(0).observe(true)
      pass(1).observe(true)
      pass(2).observe(true)

      println("After observing : " +
      VariableElimination.probability(pass(9), true))
	}
}