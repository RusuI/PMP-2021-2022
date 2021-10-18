package Lab3

import com.cra.figaro.library.atomic._
import com.cra.figaro.language._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.library.compound._


object HelloWorld {
    def main(args: Array[String]) {
        val sideOfBed = Flip(0.2)
        val sunnyToday = Flip(0.2)

            val greetingToday = If(sideOfBed,
                If(sunnyToday,
                   Select(0.6 -> "Hello, world!", 0.4 -> "Howdy, universe!"),
                   Select(0.2 -> "Hello, world!", 0.8 -> "Oh no, not again!")),
                Select(1.0 -> "Oh no, not again!"))

            greetingToday.observe("Oh no, not again!")
            println(VariableElimination.probability(sunnyToday,true))
            //Rezultatul va depinde acum de cat va fi valoarea probabilitatii sideOfBed


            // Exercitiul 3

            //a.
            val x = Flip(0.4)
            val y = Flip(0.4)
            val z = x
            val w = x === z
            println(VariableElimination.probability(w,true)) // Rezultatul este 1.0 pentru ca x === z este adevarata


            //b. 
            val x1 = Flip(0.4)
            val y1 = Flip(0.4)
            val z1 = y1
            val w1 = x1 === z1
            println(VariableElimination.probability(w1,true))  // 0.52 deoarece prob ca x1 si z1 sa fie false este 0.6 * 0.6 = 0.32 si sa fie adevarate 0.4 * 0.4 = 0.16 
                                                              // 0.16 + 0.32 = 0.52
        }
    }