package partial 


import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.algorithm.factored.VariableElimination

object Partial1 {
	def main(args: Array[String]) {
      // ne folisim de 2 variabile aleatoare ca sa modelam rzultatele celor 2 zaruri
	 val die1 = FromRange(1, 7) // zar 1 cu valori intre 1 si 6
	 val die1 = FromRange(1, 7) // zar 2 cu valori intre 1 si 6

	
     // ne folsim de apply ca sa cam calculam cu o functie anonima suma
	 val sumP1 = Apply(die1, die2, (i1: Int, i2: Int) => i1 + i2) //suma realizata
	
	// joc cu o singura runda si in functie de r4ezultat afisez cine va castiga
    val jocORunda = Apply( sum (s: Int) =>
      if (s == 7 || s == 11) "player1"
      else if (s == 2 ||  s== 3 || s == 12) "player2"
      else "remiza")

	  println(VariableElimination.probability(jocORunda, "player1")) 
  
	}
}