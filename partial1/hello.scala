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
	
    
// Varianta 1
     // joc cu o singura runda si in functie de r4ezultat afisez cine va castiga
     val jocORunda = Apply(sum (s: Int) =>
      if (s == 7 || s == 11) "player1"
      else if (s == 2 ||  s== 3 || s == 12) "player2"
      else "remiza")
      // 2. Folosim Variable Elimination ca jocOrUnda sa aiba castigator player 1
	  println(VariableElimination.probability(jocORunda, "player1"))
	  //3. Sa fie castigator player 2
	  println(VariableElimination.probability(jocORunda, "player1")) 




//Varianta 2
	  	// cream o dependenta in functie de rezultatul sumei
     val castigPrimul=RichCPD(sum,
        OneOf(7,11) -> Constant(true),
        OneOf(2,3,12)-> Constant(false))
		//pentru primul jucator
	    println(VariableElimination.probability(castigPrimul,true))
       
        //pentru al doilea jucator
		println(VariableElimination.probability(castigPrimul,false))


    // ex 4 (cel cu explicatii):
	// cele 2 rezultate difera datorita distributiilor rezultatelor.Sunt mai multe sanse ca castige al doilea, pentru ca el are mai multe valori care ii aduc victoria.
	 

  // ex5
  play(p1Wins: Element[Int], p2Wins: Element[Int], no: Int) :Element[Int]{
      

if(no == 0 )
return
else {
	val zar1jucator1 = FromRange(1,7)// zarul 1 pentru primul jucator
	val zar2jucator1 = FromRange(1,7)// zarul 1 pentru primul jucator
    val sumJucator1 = Apply(zar1jucator1, zar2jucator1, (i1: Int, i2: Int) => i1 + i2) //suma realizata 


    val zar1jucator2 = FromRange(1,7)// zarul 1 pentru primul jucator
	val zar2jucator2 = FromRange(1,7)// zarul 1 pentru primul jucator
    val sumJucator2 = Apply(zar1jucator1, zar2jucator1, (i1: Int, i2: Int) => i1 + i2) //suma realizata 


    val castigator = RichCPD(sumJucator1,sumJucator2,
	  (OneOf(1,7),NoneOf(2,3,12)) ->p1Wins
	  (NoneOf(1,7,OneOf(2,3,12)) -> p2Wins)
	  (*,*) -> 0
	  
	)


}
     }
	}
}