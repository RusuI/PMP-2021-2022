package test2
import com.cra.figaro.language.{Select, Apply, Constant, Element, Chain, Universe, Flip}
import com.cra.figaro.library.compound.{If, CPD, RichCPD, OneOf, *, ^^}
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.library.atomic.continuous.Normal
import com.cra.figaro.library.atomic.continuous.Uniform
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.algorithm.factored.beliefpropagation.BeliefPropagation

object test2 {

    class Autor {
		 var popularitate = Flip(1.0/0.6)
	}

    class Album( val autor: Autor)
    {
		 var calitate= Select(0.27 -> "mica", 0.6 ->"medie",0.3 ->"mare")
	}

    class Nominalizare(val album: Album){

     def getProb(album: String, autor:Boolean): Element[Double] = {

                                                         CPD(album.calitate, album.autor.popularitate,
                                                          ("mica", false) -> Flip(0.003),
                                                          ("mica", true) -> Flip(0.014),
                                                          ("medie", false) -> Flip(0.016),
                                                          ("medie", true) -> Flip(0.043),
                                                          ("mare", false) -> Flip(0.047),
                                                          ("mare", true) -> Flip(0.18) )

    }
    }
    
  def main(args: Array[String]) {
      count = 5
     val Autori: Array[Autor] = Array.fill(count)(new Autor())
      val Albume: Array[Album] = Array.fill(count)(new Album())

  }
}