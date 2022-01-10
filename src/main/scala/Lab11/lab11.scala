package lab11
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.factored.BeliefPropagation
import com.cra.figaro.language._
import com.cra.figaro.library.compound._

object Ex3{

	val pPresedinte = Flip(1.0/40000000.0)
	val pLeftHandedPres = If(pPresedinte, Flip(0.5), Flip(0.1))
	val pHarvardpres = If(pPresedinte, Flip(3.0/20.0), Flip(1.0/2000.0))

	def main(args: Array[Int])
	{
		// aplicarea algortimului VariableElimination.probability
        //subpunct a)
		pLeftHandedPres.observe(true)
		println("Probability to be president if you're left handed is " 
			+ VariableElimination.probability(pPresedinte,true))
		pLeftHandedPres.unobserve()

        //subpunct b)
		pHarvardpres.observe(true)
		println("Probability to be president if you have studied at Harvard is "
			+ VariableElimination.probability(pPresedinte,true))
		pHarvardpres.unobserve()
        
        //subpunct c)
		pLeftHandedPres.observe(true)
		pHarvardpres.observe(true)
		println("Probability to be president if you're left handed and you studied at Harvard is " 
			+ VariableElimination.probability(pPresedinte,true))

		// aplicarea algoritmului BeliefPropagation.probability
        //subpunct a)
		val algBelief = BeliefPropagation(100, pPresedinte)
		pLeftHandedPres.observe(true)
		algBelief.start()
		println("Probability to be president if you're left handed is " 
			+ algBelief.probability(pPresedinte,true))
		pLeftHandedPres.unobserve()
		algBelief.kill()
        //subpunct b)
		pHarvardpres.observe(true)
		algBelief.start()
		println("Probability to be president if you have studied at Harvard is "
			+ algBelief.probability(pPresedinte ,true))
		pHarvardpres.unobserve()
		algBelief.kill()
        //subpunct c)
		pLeftHandedPres.observe(true)
		pHarvardpres.observe(true)
		algBelief.start()
		println("Probability to be president if you're left handed and you studied at Harvard is " 
			+ algBelief.probability(pPresedinte,true))
		pLeftHandedPres.unobserve()
		pHarvardpres.unobserve()
		algBelief.kill()

		//aplicarea algortimului Importance.probability
		val algImportance = Importance(10, pPresedinte)
		//subpunct a
		pLeftHandedPres.observe(true)
		algImportance.start()
		println("Probability to be president if you're left handed is " 
			+ algImportance.probability(pPresedinte,true))
		pLeftHandedPres.unobserve()
		algImportance.kill()
		//subpunct B
		pHarvardpres.observe(true)
		algImportance.start()
		println("Probability to be president if you have studied at Harvard is "
			+ algImportance.probability(pPresedinte,true))
		pHarvardpres.unobserve()
		algImportance.kill()
		//subpunct c
		pLeftHandedPres.observe(true)
		pHarvardpres.observe(true)
		algImportance.start()
		println("Probability to be president if you're left handed and you studied at Harvard is " 
			+ algImportance.probability(pPresedinte,true))
		pLeftHandedPres.unobserve()
		pHarvardpres.unobserve()
		algImportance.kill()
	}
}
