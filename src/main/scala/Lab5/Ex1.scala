package Lab5

import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.language.{Apply, Constant, Element, Flip}
import com.cra.figaro.library.compound.If

object Ex6
{
	def tennis(probP1ServeWin: Double, probP1Winner: Double,probP1OutError: Double,probP1NetError: Double, probP2ServeWin: Double, probP2Winner: Double, probP2OutError: Double,probP2NetError: Double,pCorrectServeOut:Double, pWrongServeIn:Double): Element[Boolean] = 
	{
		def rally(firstShot: Boolean, player1: Boolean): Element[Boolean] = 
		{
			val pWinner = if (firstShot && player1) probP1ServeWin //aici incercam sa terminam care probabilitate va fi luata in considerara( as pentru primul jucator, as pentru al doilea sau probabilitatile de castig din rever pentru fiecare dintre jucatori)
							else if (firstShot && !player1) probP2ServeWin
								else if (player1) probP1Winner
										else probP2Winner
			
			val pNetError = if (player1) probP1NetError else probP2NetError
			val pOutError = if (player1) probP1OutError else probP2OutError
			val neterror = Flip(pNetError)
			If(neterror, //daca nu a fost eroare in fileu testam daca e eroare de out, daca nu, nu mai are sens sa testam pe cazul in care avem out
				val outerror = Constant(0),
				val outerror = Flip(pOutError)
				)		

			val winner = Flip(pWinner)
			val error = Flip(pError)
			val correctServeOut = Flip(pCorrectServeOut)
			val wrongServeIn = Flip(pWrongServeIn)

			//verificam daca s-a castigat runda sau nu, si cui anume i se atribuie victoria
				If(winner,//daca e punct castigat, verificam al cui este
				    If(!correctServeOut) //aici testam sa vedem daca serva care a fost corecta, a fost considerata corecta si de catre arbitru
				       Constant(player1),
					   Constant(!player1) // desi a fost corecta, arbotrul a faacut o eroare si a considerat ca este out asa ca se joaca din nou ????

				If(neterror, 
					Constant(!player1),  //daca e eroare de fileu, castiga al doilea punctul
					If(outerror,  //daca e eroare de out
					    If(!wrongServeIn) //si a fost  arbitrata corect, ca fiind out 
						   Constant(!player1),  //castiga al doilea
						   Constant(player1) //in caz contrat, punctul desi este out, a fost considerat in de catre arbitru si castiga primul player
						rally(false,!player1) // nu a castigat si nici nu e eroare, deci se continua raliu   
					)
				)
			)
			)
		}

        //aici modelam un game
		def game(p1Serves: Boolean, p1Points: Element[Int], p2Points: Element[Int]): Element[Boolean] =
		{

			//luam rezultatul din urma unei runde
			val p1WinsPoint = rally(true, p1Serves)

            //verificam mai departe pentru fiecare caz, cel in care e punctul castiga player1(adica atunci cand e true) sau cel in care castiga playr2(false) si ii incrementam numarul de puncte dupa caz
			val newP1Points = Apply(p1WinsPoint, p1Points,
								(wins: Boolean, points: Int) =>
									if (wins) points + 1 else points
			)

			val newP2Points = Apply(p1WinsPoint, p2Points,
								(wins: Boolean, points: Int) =>
									if (wins) points else points + 1
			)

//mai departe testam sa vedem in functie de numarul de puncte daca s-a castigat un set sau nu, adica dca diferenta este de cel putin 2 punct si daca s a ajuns la macar 4 puncte
			val p1WinsGame = Apply(newP1Points, newP2Points,
								(p1: Int, p2: Int) =>
									p1 >= 4 && p1 - p2 >= 2
			)
// la fel si pentru al doilea jucator
			val p2WinsGame = Apply(newP2Points, newP1Points,
								(p2: Int, p1: Int) =>
									p2 >= 4 && p2 - p1 >= 2
			)
//game- ul se terminca cand unul din jucatori a castigat, adica oricare dintre cele 2 prob este pe true
			val gameOver = p1WinsGame || p2WinsGame


//utilizand structura probabilista if, dam rezultatul functiei daca gameOver este pe true, se returneaza castigatorul, true pentru primul, false pt al doilea, in caz contrat, se continua gameul apeland functia cu punctajele actualizate
			If(gameOver, 
				p1WinsGame, 
				game(p1Serves, newP1Points, newP2Points)
			)
		}


        //aici modelam jocul practic, pe seturi
		def play(p1Serves: Boolean, p1Sets: Element[Int], p2Sets: Element[Int],
					p1Games: Element[Int], p2Games: Element[Int]): Element[Boolean] =
		{
			val p1WinsGame = game(p1Serves, Constant(0), Constant(0)) // luam de aici rezultatul din urma unui game, adica cine a castigat
			val newP1Games = Apply(p1WinsGame, p1Games, p2Games,  //daca a e game de castigat de player 1 si avem mai muult de 5 sau macar 5 game uri, reinitializam numarul de game-uri cu 0 pentru un nou set
								(wins: Boolean, p1: Int, p2: Int) =>
									if (wins)
									{
										if (p1 >= 5) 0 else p1 + 1  
									}
									else // daca a castigat al doilea, facem acelasi lucru pentru player 2
									{
										if (p2 >= 5) 0 else p1
									}
			)

			val newP2Games = Apply(p1WinsGame, p1Games, p2Games,
								(wins: Boolean, p1: Int, p2: Int) =>
									if (wins)
									{
										if (p1 >= 5) 0 else p2
									}
									else
									{
										if (p2 >= 5) 0 else p2 + 1
									}
			)
  // aici modificam numarul de seturi casttigate pentru fiecare jucator in parte
			val newP1Sets = Apply(p1WinsGame, p1Games, p1Sets,
								(wins: Boolean, games: Int, sets: Int) =>
									if (wins && games == 5) //daca a castgat game si numarul vechi e egal cu 5, crestem numarul de seturi castigate
										sets + 1
									else 
										sets //daca nu, ramane la fel
			)

			val newP2Sets = Apply(p1WinsGame, p2Games, p2Sets,
								(wins: Boolean, games: Int, sets: Int) =>
									if (!wins && games == 5) //idem ca in cazul de mai sus
										sets + 1
									else
										sets
			)

			val matchOver = Apply(newP1Sets, newP2Sets,
								(p1: Int, p2: Int) => //jocul e gata cand unul din jucatori a castigat cel putin 2 seturi, deci verificam nr de seturi pentru fiecare jucator
									p1 >= 2 || p2 >= 2
			)
			
			If(matchOver, //daca meciul s a terminat
				Apply(newP1Sets, (sets: Int) => sets >= 2), 
				play(!p1Serves, newP1Sets, newP2Sets, newP1Games, newP2Games) //continuam cu serviciul pentru al doilea jucator
			)
		}

		play(true, Constant(0), Constant(0), Constant(0), Constant(0))
	}


	def main(args: Array[String])
	{
		val tennis_match = tennis(0.5, 0.5, 0.5, 0.5, 0.5, 0.5)
		val alg = Importance(200, tennis_match)
		alg.start()
		alg.stop()
		println("Expected gain:" + alg.probability(tennis_match, true))
	}
}
