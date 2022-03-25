import scala.util.Sorting

enum Couleur:
  case P(c:String)

  def valueOf(s:Int) : Couleur = s match{
    case 1 => Couleur.P("Carreau")
    case 2 => Couleur.P("Coeur")
    case 3 => Couleur.P("Trefle")
    case 4 => Couleur.P("Pique")
  }

enum Figure:
  case P(c:String)

  def valueOf(s:Int) : Figure = s match{
    case 2 => Figure.P("2")
    case 3 => Figure.P("3")
    case 4 => Figure.P("4")
    case 5 => Figure.P("5")
    case 6 => Figure.P("6")
    case 7 => Figure.P("7")
    case 8 => Figure.P("8")
    case 9 => Figure.P("9")
    case 10 => Figure.P("10")
    case 11 => Figure.P("Valet")
    case 12 => Figure.P("Dame")
    case 13 => Figure.P("Roi")
    case 14 => Figure.P("As")
  }

class PlayingCard(val couleur : Couleur, val figure : Figure):
  override def toString: String = "Couleur : " + couleur + " , Figure : " + figure;
  def memeFigure(x : PlayingCard) : Boolean = x.figure == this.figure;

object CouleurComparator extends scala.math.Ordering[PlayingCard]:
  override def compare(x: PlayingCard, y: PlayingCard): Int = x.couleur.ordinal - y.couleur.ordinal;

object FigureComparator extends scala.math.Ordering[PlayingCard]:
  override def compare(x: PlayingCard, y: PlayingCard): Int = x.figure.ordinal - y.figure.ordinal;


class PokerHand(list: List[PlayingCard]):
   def couleurComparator(): List[PlayingCard] = this.list.sorted(ord = CouleurComparator)
   def figureComparator(): List[PlayingCard] = this.list.sorted(ord = FigureComparator);

def freq(liste : List[PlayingCard], card : PlayingCard): Int = liste match {
  case moi :: suivant => if moi.memeFigure(card) then 1 + freq(suivant,card) else 0 + freq(suivant,card);
  case Nil => 0;
}



//On va maintenant faire une fonction pour chaque type de main que l'on utilisera par la suite pour le comparateur de main

//Quinte Flush Royale = 1
//Quinte Flush = 2
//Carré = 3
//Full = 4
//Couleur Flush = 5
//Suite (Quinte) = 6
//Brelan = 7
//Double Pair = 8
//Pair = 9
//Carte isolé = 10

def isQuinteFlushRoyale(main : List[PlayingCard]): Boolean = main(0).figure.ordinal == 14 && main(1).figure.ordinal == 13 && main(2).figure.ordinal == 12 && main(3).figure.ordinal == 11 && main(4).figure.ordinal == 10 && main(0).couleur.ordinal == main(1).couleur.ordinal && main(1).couleur.ordinal == main(2).couleur.ordinal  && main(2).couleur.ordinal == main(3).couleur.ordinal && main(3).couleur.ordinal == main(4).couleur.ordinal;

def isQuinteFlush(main : List[PlayingCard]): Boolean = main(0).figure.ordinal == main(1).figure.ordinal+1 && main(1).figure.ordinal == main(2).figure.ordinal+1 && main(2).figure.ordinal == main(3).figure.ordinal+1 &&  main(3).figure.ordinal == main(4).figure.ordinal+1 && main(0).couleur.ordinal == main(1).couleur.ordinal && main(1).couleur.ordinal == main(2).couleur.ordinal  && main(2).couleur.ordinal == main(3).couleur.ordinal && main(3).couleur.ordinal == main(4).couleur.ordinal;

def isCarre(main : List[PlayingCard]):Boolean = main match {
  case moi :: suivant => if freq(main,main(0))==4 then true else isCarre(suivant);
  case Nil => false;
}

def isFull //Je sais pas comment faire

def isCouleurFlush(main : List[PlayingCard]): Boolean = main(0).couleur.ordinal == main(1).couleur.ordinal && main(1).couleur.ordinal == main(2).couleur.ordinal  && main(2).couleur.ordinal == main(3).couleur.ordinal && main(3).couleur.ordinal == main(4).couleur.ordinal;

def isSuite(main : List[PlayingCard]): Boolean = main(0).figure.ordinal == main(1).figure.ordinal+1 && main(1).figure.ordinal == main(2).figure.ordinal+1 && main(2).figure.ordinal == main(3).figure.ordinal+1 &&  main(3).figure.ordinal == main(4).figure.ordinal+1

def isBrelan(main : List[PlayingCard]):Boolean = main match {
  case moi :: suivant => if freq(main,main(0))==3 then true else isBrelan(suivant);
  case Nil => false;
}

def isDoublePair(main : List[PlayingCard]):Boolean = main match { //Je sais pas comment faire
  case moi :: suivant =>
}

def isPair(main : List[PlayingCard]): Boolean = main match {
  case moi :: suivant => if freq(main,main(0))==2 then true else isPair(suivant);
  case Nil => false;
}

def highestFigure(main : List[PlayingCard], nb : Int):Int = main match {
  case moi :: suivant => if main(0).figure.ordinal > nb then highestFigure(suivant,main(0).figure.ordinal) else highestFigure(suivant,nb)
  case Nil => nb;
}

def combinaisonToValue(main : PokerHand):Int =
  if isQuinteFlushRoyale(main.figureComparator()) then 1
  else if isQuinteFlush(main.figureComparator()) then 2
  else if isCarre(main.figureComparator()) then 3
  else if isFull(main.figureComparator()) then 4
  else if isCouleurFlush(main.figureComparator()) then 5
  else if isSuite(main.figureComparator()) then 6
  else if isBrelan(main.figureComparator()) then 7
  else if isDoublePair(main.figureComparator()) then 8
  else if isPair(main.figureComparator()) then 9
  else 10;
















@main def mainProjet =
  val list = List(new PlayingCard(Couleur.P("Carreau"),Figure.P("3")),new PlayingCard(Couleur.P("Coeur"),Figure.P("3")));
  val poker = new PokerHand(list);
  println(isPair(list));





