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
  override def toString: String = "Couleur : " + couleur + ", Figure : " + figure;
;

class CouleurComparator extends scala.math.Ordering[PlayingCard]:
  override def compare(x: PlayingCard, y: PlayingCard): Int = x.couleur.ordinal - y.couleur.ordinal;

class FigureComparator extends scala.math.Ordering[PlayingCard]:
  override def compare(x: PlayingCard, y: PlayingCard): Int = x.figure.ordinal - y.figure.ordinal;