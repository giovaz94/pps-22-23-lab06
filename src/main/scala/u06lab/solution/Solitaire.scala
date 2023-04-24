package u06lab.solution

import scala.:+

object Solitaire:
  type Position = (Int, Int)
  type Path = Iterable[Position]
  def newBoard(width: Int, height: Int): Solitaire = new SolitaireImpl(width,height)
  trait Solitaire:
    def placeMarks(n: Int): Seq[Path]
    def render(solution: Path): String

  private[Solitaire] class SolitaireImpl(width: Int, height:Int) extends Solitaire:
    extension (actual: Position)
      def isCompatibleWith (other: Position): Boolean =
        ((actual._1 - other._1).abs + (actual._2 - other._2).abs) == 2

    override def placeMarks(n:Int) : Seq[Path] = n match
      case 1 => List(Set((width / 2, height / 2)))
      case _ =>
        for
          solution <- placeMarks(n-1)
          x <- 0 until width
          y <- 0 until height
          cell = (x,y)
          if !solution.toSeq.contains(cell)
          if cell isCompatibleWith solution.last
        yield solution.toSeq :+ cell

    override def render(solution: Path): String =
      val reversed = solution.toSeq.reverse
      val rows =
        for y <- 0 until height
            row = for x <- 0 until width
                      number = reversed.indexOf((x, y)) + 1
            yield if number > 0 then "%-2d ".format(number) else "X  "
        yield row.mkString
      rows.mkString("\n")




@main def testSolitaire(): Unit =
  import Solitaire.*
  val board = newBoard(5,5)
  val solutions = board.placeMarks(13)
  solutions foreach (solution => println(board.render(solution) + "\n"))
