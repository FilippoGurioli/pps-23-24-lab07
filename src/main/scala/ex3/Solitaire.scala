package ex3

object Solitaire extends App:
  type Position = (Int, Int)
  type Solution = Seq[Position]

  val width: Int = 3
  val height: Int = 3

  def render(solution: Seq[Position], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  for sol <- placeMarks(width,height) do
    println(render(sol, width, height))

  def placeMarks(w: Int, h: Int): Iterable[Solution] = 
    val start = (w / 2, h / 2)
    solve(Seq(start), start)
    
  def moves(start: Postion, occupied: Seq[Position]): Seq[Position] =
    val (x, y) = start
    val possible = Seq((x + 2, y), (x - 2, y), (x, y + 2), (x, y - 2), (x + 1, y + 1), (x - 1, y - 1), (x + 1, y - 1), (x - 1, y + 1))
    possible.filter { case (x, y) => x >= 0 && x < w && y >= 0 && y < h && !occupied.contains((x, y)) }

  def solve(occupied: Seq[Position], position: Position): Solution =
    val next = moves(position, occupied)
    if next.isEmpty && !isValidSolution(occupied) then Seq.empty
    else if next.isEmpty then occupied
    else
      val solutions = for move <- next yield solve(occupied :+ move, move)
      solutions.find(isValidSolution).getOrElse(Seq.empty)

  def isValidSolution(occupied: Seq[Position]): Boolean = occupied.size == width * height