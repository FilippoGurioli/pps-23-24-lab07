package ex3

object Solitaire extends App:
  type Position = (Int, Int)
  type Solution = Seq[Position]

  val width: Int = 7
  val height: Int = 7
  val isLogEnabled = false

  def render(solution: Seq[Position], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  val solutions = placeMarks(width, height)
  println(s"number of solutions: ${solutions.size}")
  for sol <- solutions do
    println(render(sol.toSeq, width, height))

  def placeMarks(w: Int, h: Int): Iterable[Solution] = 
    val start = (w / 2, h / 2)
    solve(Seq(start), start).filter(!_.isEmpty)
    
  def solve(occupied: Seq[Position], position: Position): Seq[Solution] =
    val next = moves(position, occupied)
    log("current occupied are: ")
    var occ = ""
    for o <- occupied yield occ += s" $o"
    log(occ)
    log("")
    if isValidSolution(occupied) then Seq(occupied)
    else if !next.isEmpty then 
      for m <- next yield
        solve(occupied :+ m, m).flatten
    else Seq.empty

  def moves(start: Position, occupied: Seq[Position]): Seq[Position] =
    val (x, y) = start
    val possible = Seq((x + 2, y), (x - 2, y), (x, y + 2), (x, y - 2), (x + 1, y + 1), (x - 1, y - 1), (x + 1, y - 1), (x - 1, y + 1))
    possible.filter { case (x, y) => x >= 0 && x < width && y >= 0 && y < height && !occupied.contains((x, y)) }

  def isValidSolution(occupied: Seq[Position]): Boolean = occupied.size == width * height

  def log(message: String) =
    if isLogEnabled then println(message)