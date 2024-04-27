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

    ???
