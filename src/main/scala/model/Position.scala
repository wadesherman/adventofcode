package model

case class Position (x: Long, y: Long){
  def n = Position(x, y - 1)
  def s = Position(x, y + 1)
  def e = Position(x + 1, y)
  def w = Position(x - 1, y)
  def ne = Position(x + 1, y - 1)
  def nw = Position(x - 1, y - 1)
  def se = Position(x + 1, y + 1)
  def sw = Position(x - 1, y + 1)

  def adjacent: List[Vector] = List(
    Vector(n, N),
    Vector(s, S),
    Vector(e, E),
    Vector(w, W),
    Vector(ne, NE),
    Vector(nw, NW),
    Vector(se, SE),
    Vector(sw, SW)
  )

  def cardinalNeighbors: List[Vector] = adjacent.filter(_.direction match {
    case _: CardinalDirection => true
    case _                    => false
  })

  def diagonalNeighbors: List[Vector] = adjacent.filter(_.direction match {
    case _: DiagonalDirection => true
    case _                    => false
  })

  def plus(p: Position): Position = Position(x + p.x, y + p.y)
}
