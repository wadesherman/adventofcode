package model

case class Vector(position: Position, direction: Direction) {
  def next: Vector = direction match {
    case N  => this.copy(position = position.n)
    case S  => this.copy(position = position.s)
    case E  => this.copy(position = position.e)
    case W  => this.copy(position = position.w)
    case NE => this.copy(position = position.ne)
    case NW => this.copy(position = position.nw)
    case SE => this.copy(position = position.se)
    case SW => this.copy(position = position.sw)
  }
}
