package model

sealed trait Direction
sealed trait CardinalDirection extends Direction
sealed trait DiagonalDirection extends Direction
case object N extends CardinalDirection
case object S extends CardinalDirection
case object E extends CardinalDirection
case object W extends CardinalDirection
case object NE extends DiagonalDirection
case object NW extends DiagonalDirection
case object SE extends DiagonalDirection
case object SW extends DiagonalDirection