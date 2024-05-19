enum Player:
  case X
  case O

import Player.*

extension (player: Player)
  def other: Player = player match
    case X => O
    case O => X
