package code

sealed trait Frame extends Product with Serializable
object Frame {
  case object Strike extends Frame
  final case class Spare(roll: Int) extends Frame
  final case class Open(roll1: Int, roll2: Int) extends Frame

  val strike: Frame = Strike

  def spare(roll: Int): Frame =
    if (roll < 10 && roll >= 0) Spare(roll)
    else
      throw new IllegalArgumentException(
        s"Cannot construct a spare frame from a roll of $roll"
      )

  def open(roll1: Int, roll2: Int): Frame = {
    val total = roll1 + roll2
    if (roll1 >= 0 && roll2 >= 0 && total < 10) Open(roll1, roll2)
    else
      throw new IllegalArgumentException(
        s"Cannot construct an open frame from rolls $roll1 and $roll2"
      )
  }
}
