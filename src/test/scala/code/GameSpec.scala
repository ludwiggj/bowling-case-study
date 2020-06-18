package code

import org.scalacheck._
import org.scalacheck.Prop._

object GameSpecification extends Properties("Game") {
  import Frame._
  import Generators._

  property("score of game with all open frames is simple sum of rolls") =
    forAll(genGame(genOpen, makeGenFinal(genOpen, Gen.choose(0, 0)))) { game =>
      val framesTotal =
        game.frames.collect { case Open(r1, r2) => r1 + r2 }.sum
      val finalTotal =
        game.finalFrame match {
	        case FinalFrame.Strike(bonus1, bonus2) => 10 + bonus1 + bonus2
	        case FinalFrame.Spare(roll, bonus) => 10 + bonus
	        case FinalFrame.Open(roll1, roll2) => roll1 + roll2
        }

      game.score ?= (framesTotal + finalTotal)
    }

  property("score of game with all strikes is 300") = forAll(
    genGame(genStrike, makeGenFinal(Gen.const(Frame.strike), Gen.const(10)))
  ) { game => game.score ?= 300 }

  property("score of game with all 5/ is 150") = forAll(
    genGame(
      Gen.const(Frame.spare(5)),
      makeGenFinal(Gen.const(Frame.spare(5)), Gen.const(5))
    )
  ) { game => game.score ?= 150 }

  property("score of game is within limits") =
    forAll(genGame(genFrame, genFinal)) { game =>
      val score = game.score
      val framesLowerLimit = game.frames.map { f =>
        f match {
          case Strike             => 10
          case Spare(roll)        => 10
          case Open(roll1, roll2) => roll1 + roll2
        }
      }.sum
      val finalLowerLimit =
        game.finalFrame match {
	        case FinalFrame.Strike(bonus1, bonus2) => 10 + bonus1 + bonus2
	        case FinalFrame.Spare(roll, bonus) => 10 + bonus
	        case FinalFrame.Open(roll1, roll2) => roll1 + roll2
        }
      val lowerLimit = framesLowerLimit + finalLowerLimit

      (score <= 300) && (score >= lowerLimit)
    }
}
