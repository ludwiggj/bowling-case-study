package code

import org.scalacheck.Gen

object Generators {
  def genOpen: Gen[Frame] = {
    for {
      misses <- Gen.choose(1, 10)
      hits = 10 - misses
      roll1 <- Gen.choose(0, hits)
      roll2 = hits - roll1
    } yield Frame.open(roll1, roll2)
  }

  def genSpare: Gen[Frame] =
    for {
      misses <- Gen.choose(1, 10)
      roll1 = 10 - misses
    } yield Frame.spare(roll1)

  def genStrike: Gen[Frame] =
    Gen.const(Frame.strike)

  def genFrame: Gen[Frame] =
    Gen.oneOf(genOpen, genSpare, genStrike)

  def genFinal: Gen[FinalFrame] =
    makeGenFinal(genFrame, Gen.choose(0, 10))

  def makeGenFinal(frame: Gen[Frame], bonus: Gen[Int]): Gen[FinalFrame] =
    for {
      f <- frame
      ff <- f match {
        case Frame.Strike =>
          for {
            b1 <- bonus
            b2 <- bonus
          } yield FinalFrame.strike(b1, b2)

        case Frame.Spare(r) =>
          for {
            b <- bonus
          } yield FinalFrame.spare(r, b)

        case Frame.Open(r1, r2) =>
          Gen.const(FinalFrame.open(r1, r2))
      }
    } yield ff

  def genGame(genFrame: Gen[Frame], genFinal: Gen[FinalFrame]): Gen[Game] = {
    for {
      frames <- Gen.listOfN(9, genFrame)
      last <- genFinal
    } yield Game(frames, last)
  }
}
