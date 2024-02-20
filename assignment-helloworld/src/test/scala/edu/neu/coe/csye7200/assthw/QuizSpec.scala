package edu.neu.coe.csye7200.assthw

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class QuizSpec extends AnyFlatSpec with Matchers {
  behavior of "Quiz"
  it should "get the correct greeting" in {
    Quiz.greeting shouldBe "Hello World!"
  }
}