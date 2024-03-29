/*
 * Copyright (c) 2019. Phasmid Software
 */

package edu.neu.coe.csye7200.fp.list

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * @author scalaprof
  */
class ListSpec extends AnyFlatSpec with Matchers {

  behavior of "Nil"

  it should "have zero length" in {
    Nil.length should be(0)
  }
  it should "equal Nil" in {
    Nil.equals(Nil) shouldBe true
  }
  it should "be empty" in {
    Nil.isEmpty shouldBe true
  }
  it should "have Nil tail" in {
    an[Exception] should be thrownBy Nil.x3
  }
  it should "should throw an exception on get" in {
    val x: List[Nothing] = Nil
    an[IndexOutOfBoundsException] should be thrownBy x.x4(0)
  }
  it should "should return None for apply" in {
    val x: List[Nothing] = Nil
    x(0) shouldBe None
  }
  it should "yield list of value on prepend" in {
    Nil :+ 1 shouldBe List(1)
  }
  it should "yield list of value on append" in {
    1 +: Nil shouldBe List(1)
  }
  it should "leave any list unchanged on append" in {
    val x = List(1, 2, 3) // arbitrary
    (x ++ Nil).equals(x) shouldBe true
  }

  behavior of "List(1,2,3)"
  it should "have 3 length" in {
    List(1, 2, 3).length should be(3)
  }
  it should "equal IList(1,2,3)" in {
    List(1, 2, 3).equals(List(1, 2, 3)) shouldBe true
  }
  it should "not be empty" in {
    List(1, 2, 3).isEmpty shouldBe false
  }
  it should "have Nil tail" in {
    List(1, 2, 3).x3 should be(List(2, 3))
  }
  it should "be 3 on x4(2)" in {
    val x: List[Int] = List(1, 2, 3)
    x.apply(2) shouldBe Some(3)
  }
  it should "be List(1,2,3) on map" in {
    val x: List[Int] = List(1, 2, 3)
    x.x7(_.toString) shouldBe List("1", "2", "3")
  }
  it should "be List(1,2,3) on flatMap" in {
    val x: List[Int] = List(1, 2, 3)
    x.x8({ e => List(e.toString) }) shouldBe List("1", "2", "3")
  }
  it should "be List(1,2,3,4,5,6) on ++" in {
    val x: List[Int] = List(1, 2, 3)
    val y: List[Int] = List(4, 5, 6)
    (x ++ y) shouldBe List(1, 2, 3, 4, 5, 6)
  }
  it should "have length 2 on ++" in {
    val x: List[CharSequence] = List(new StringBuffer("A"))
    val y: List[String] = List("B")
    (x ++ y).length should be(2)
  }
  it should "have length 2 on ++ (2)" in {
    val x: List[CharSequence] = List(new StringBuffer("A"))
    val y: List[String] = List("B")
    (y ++ x).length should be(2)
  }
  it should "sum by iteration" in {
    val xs: List[Int] = List(1, 2, 3)
    xs.sumByIteration shouldBe 6
  }
  it should "foldLeft 1" in {
    val xs: List[Int] = List(1, 2, 3)
    xs.foldLeft(0)(_+_) shouldBe 6
  }
  it should "foldLeft 2" in {
    val xs: List[Int] = List(1, 2, 3)
    xs.foldLeft(6)(_ - _) shouldBe 0
  }
  it should "foldLeft 3" in {
    val xs: List[Int] = List(1, 2, 3)
    xs.foldLeft(-2)(_ + _) shouldBe 4
  }
  it should "foldRight 1" in {
    val xs: List[Int] = List(1, 2, 3)
    xs.foldRight(0)(_ + _) shouldBe 6
  }
  it should "foldRight 2" in {
    val xs: List[Int] = List(1, 2, 3)
    xs.foldRight(6)(-_ + _) shouldBe 0
  }
  it should "foldRight 3" in {
    val xs: List[Int] = List(1, 2, 3)
    xs.foldRight(-2)(_ + _) shouldBe 4
  }
}