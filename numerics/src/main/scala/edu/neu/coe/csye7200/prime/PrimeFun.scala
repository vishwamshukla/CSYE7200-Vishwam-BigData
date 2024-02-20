package edu.neu.coe.csye7200.prime
//object PrimeFun extends App {
//
//  // NOTE: This exercise concerns the values of p^2 % n where p is a prime number and n is a "magic number."
//  // You can get a lazy list of primes from edu.neu.coe.csye7200.prime.Prime.primes
//
//  // TODO read a set of numbers from the command line (set these with menu item: Run/Edit Configuration ... Program arguments).
//  // For each number (called the magicNumber) write out the number and the first 100 values, skipping the first two.
//  // The numbers should start after 12 and you shouldn't need more than 12 to see the pattern.
//  // The pattern should be obvious in just the first 10 results.
//  // Submit the file (Question 1)
//
//  // TODO using the one magic number that gives you the pattern, get a list of the first 100,000 numbers (again excluding the first two).
//  // Try to find the first number that doesn't match the pattern (there may be none).
//
//  private val numbers: Array[Int] = args map (_.toInt)
//
//  private def show(magicNumber: Int): String = {
//    // SOLUTION
//    // STUB
//    ???
//    // END
//
//    s"""$magicNumber: ${ys.mkString(",")}"""
//  }
//
//  for (n <- numbers) println(show(n))
//}

object PrimeFun extends App {

  // Read magic numbers from command line arguments
  private val numbers: Array[Int] = args.map(_.toInt)
  private def generatePattern(magicNumber: Int): LazyList[BigInt] = {
    // Generate lazy list of prime numbers
    val primes = edu.neu.coe.csye7200.prime.Prime.primes

    // Generate p^2 % n for each prime number p
    primes.map(p => (BigInt(p.x.toString) * BigInt(p.x.toString)) % BigInt(magicNumber)).drop(2)

  }
  private def showPattern(magicNumber: Int): String = {
    // Generate pattern for the magic number
    val pattern = generatePattern(magicNumber).take(100).toList

    s"$magicNumber: ${pattern.mkString(", ")}"
  }

  // Task 1 Output the pattern for each magic number
  println("Output for Task 1:")
  for (magicNumber <- numbers) {
    println(showPattern(magicNumber))
  }
}
