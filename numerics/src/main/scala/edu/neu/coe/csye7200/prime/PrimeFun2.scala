package edu.neu.coe.csye7200.prime

object PrimeFun2 extends App {

  // Read magic numbers from command line arguments
  private val numbers: Array[Int] = args.map(_.toInt)

  private def generatePattern(magicNumber: Int): LazyList[BigInt] = {
    // Generate lazy list of prime numbers
    val primes = edu.neu.coe.csye7200.prime.Prime.primes

    // Generate p^2 % n for each prime number p
    primes.map(p => (BigInt(p.x.toString) * BigInt(p.x.toString)) % BigInt(magicNumber)).drop(2)

  }

  // Function to show the pattern for a given magic number
  private def showPattern(magicNumber: Int): String = {
    // Generate pattern for the magic number
    val pattern = generatePattern(magicNumber).take(100).toList

    // Format the output string
    s"$magicNumber: ${pattern.mkString(", ")}"
  }

  // Task 2: Find the first number that doesn't match the pattern
  println("\nOutput for Task 2:")
  val patternForFirstMagicNumber = generatePattern(numbers.head).take(100000).toList
  val firstNonMatchingNumber = (12 to Int.MaxValue).find(i => generatePattern(i).take(100000).toList != patternForFirstMagicNumber)

  firstNonMatchingNumber match {
    case Some(number) => println(s"First number that doesn't match the pattern: $number")
    case None => println("No number found that doesn't match the pattern within the specified range.")
  }
}
