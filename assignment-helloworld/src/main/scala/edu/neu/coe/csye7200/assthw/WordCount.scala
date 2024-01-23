package edu.neu.coe.csye7200.assthw

object WordCount {
  def main(args: Array[String]): Unit = {
    println("Enter a string:")
    val inputString = scala.io.StdIn.readLine()

    val wordCount = countWords(inputString)

    println(s"Word count: $wordCount")
  }

  def countWords(input: String): Int = {
    if (input == null || input.trim.isEmpty) {
      0
    } else {
      input.trim.split("\\s+").length
    }
  }
}

