import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine

import Utils._

case object Generator {

  @tailrec
  def guessNextSquareRoot(n: Int, guess: Int = 1): Int = {
    if (guess * guess == n) return guess
    if (guess * guess > n) return guess - 1
    guessNextSquareRoot(n, guess + 1)
  }

  def explain(end: Int, pauseBetweenEach: Boolean = true): Unit = {

    val default = Console.BLUE
    val reset = Console.RESET

    def blue(data: Any): String = Console.BLUE + data + reset + default

    def white(data: Any): String = Console.WHITE + data + reset + default

    def green(data: Any): String = Console.GREEN + data + reset + default

    def yellow(data: Any): String = Console.YELLOW + data + reset + default

    def magenta(data: Any): String = Console.MAGENTA + data + reset + default

    def cyan(data: Any): String = Console.CYAN + data + reset + default

    def blink(data: Any): String = Console.BLINK + data + reset + default

    def reversed(data: Any): String = Console.REVERSED + data + reset + default

    def underlined(data: Any): String = Console.UNDERLINED + data + reset + default

    val primes = new ListBuffer[Int]
    var lastKnownSquareRoot = 1

    @inline
    def checkPrimeDivisors(n: Int): Int = {
      println(s"\nLooking for a potential prime divisor :")
      if (primes.isEmpty) {
        println(s"\n -> Oops... We don't have any prime divisor yet since we haven't created ${magenta(underlined("any"))} prime")
        println("    number so far. Let's create one !\n")
      }
      for (p <- primes) {
        if (p > lastKnownSquareRoot && lastKnownSquareRoot > 1) {
          print(s"\nNOT ${blink("trying with " + white(p))} (since ${white(p)} > ${green(lastKnownSquareRoot)})")
          return n
        }
        print(s"\n  ${cyan("*")}${" trying with "}${white(p)}")
        if (n % p == 0) return p
      }
      return n
    }

    println(white("=========================================================================================="))

    for (n <- 1 to end) {

      var isPerfectSquareRoot = false

      println(s"n = ${white(n)}   ${white("|")}   last known square root = ${green(lastKnownSquareRoot)}   ${white("|")}   number of primes created so far = ${magenta(primes.length)}")
      println(white("=========================================================================================="))

      val k = (n - 1) / 12
      if (n - 12 * k == 1) {

        val sqr = guessNextSquareRoot(n, lastKnownSquareRoot)
        isPerfectSquareRoot = (sqr * sqr == n)

        println(s"\n(${white(s"${n} - 1")}) is a multiple of ${reversed(12)} ($k x 12)")
        println(s"\nWhat is the square root of ${white(n)} ?")
        println(s" -> An approximated computation of the square root of ${white(n)} gives ${green(sqr)}\n")

        if (sqr != lastKnownSquareRoot) {
          println(s" => Ok, so from now on, and until further notice,")
          println(s"    If we don't find any prime divisor below (or equal) to ${green(sqr)},")
          println(s"    we won't need to look further to tell if the next numbers must become primes !")
        }
        else {
          println(s"Ok. Let's carry on with ${green(sqr)}...")
        }

        println(s"\nBy the way, does ${green(sqr)} x ${green(sqr)} = ${white(n)} ?")

        if (isPerfectSquareRoot) {
          println(s" -> ${yellow("Yes")} (${white(n)} is the square of ${green(sqr)})")
        } else {
          println(s" -> ${green("No.")}")
        }

        lastKnownSquareRoot = sqr
      }

      if (isPerfectSquareRoot) {
        println(s"\n => Great... For once we won't have to look for any divisor")
        println(s"    since we already know this one : ${green(s"${lastKnownSquareRoot}")}\n")
        println(yellow(s" => ${n} can not be prime"))
        checkIfPrimeOrNot(n, false)
      } else {
        val check = checkPrimeDivisors(n)
        if (check == n) {
          if (!primes.isEmpty) println("\n\nDidn't find any so far... Did you ?\n")
          println(green(s" => ${n} must become prime"))
          checkIfPrimeOrNot(n)
          primes.append(n)
        } else {
          println("   ->   " + yellow(s"${check}") + s" is a prime divisor of ${white(n)}")
          checkIfPrimeOrNot(n, false)
          println(yellow(s"\n => ${n} will not be prime"))
        }
      }
      println(white("\n=========================================================================================="))
      if (pauseBetweenEach) {
        println(white("Press [Enter] to continue..."))
        println(white("or type [q] and press [Enter] to quit."))
        val in = readLine()
        print("\u001b[%dA\u001b[2K".format(1)) // Move up 1 times and erase line content
        println("==========================================================================================")
        print("\u001b[%dA\u001b[2K".format(3)) // Move up 3 times and erase line content
        if (in == "q") return
      }
    }
    println(s"All along the way, ${magenta(primes.length)} prime numbers have been created !")
    println(white("=========================================================================================="))
  }

  private def apply(end: Int): Seq[Int] = {

    val primes = new ListBuffer[Int]
    var lastKnownSquareRoot = 1

    @inline
    def hasPrimeDivisor(n: Int): Boolean = {
      for (p <- primes) {
        if (p > lastKnownSquareRoot && lastKnownSquareRoot > 1) return false
        if (n % p == 0) return true
      }
      return false // {2, 3, 5, 7, 11}
    }

    @inline
    def isPerfectSquareRoot(n: Int): Boolean = {
      if ((n - 1) % 12 == 0) {
        val sqr = guessNextSquareRoot(n, lastKnownSquareRoot)
        lastKnownSquareRoot = sqr
        return sqr * sqr == n
      }
      return false
    }

    //for (n <- 1 to end if isPrime(n)) yield n
    for (n <- 1 to end if (!isPerfectSquareRoot(n) && !hasPrimeDivisor(n))) primes.append(n)

    return primes.toSeq
  }

  def main(args: Array[String]): Unit = {

    this.explain(26)

    println(this(26))
    
  }

}