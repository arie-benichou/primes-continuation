import scala.annotation.tailrec

object Utils {

  def isPrime(n: Int): Boolean = {

    if (n < 0) throw new Exception(s"${n} is not a valid integer !")

    if (n == 1) return false
    if (n == 2) return true
    if (n == 3) return true

    if (n % 2 == 0) return false

    @tailrec
    def _isPrime(i: Int): Boolean = {
      if (i * i > n) return true
      if (n % i == 0) return false
      return _isPrime(i + 2)
    }

    return _isPrime(3)
  }

  def checkIfPrimeOrNot(n: Int, isExpectedToBePrime: Boolean = true) = {
    if (isPrime(n) != isExpectedToBePrime)
      throw new Exception(s"${Console.RED}inconsistent result with ${n} !")
  }

}