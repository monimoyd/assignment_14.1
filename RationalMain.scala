class Rational (x:BigInt, y:BigInt) {
    private val numerator:BigInt = x
    private val denominator:BigInt = y
    def this(a:BigInt) = this(a, 1)
    def sum(b: Rational):Rational = {
        return new Rational(numerator * b.denominator +  denominator * b.numerator, denominator * b.denominator)
    }
    def sum(b: BigInt):Rational = {
        return new Rational(numerator +  b, 1)
    }
    def subtract(b: Rational):Rational = {
        return new Rational(numerator * b.denominator -  denominator * b.numerator, denominator * b.denominator)
    }
    def subtract(b: BigInt):Rational = {
        return new Rational(numerator -  b, 1)
    }
    def multiply(b: Rational):Rational = {
        return new Rational(numerator * b.numerator, denominator * b.denominator)
    }
    def multiply(b: BigInt):Rational = {
        return new Rational(numerator *  b, 1)
    }
    def divide(b: Rational):Rational = {
        return new Rational(numerator * b.denominator, denominator * b.numerator)
    }
    def devide(b: BigInt):Rational = {
        return new Rational(numerator /  b, 1)
    }
    def compute_lcm(m: BigInt, n:BigInt):BigInt = {
        var a = m
        var b = n
        while ( a != b) {
            if (a<b )  a = a + m
            else b = b + n
        }
        return a
    }
    
    def compute_gcd(a:BigInt, b:BigInt) : BigInt = {
        var first_number:BigInt = 0
        var second_number:BigInt = 0
        if (a>b) {
            first_number = a
            second_number = b
        } else {
           first_number = b
           second_number = a
        }
        var remainder:BigInt = 1
        while (remainder != 0) {
            remainder = first_number % second_number
            first_number = second_number
            second_number = remainder
        }
        return first_number
    }

    def gcd(b:Rational) : Rational = {
        val x:BigInt = compute_gcd(numerator, b.numerator)
        val y:BigInt = compute_lcm(denominator, b.denominator)
        return new Rational(x, y)
    }

    def gcd(b:BigInt) : Rational = {
        val z:Rational = new Rational(b, 1)
        return gcd(y)
    }

    def printObject = println("numerator =" + numerator + " denominator=" + denominator)
}


object RationalMain {
  def main(args: Array[String]):Unit = {
      val r1 = new Rational(15,12)
      println("For RationaL Number r1")
      r1.printObject
      val r2 = new Rational(6,8)
      println("For RationaL Number r2")
      r2.printObject
      val sum_r1_r2 = r1.sum(r2)
      println(" For r1 + r2: ")
      sum_r1_r2.printObject
      val subtract_r1_r2 = r1.subtract(r2)
      println(" For r1 - r2: ")
      sum_r1_r2.printObject
      subtract_r1_r2.printObject
      val multiply_r1_r2 = r1.multiply(r2)
      println(" For r1 * r2: ")
      multiply_r1_r2.printObject
      val divide_r1_r2 = r1.divide(r2)
      println( " For r1 / r2: ")
      divide_r1_r2.printObject
      val gcd_r1_r2 = r1.gcd(r2)
      println (" gcd r1 and r2: ")
      gcd_r1_r2.printObject
      val r3 = new Rational(15)
      println("For RationaL Number r3")
      r3.printObject
      val r4 = new Rational(6)
      println("For RationaL Number r4")
      r4.printObject
      val sum_r3_r4 = r3.sum(r4)
      println(" For r3 + r4: ")
      sum_r3_r4.printObject
      val subtract_r3_r4 = r3.subtract(r4)
      println(" For r3 - r4: ")
      sum_r3_r4.printObject
      subtract_r3_r4.printObject
      val multiply_r3_r4 = r3.multiply(r4)
      println(" For r3 * r4: ")
      multiply_r3_r4.printObject
      val divide_r3_r4 = r3.divide(r4)
      println(" For r3 / r4: ")
      divide_r3_r4.printObject
      val gcd_r3_r4 = r3.gcd(r4)
      println(" gcd r3 and r4: ")
      gcd_r3_r4.printObject
  }
} 
