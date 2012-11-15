import common._

object lesson1 {
  def abs(x: Double) = if (x < 0) -x else x       //> abs: (x: Double)Double

  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)           //> sqrtIter: (guess: Double, x: Double)Double

  def isGoodEnough(guess: Double, x: Double) = {
  	abs(1 - x/(guess * guess)) < 0.000001
  }                                               //> isGoodEnough: (guess: Double, x: Double)Boolean
  

  def improve(guess: Double, x: Double) =
    (guess + x / guess) / 2                       //> improve: (guess: Double, x: Double)Double

  def sqrt(x: Double) = sqrtIter(1.0, x)          //> sqrt: (x: Double)Double
  
  sqrt(0.01)                                      //> res0: Double = 0.10000000000139897
  
  sqrt(0.1e-20)                                   //> res1: Double = 3.1622778383672726E-11
  
  sqrt(1e20)                                      //> res2: Double = 1.0000000000023079E10
  
  sqrt(1e50)                                      //> res3: Double = 1.0000003807575104E25
}