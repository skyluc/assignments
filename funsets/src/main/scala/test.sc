import funsets.FunSets._

object test {
  
  println()                                       //> 
  
  val s1 = singletonSet(2)                        //> s1  : Int => Boolean = <function1>
  
  printSet(s1)                                    //> {2}
  printSet(map(s1, _ * 4))                        //> {8}

}