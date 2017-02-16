//################## Currying ###########################

//curried function with 3 param groups
def totalCost(donutType: String)(quantity: Int)(discount: Double,tax:Int): Double = {
  println(s"Calculating total cost for $quantity $donutType with ${discount * 100}% discount")
  val totalCost = 2.50 * quantity
  totalCost - (totalCost * discount)
}

totalCost("asf")(3)(4.0,2)

//################## partial functions ##################
//note the syntax for type of partial function.
val costForX: (Int) => (Double, Int) => Double = totalCost("X")_//alternate syntax is totalCost("X")
costForX(3)(4.0,2)

//#################### named functions #####################

//full syntax for a named function
val taxCal: (Double) => Double = (x:Double) => x*1.1
//with type inference for return type
val taxCal1 = (x:Double) => x*1.1
//not so better way with type inference for params
val taxCal2: (Double) => Double = (x) => x*1.1

//converting any method to a named function
def method(s:String,i:Int): Double = { i.toDouble}
val func: (String, Int) => Double = method _
//converting a curried to named function
val totalCostFunc: (String) => (Int) => (Double, Int) => Double = totalCost _


//################## higher order functions ##################

def totalCostwithTax(donutType: String)(quantity: Int)(discount: Double)(taxFunc:Double => Double): Double = {
  println(s"Calculating total cost for $quantity $donutType with ${discount * 100}% discount")
  val totalCost = 2.50 * quantity
  taxFunc(totalCost - (totalCost * discount))
}

def taxCalMethod(price :Double): Double = price*1.1

totalCostwithTax("asf")(3)(4.0)(taxCalMethod(_))//using a method
totalCostwithTax("asf")(3)(4.0)(taxCalMethod)//using a method with out args,needs to check how this works with 22 args
totalCostwithTax("asf")(3)(4.0)(taxCal)//using named function
totalCostwithTax("asf")(3)(4.4)(x => x*1.1)//function literal

//################## higher order  and partial functions ##################
//how the type is chained in partial function,this show that this partial function can be further slip on the other 2 params groups
val totalCostwithTaxPartial: (Int) => (Double) => ((Double) => Double) => Double = totalCostwithTax("asf")_
val totalCostwithTaxPartial2: (Double) => ((Double) => Double) => Double = totalCostwithTax("asf")(2)_
val totalCostwithTaxPartial3: ((Double) => Double) => Double = totalCostwithTax("asf")(2)(4.5)_

totalCostwithTaxPartial(3)(4.4)((x:Double) => x*1.1)
totalCostwithTaxPartial2(4.4)(x => x*1.1)
totalCostwithTaxPartial3(x => x*1.1)



//################## Implicit parameters ###########################
implicit val x = 4
implicit val y = 3.0
//problem if I have one more int as implicit
//we can not have two implicit params of same type.

def cost(items:Int)(implicit price:Int,tax:Double)={
  items*price+tax
}

cost(2)

//################## Implicit convertions ###########################

//implicits helps us in extending any class
//lets extend a string class to have a method called isRich.
//define a new class with additional methods
//make a object which encapsulates an implicit method to conert the string to richString
//import this in scope and use the additional methods on string
class Richstring(s:String){
  def isRich: Boolean = s.length > 5
}

object StringConverter{
  implicit def stringTORichString(s:String): Richstring =new Richstring(s)
}

//implicit def stringTORichString(s:String): Richstring =new Richstring(s)
import StringConverter._
val s = "aaf"

s.isRich

//################## Generic functions ###########################

def applyDiscountWithReturnType[T,U](discount: T): Seq[U] = {
  discount match {
    case d: String =>
      println(s"Lookup percentage discount in database for $d")
      Seq(discount.asInstanceOf[U])
    case d: Double =>
      println(s"$d discount will be applied")
      Seq(discount.asInstanceOf[U])
    case d @ _ =>
      println("Unsupported discount type")
      Seq(discount.asInstanceOf[U])
  }
}


applyDiscountWithReturnType[Double,String](5.0)

//################## varargs ###########################

def printReport(s:String*): Unit ={
  println(s" ${s.mkString(",")}")
}


printReport("afasf","asfasf","fasfasf")
//printReport(List("dqwwq","fwfqsf"))//wont compile
printReport(List("dqwwq","fwfqsf"):_*)//type ascription
