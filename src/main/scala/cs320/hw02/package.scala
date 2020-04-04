package cs320

import cs320._

package object hw02 extends Homework02 {
  // applies a binary numeric function on all combinations of numbers from
  // the two input lists, and return the list of all of the results
  def binOp(
    op: (Int, Int) => Int,
    ls: List[Int],
    rs: List[Int]
  ): List[Int] = ls match {
    case Nil => Nil
    case l :: rest =>
      def f(r: Int): Int = op(l,r)
      rs.map(f) ++ binOp(op, rest, rs) //map applies all elements the same way
  }

  def run(str: String): List[Int] = {
    val value:MUWAE = MUWAE(str)
    interp(value,Map())

  }

  def interp(muw:MUWAE, env:Map[String,List[Int]]):List[Int] = muw match{
    case Num(nums) => nums
    case Add(left,right) => binOp((x,y)=>x+y,interp(left,env),interp(right,env))
    case Sub(left,right) => binOp((x,y)=>x-y,interp(left,env),interp(right,env))
    case With(name, expr, body) => interp(body, env+(name->interp(expr,env)))
    case Id(name) => lookup(name, env)
    case Min(left, mid, right)=> {
      val temp:List[Int] = binOp(findMin,interp(left,env),interp(mid,env))
      binOp(findMin, temp, interp(right,env))
    }
    case Max(left, mid, right) => {
      val temp:List[Int] = binOp(findMax,interp(left,env),interp(mid,env))
      binOp(findMax, temp, interp(right,env))
    }
  }
  def findMin(x:Int, y:Int):Int = {
    if(x>y) y else x
  }
  def findMax(x:Int, y:Int):Int = {
    if(x<y) y else x
  }
  def lookup(name:String, env:Map[String,List[Int]]): List[Int] = env.getOrElse(name, error(s"free identifier"))
    ///binop usage is needed in order to run the function.

  def tests: Unit = {
    test(run("{+ 3 7}"), List(10))
    test(run("{- 10 {3 5}}"), List(7, 5))
    test(run("{with {x {+ 5 5}} {+ x x}}"), List(20))
    test(run("{min 3 4 5}"), List(3))
    test(run("{max {+ 1 2} 4 5}"), List(5))
    test(run("{min {1 4} {2 9} 3}"), List(1, 1, 2, 3))
    test(run("{max {1 6} {2 5} {3 4}}"), List(3, 4, 5, 5, 6, 6, 6, 6))

    /* Write your own tests */
    test(run("{+ 3 {with {x {+ 1 2}} {+ x {2 4 6}}}}"), List(8, 10, 12))
    test(run("{+ {1 2} {1 2}}"), List(2, 3, 3, 4))
    test(run("{+ {} 7}"), List())
    test(run("{- 200 {+ 20 11}}"), List(169))
    test(run("{+ 3 {with {x {+ 2 5}} {+ x {1 2 3}}}}"), List(11, 12, 13))
    test(run("{- 20 {5 6}}"), List(15, 14))
    test(run("{with {x {1 3}} {+ x x}}"), List(2,4,4,6))
    test(run("{with {x {+ 6 6}} {with {x 10} {+ x x}}}"), List(20))
    test(run("{min {} {1 3 5} {2 4}}"), List())

    test(run("{min 4 {max {2 1} {3 1} 2} 3}"), List(3,2,3,2))
    test(run("{max 1 {max {1 2} {2 4} 3} {1 2}}"), List(3,3,4,4,3,3,4,4))
    test(run("{with {x {1 2}} {+ x x}}"), List(2,3,3,4))


  }
}
