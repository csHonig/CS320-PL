package cs320

package object hw01 extends Homework01 {
  // 1. Primitives (20 pts)
  def volumeOfCuboid(a: Int, b: Int, c: Int): Int = a*b*c
  def concat(x: String, y: String): String = x+y

  // 2. Function Values (30 pts)
  def addN(n: Int): Int => Int = {
    def addMore(x:Int): Int = x+n
    addMore
  }
  def twice(f: Int => Int): Int => Int = {
    def answ(x:Int): Int = f(x)*2 -x
    answ
  }
  def compose(f: Int => Int, g: Int => Int): Int => Int = {
    def answ(x:Int): Int = g(f(x))
    answ
  }

  // 3. Data Structures (50 pts)
  // 3.1. Lists (20 pts)
  def double(l: List[Int]): List[Int] = {
    val t:List[Int] = l.map(_*2)
    t
  }
  def sum(l: List[Int]): Int = {
    val sum:Int = l.foldLeft(0)(_+_)
    sum
  }

  // 3.2. Maps (10 pts)
  def getKey(m: Map[String, Int], s: String): Int = {
    val ans = m.getOrElse(s,error(s))
    ans
  }

  // 3.3. User-defined Structures (20 pts)
  def countLeaves(t: Tree): Int = {
    def counter(c:Tree): Int = c match{
      case Branch(left,value,right) => counter(left)+counter(right)
      case Leaf(value) => 1
    }
    val answ:Int = counter(t)
    answ
  }
  def flatten(t: Tree): List[Int] = {
    def checker(c:Tree):List[Int] = c match{
      case Branch(left,value,right) => checker(left)++List(value)++checker(right)//left, then value, then value
      case Leaf(value) => List(value)
    }
    val answ:List[Int] = checker(t)
    answ
  }

  def tests: Unit = {
    test(concat("abc", "def"), "abcdef")
    test(addN(5)(3), 8)
    test(addN(5)(42), 47)
    test(twice(addN(3))(2), 8)
    test(twice(addN(3))(7), 13)
    test(compose(addN(3), addN(4))(5), 12)
    test(compose(addN(3), addN(4))(11), 18)

    val l: List[Int] = List(1, 2, 3)
    test(double(l), List(2, 4, 6))
    test(double(double(l)), List(4, 8, 12))

    test(sum(List(1,2,3)), 6)
    test(sum(List(4,2,3,7,5)), 21)

    val m: Map[String, Int] = Map("Ryu" -> 42, "PL" -> 37)
    test(getKey(m, "Ryu"), 42)
    test(getKey(m, "PL"), 37)
    testExc(getKey(m, "CS320"), "CS320")

    val tree: Tree = Branch(Leaf(1), 2, Branch(Leaf(3), 4, Leaf(5)))
    test(countLeaves(tree), 3)
    test(flatten(tree), List(1, 2, 3, 4, 5))

    /* Write your own tests */
    test(volumeOfCuboid(1,3,6),18)
    test(volumeOfCuboid(3,4,8),96)
    test(concat("ad","de"),"adde")
    test(concat("acd","def"),"acddef")
    test(addN(10)(2),12)
    test(addN(14)(16),30)
    test(twice(addN(10))(2), 22)
    test(twice(addN(8))(4), 20)
    test(compose(addN(2), addN(8))(2), 12)
    test(compose(addN(10), addN(5))(9), 24)

    val k: List[Int] = List(2, 5, 8, 11)
    test(double(k), List(4, 10, 16, 22))
    test(double(double(k)), List(8, 20, 32, 44))

    test(sum(List(2,5,8,11)), 26)
    test(sum(List(4,10,16,22)), 52)

    val tod: Map[String, Int] = Map("Oh" -> 41, "Jio" -> 25)
    test(getKey(tod, "Oh"), 41)
    test(getKey(tod, "Jio"), 25)
    testExc(getKey(tod, "BADBAD"), "BADBAD")

    val tr: Tree = Branch(Branch(Leaf(1),10,Branch(Leaf(2), 2, Leaf(2))), 2, Branch(Leaf(5), 4, Leaf(8)))
    test(countLeaves(tr), 5)
    test(flatten(tr), List(1, 10, 2, 2, 2, 2, 5, 4, 8))


  }
}
