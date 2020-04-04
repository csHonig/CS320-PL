package cs320

package object ex01 extends Exercise01 {


  // Problem 1
  def freeIds(expr: WAE): Set[String] = {
    def freeRec(expr: WAE, set:Set[String]): Set[String] = expr match{
      case Num(n) => Set()
      case Add(left,right) => freeRec(left, set) ++ freeRec(right,set)
      case Sub(left,right) => freeRec(left, set) ++ freeRec(right,set)
      case With(name, expr, body) => freeRec(body, set + name) ++ freeRec(expr,set)
      case Id(x) => {
        if (set.contains(x)) Set()
        else Set(x)
      }
    }
    freeRec(expr, Set())
  }

  // Problem 2
 def bindingIds(expr: WAE): Set[String] = expr match{
    case Num(n) => Set()
    case Add(left,right) => bindingIds(left) ++ bindingIds(right)
    case Sub(left,right) => bindingIds(left) ++ bindingIds(right)
    case With(name, expr, body) =>  bindingIds(body) ++ bindingIds(expr) ++ Set(name)
    case Id(x) => Set()
  }

  // Problem 3
  def boundIds(expr: WAE): Set[String] = {
    def boundRec(wae:WAE, set:Set[String]): Set[String] = wae match{
      case Num(n) => Set()
      case Add(l, r) => boundRec(l, set) ++ boundRec(r,set)
      case Sub(l, r) => boundRec(l, set) ++ boundRec(r,set)
      case With(name, expr, body) => boundRec(expr,set) ++ boundRec(body, set + name)
      case Id(x) => {
        if (set.contains(x)) Set(x)
        else Set()
      }
    }
    boundRec(expr, Set())
  }

  // Tests
  def tests: Unit = {
    test(freeIds(WAE("{with {x 1} {+ x y}}")), Set("y"))
    test(freeIds(WAE("{with {z 2} 1}")), Set())
    test(bindingIds(WAE("{with {x 1} {+ x y}}")), Set("x"))
    test(bindingIds(WAE("{with {z 2} 1}")), Set("z"))
    test(boundIds(WAE("{with {x 1} {+ x y}}")), Set("x"))
    test(boundIds(WAE("{with {z 2} 1}")), Set())
    test(boundIds(WAE("{with {x 3} {+ x {with {y 7} {- x y}}}}")), Set("x", "y"))
    /* Write your own tests */
    // tests for freeIds
    test(freeIds(WAE("{with {x 3} {+ x {- 3 x}}}")), Set())
    test(freeIds(WAE("{with {x 3} {- a {+ 4 x}}}")), Set("a"))
    test(freeIds(WAE("{with {x 3} {- b {- a x}}}")), Set("a", "b"))
    test(freeIds(WAE("{with {x 3} {- a {- b {+ x b}}}}")), Set("a", "b"))
    test(freeIds(WAE("{with {x 3} {- y {with {y 7} {+ x {- b a}}}}}")), Set("a", "b", "y"))
    test(freeIds(WAE("{with {x t} {- x {with {y y} {+ x {- b a}}}}}")), Set("a", "b", "t", "y"))
    test(freeIds(WAE("{with {x {with {y 3} {- x y}}} {+ x y}}")), Set("x", "y"))
    test(freeIds(WAE("{+ {with {x 10} {with {x 3} {- y {with {y 7} {+ x {- c b}}}}}} {with {a a} a}}")), Set("a", "b", "c", "y"))
    test(freeIds(WAE("{+ {with {x 10} {with {x 3} {- y {with {y 7} {+ x {- c b}}}}}} {with {a d} a}}")), Set("b", "c", "d", "y"))
    test(freeIds(WAE("{+ {with {x 10} {with {x 3} {- y {with {y 7} {+ x {- c b}}}}}} {with {a d} z}}")), Set("b", "c", "d", "y", "z"))

    // tests for bindingIds
    test(bindingIds(WAE("{+ 3 {- x y}}")), Set())
    test(bindingIds(WAE("{with {y 3} {with {x x} y}}")), Set("x", "y"))
    test(bindingIds(WAE("{with {y 3} {with {y x} {+ x y}}}")), Set("y"))
    test(bindingIds(WAE("{with {y 3} {with {y {with {x {+ 3 y}} {- x y}}} {+ x y}}}")), Set("x", "y"))
    test(bindingIds(WAE("{with {z 3} {with {w {with {z {+ 3 y}} {- x y}}} {with {w y} {+ 7 w}}}}")), Set("w", "z"))

    // tests for boundIds
    test(boundIds(WAE("{with {x 3} {+ y 3}}")), Set())
    test(boundIds(WAE("{with {x 3} {+ x {- x y}}}")), Set("x"))

    test(boundIds(WAE("{with {x 3} {with {y x} {- 3 y}}}")), Set("x", "y"))
    test(boundIds(WAE("{with {x 3} {+ y {with {y x} {- 3 7}}}}")), Set("x"))
    test(boundIds(WAE("{with {x x} {+ y {with {y y} {- 3 {with {z 7} {- z x}}}}}}")), Set("x", "z"))
    test(boundIds(WAE("{with {x {with {y 3} {+ x y}}} {+ y {with {y y} {- 3 7}}}}")), Set("y"))
    test(boundIds(WAE("{with {x a} {with {y b} {with {z c} {+ d {- x {+ y z}}}}}}")), Set("x", "y", "z"))
    test(boundIds(WAE("{+ {with {x 10} {with {x 3} {- y {with {y 7} {+ x {- c b}}}}}} {with {a d} a}}")), Set("a", "x"))
    test(boundIds(WAE("{+ {with {x 10} {with {x 3} {- y {with {y 7} {+ x {- c b}}}}}} {with {a d} z}}")), Set("x"))
  }
}
