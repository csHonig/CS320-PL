package cs320

package object hw03 extends Homework03 {
  def run(str: String): String = {
    val temp = MRFWAE(str)
    val tempo = interp(temp,Map())
    val ans = tempo match {
      case NumV(n) => n.toString
      case CloV(param,body,env)=> "function"
      case RecV(rec) => "record"
    }
    ans
  }
  trait MRFWAEval
  case class NumV(n: Int) extends MRFWAEval
  case class CloV(param:List[String], body:MRFWAE, env:Env) extends MRFWAEval
  case class RecV(r:Env) extends MRFWAEval
  type Env = Map[String, MRFWAEval]


  def interp(mrfwae:MRFWAE, env: Env) : MRFWAEval = mrfwae match{
    case Num(n) => NumV(n)
    case Add(l,r) => numVAdd(interp(l,env), interp(r,env))
    case Sub(l,r) => numVSub(interp(l,env), interp(r,env))
    case With(name, body, value) => interp(value, env+ (name ->interp(body,env)))
    case Id(x) => lookup(x,env)
    case App(f,a) =>interp(f,env) match {
      case CloV(params, body, fenv) => {
        if (a.length != params.length) error(s"wrong arity")
        else {
          val temp = changeVal(a, env)
          val j: Map[String, MRFWAEval] = ((params zip temp) toMap)
          interp(body, fenv ++ j)
        }
      }
      case _ => error(s"Error is Made")

    }
    case Fun(params, body)=> CloV(params, body, env)
    case Rec(rec) => {
      val ans = rec.map{case(key,value)=>(key,interp(value,env))}
      RecV(ans)
    }
    case Acc(expr, name)=> interp(expr,env) match {
      case RecV(r) => lookup(name,r)
      case _ => error(s"no such field")
    }
  }
  def changeVal(a:List[MRFWAE], env:Env): List[MRFWAEval] = a match{
    case Nil => Nil
    case l::rest => List(interp(l,env))++ changeVal(rest, env)
  }




  def lookup(x:String, env:Env): MRFWAEval = env.getOrElse(x,error(s"no such field"))
  def numVAdd(left:MRFWAEval, right:MRFWAEval): MRFWAEval = (left,right) match{
    case (NumV(n),NumV(m)) => NumV(n+m)
    case _ => error(s"both aren't Nums")
  }
  def numVSub(left:MRFWAEval, right:MRFWAEval): MRFWAEval = (left, right) match{
    case(NumV(n), NumV(m)) => NumV(n-m)
    case _ => error(s"both aren't Nums")
  }
  def tests: Unit = {
    test(run("{{fun {x y} {+ x y}} 1 2}"), "3")
    test(run("{{fun {} {+ 3 4}}}"), "7")
    testExc(run("{{fun {x y} {+ x y}} 1}"), "wrong arity")
    test(run("{access {record {x 1} {y 2}} x}"), "1")
    testExc(run("{access {record {x 1} {y 2}} z}"), "no such field")
    testExc(run("{record {x {access {record {y 1}} z}}}"), "no such field")
    test(run("42"), "42")
    test(run("{fun {x} x}"), "function")
    test(run("{record {x 1}}"), "record")

    /* Write your own tests */
    test(run("{with {x {record {a 3}}} {access x a}}"),"3")
    test(run("{{fun {x y z w} {+ {+ x y} {- z w}}} 2 5 10 3}"), "14")
    test(run("{{fun {} {+ 10 13}}}"), "23")
    test(run("{record {z 3}}"), "record")
    test(run("{fun {x} {+ x x}}"), "function")
    testExc(run({"{record {y {access {record {z 3} {a 10} {b 20}} y}}}"}),"no such field")
    test(run({"{record {y {access {record {z 3} {a 10} {b 20}} z}}}"}),"record")
    test(run("{access {record {x 10} {y {+ 3 5}}} y}"), "8")
    test(run("{record {a 10} {b {+ 1 2}}}"), "record")
    test(run("{access {record {a 10} {b {+ 1 2}}} b}"), "3")
    test(run("{with {g {fun {r} {access r c}}} {g {record {a 0} {c 12} {b 7}}}}"), "12")
    test(run("{access {record {r {record {z 0}}}} r}"), "record")
    test(run("{access {access {record {r {record {z 0}}}} r} z}"), "0")
    test(run("{with {f {fun {a b} {+ a b}}} {with {g {fun {x} {- x 5}}} {with {x {f 2 5}} {g x}}}}"), "2")
    test(run("{with {f {fun {x y} {+ x y}}} {f 1 2}}"), "3")
    test(run("{with {f {fun {} 5}} {+ {f} {f}}}"), "10")
    test(run("{with {h {fun {x y z w} {+ x w}}} {h 1 4 5 6}}"), "7")
    test(run("{with {f {fun {} 4}} {with {g {fun {x} {+ x x}}} {with {x 10} {- {+ x {f}} {g 4}}}}}"), "6")
    test(run("{record {a 10} {b {+ 1 2}}}"), "record")
    test(run("{access {record {r {record {z 0}}}} r}"), "record")
    test(run("{access {access {record {r {record {z 0}}}} r} z}"), "0")
    test(run("{with {x 3} {with {y 5} {access {record {a x} {b y}} a}}}"), "3")
    test(run("{with {f {fun {a b} {+ {access a a} b}}} {with {g {fun {x} {+ 5 x}}} {with {x {f {record {a 10} {b 5}} 2}} {g x}}}}"), "17")
    test(run("{with {f {fun {a b c d e} {record {a a} {b b} {c c} {d d} {e e}}}} {access {f 1 2 3 4 5} c}}"), "3")
    test(run("{with {f {fun {a b c} {record {a a} {b b} {c c}}}} {access {f 1 2 3} b}}"), "2")
    test(run("{with {f {fun {a b c} {record {x a} {y b} {z c} {d 2} {e 3}}}} {access {f 1 2 3} y}}"), "2")
    test(run("{with {f {fun {a b c} {record {x a} {y b} {z c} {d 2} {e 3}}}} {access {f 1 2 3} d}}"), "2")
    test(run("{with {f {fun {x} {+ 5 x}}} {f {access {access {record {a {record {a 10} {b {- 5 2}}}} {b {access {record {x 50}} x}}} a} b}}}"), "8")
    test(run("{access {record {a 10} {b {+ 1 2}}} b}"), "3")
    test(run("{access {record {r {record {z 0}}}} r}"), "record")
    test(run("{access {access {record {r {record {z 0}}}} r} z}"), "0")
    test(run("{access {record {a 10}} a}"), "10")
    test(run("{access {record {a {+ 1 2}}} a}"), "3")
    test(run("{fun {x} x}"), "function")
    test(run("{access {record {a {record {b 10}}}} a}"), "record")
    test(run("{access {access {record {a {record {a 10}}}} a} a}"), "10")
    test(run("{access {access {record {a {record {a 10} {b 20}}}} a} a}"), "10")
    test(run("{access {access {record {a {record {a 10} {b 20}}}} a} b}"), "20")
    test(run("{+ {access {record {a 10}} a} {access {record {a 20}} a}}"), "30")
    test(run("{record {a {- 2 1}}}"), "record")
    test(run("{access {record {a 10}} a}"), "10")
    test(run("{access {record {a {- 2 1}}} a}"), "1")
    test(run("{access {record {a {record {b 10}}}} a}"), "record")
    test(run("{access {access {record {a {record {a 10}}}} a} a}"), "10")
    test(run("{access {access {record {a {record {a 10} {b 20}}}} a} a}"), "10")
    test(run("{access {access {record {a {record {a 10} {b 20}}}} a} b}"), "20")
    test(run("{access {record {r {record {z 0}}}} r}"), "record")
    test(run("{access {access {record {r {record {z 0}}}} r} z}"), "0")
    test(run("{with {y {record {x 1} {y 2} {z 3}}} {access y y}}"), "2")
    test(run("{with {y {record {x 1} {y 2} {z 3}}} {access y z}}"), "3")
    test(run("{record {a 10} {b {+ 1 2}}}"), "record")
    test(run("{access {record {a 10} {b {+ 1 2}}} b}"), "3")
    test(run("{with {g {fun {r} {access r c}}} {g {record {a 0} {c 12} {b 7}}}}"), "12")
    test(run("{access {record {r {record {z 0}}}} r}"), "record")
    test(run("{access {access {record {r {record {z 0}}}} r} z}"), "0")
    testExc(run("{access {record {b 10} {b {+ 1 2}}} b}"), "duplicate fields")
    testExc(run("{access {record {a 10}} b}"), "no such field")
    testExc(run("{record {z {access {record {z 0}} y}}}"), "no such field")

  }
}
