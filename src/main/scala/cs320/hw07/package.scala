package cs320

package object hw07 extends Homework07 {
  trait KXCFAEval
  case class NumV(n: Int) extends KXCFAEval
  case class CloV(params:List[String], body:KXCFAE, env:Env) extends KXCFAEval
  case class ContV(cont:Cont) extends KXCFAEval
  case object ThrowV extends KXCFAEval
  type Env = Map[String,KXCFAEval]
  type Cont = KXCFAEval=> KXCFAEval
  //세가지 중 하나인데, NUmV, CLoV, ContV, THroWV
  def interp(kxcfae:KXCFAE, env:Env, k:Cont): KXCFAEval = kxcfae match{
    case Num(n) => k(NumV(n))
    case Add(l,r) => interp(l,env, lv=>
      lv match{
        case ThrowV=> k(ThrowV)
        case _=> interp(r,env,rv=> rv match {
          case ThrowV => k(ThrowV)
          case _ => k(numVAdd(lv, rv))
        })
      })
    case Sub(l,r) => interp(l,env, lv=>
      lv match{
        case ThrowV=> k(ThrowV)
        case _=> interp(r,env,rv=> rv match {
          case ThrowV => k(ThrowV)
          case _ => k(numVSub(lv, rv))
        })
      })
    case Id(x) => k(env.getOrElse(x,error(s"no such field")))
    case Fun(x,b) => k(CloV(x,b,env))
    case If0(cond,zero,no) => interp(cond,env, iv=> iv match{
      case NumV(0)=> interp(zero,env,k)
      case ThrowV => k(ThrowV)
      case _ =>interp(no,env,k)
    })
    case Withcc(name,body) =>interp(body, env + (name->ContV(k)),k)
    case App(f, a) =>
      interp(f, env, fv => fv match{
        case CloV(params,body,fenv)=> {
          if (a.length!=params.length) error(s"wrong arity")
          aInterp(a,params,body,env,fenv,k)
        }
        case ContV(fv) => interp(a.head,env,rv=> rv match{
          case ThrowV=> k(rv)
          case _=> fv(rv)
        })
        case ThrowV => k(ThrowV)
        case v=> error(s"Can't call the app $v")
      })
    case Try(t,c) => interp(t,env,tv=> {
      if (tv==ThrowV) interp(c,env,k)
      else k(tv)
    })
    case Throw=> k(ThrowV)


  }
  def aInterp(a:List[KXCFAE], params:List[String], body:KXCFAE, env:Env, fenv:Env, k:Cont):KXCFAEval=(a,params) match{
    case (Nil,Nil) =>interp(body,fenv,k)
    case (ah::at, ph::pt) =>{
      interp(ah,env, av=>av match {
        case ThrowV => k(ThrowV)
        case _ => aInterp(at,pt,body,env,fenv+(ph->av),k)
      })

    }
  }


  def numVAdd(l:KXCFAEval, r:KXCFAEval):KXCFAEval = (l,r) match{
    case (NumV(l), NumV(r)) => NumV(l+r)

    case _ => error(s"One of them isn't Num")
  }
  def numVSub(l:KXCFAEval, r:KXCFAEval):KXCFAEval = (l,r) match {
    case (NumV(l), NumV(r)) => NumV(l - r)

    case _ => error(s"One of them isn't Num")
  }




  def run(str: String): String = {
    val temp = KXCFAE(str)
    val v = interp(temp,Map(),x=>x)
    val ans = v match {
      case NumV(n) => n.toString
      case CloV(param,body,env)=> "function"
      case ContV(kv) => "continuation"
      case ThrowV => error(s"no enclosing try-catch")
    }
    ans

  }

  def tests: Unit = {
    test(run("{{fun {x y} {- y x}} 10 12}"), "2")
    test(run("{fun {} 12}"), "function")
    testExc(run("{{fun {x y} 1} 2}"), "wrong arity")
    test(run("{withcc esc {{fun {x y} x} 1 {esc 3}}}"), "3")
    test(run("{withcc esc {{fun {x y} x} {esc 3} 1 }}"), "3")
    test(run("{withcc esc {{fun {x y} x} {esc 4} {esc 3}}}"), "4")

    test(run("{try {throw} catch 2}"), "2")
    test(run("{try {+ 1 {throw}} catch 2}"), "2")
    test(run("{try {{fun{x y} {y-x}} 2 {throw}} catch 2}"), "2")
    test(run("{try {if0 {- 54 54} {throw} 43} catch 54}"), "54")
    test(run("{{fun {f x} {f f x}} {fun {g y} {if0 {- y 1} {throw} {try {+ y {g g {- y 1}}} catch y}}} 10}"), "54")//보기
    test(run("{{fun {f} {try {f} catch 1}} {fun {} {{fun{x y} {y-x}} 2 {throw}}}}"), "1")
    test(run("{{fun {f} {try {f} catch 1}} {fun {} {+ 1 {throw}}}}"), "1")
    test(run("{{fun {f} {try {f} catch 1}} {fun {} {+ 1 {throw}}}}"), "1")
    test(run("{withcc x {try {+ {throw} {x 2}} catch {x 3}}}"),"3")
    test(run("{withcc x  {x 3}}"),"3")
    testExc(run("{throw}"), "no enclosing try-catch")
    testExc(run("{if0 {throw} 1 2}"), "no enclosing try-catch")
    test( run("{if0 1 {throw} 2}"), "2")
    test(run("{{fun {x y} x} {withcc esc {esc 1}} 2}"), "1")

    /* Write your own tests */


  }

}
