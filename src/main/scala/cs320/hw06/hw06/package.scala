package cs320

package object hw06 extends Homework06 {
  trait SRBFAEValue
  case class NumV(n: Int) extends SRBFAEValue
  case class CloV(param: String, body: SRBFAE, env:Env) extends SRBFAEValue
  case class BoxV(addr: Addr) extends SRBFAEValue
  case class RecV(reco:Addr) extends SRBFAEValue

  type Addr = Int
  type Env = Map[String,SRBFAEValue]
  type Sto = Map[Addr, SRBFAEValue]
  type Rec = Map[Addr,Map[String,SRBFAEValue]]

  def interp(srbfae:SRBFAE, env:Env, sto:Sto,rec:Rec):(SRBFAEValue, Sto,Rec) = srbfae match{
    case Num(n) => (NumV(n),sto,rec)
    case Add(l,r) => {
      val (lv,ls,lr) = interp(l,env,sto,rec)
      val (rv, rs,rr) =interp(r,env,ls,lr)
      (numVAdd(lv,rv),rs,rr)
    }
    case Sub(l,r) => {
      val (lv,ls,lr) = interp(l,env,sto,rec)
      val (rv, rs,rr) =interp(r,env,ls,lr)
      (numVSub(lv,rv),rs,rr)
    }
    case Id(x)=> (lookup(x,env),sto,rec)
    case Fun(x,b) => (CloV(x,b,env),sto,rec)
    case App(f,a) =>{
      val (fv,fs,fr) = interp(f,env,sto,rec)
      val (av,as,ar) = interp(a,env, fs,fr)
      fv match {
        case CloV(x, b, fenv) =>{
          interp(b,fenv+(x->av), as,ar)
        }
        case _=> error(s"It is not a function")
      }
    }
    case NewBox(expr) =>{
      val (ev,es,er) = interp(expr,env,sto,rec)
      val addr = mallocSto(es)
      (BoxV(addr), es+(addr->ev),er)
    }
    case SetBox(box, expr) => {
      val (bv, bs,br) = interp(box,env,sto,rec)
      bv match{
        case BoxV(addr) => {
          val (ev,es,er)  = interp(expr,env,bs,br)
          (ev, es+(addr->ev),er)
        }
        case _ => error(s"Not a Box. Can't Set")
      }
    }
    case OpenBox(box) => {
      val (bv, bs,br) = interp(box,env,sto,rec)
      bv match{
        case BoxV(addr) => (lookupSto(addr,bs),bs,br)
        case _ => error(s"Not a Box. Can't Set")
      }
    }
    case Seqn(left, right) => {
      val (lv,ls,lr) = interp(left,env,sto,rec)
      right match{
        case Nil=> (lv,ls,lr)
        case h::t => interp(Seqn(h,t),env,ls,lr)
      }
    }
    case Rec(fields) => fields match{
      case Nil => {
        val addr = mallocRec(rec)
        (RecV(addr),sto,rec)
      }
      case (hs,hv)::t => {
        val addr = mallocRec(rec)
        val (v,s,r) = interp(hv,env,sto,rec)


        def mkRec(l: List[(String, SRBFAE)],sto:Sto, rec:Rec):(Map[String,SRBFAEValue],Sto,Rec) = l match {
          case Nil => (Map(), sto, rec)
          case (str, value) :: tail => {
            val (tv, ts, tr) = interp(value, env, sto, rec)
            val (resv, ress, resr) = mkRec(tail, ts, tr)
            (Map(str -> tv) ++ resv, ress, resr)
          }
        }
        val (rv,rs,rr) = mkRec(fields,s,r+(addr->Map(hs->v)))



        //addr은 주어져 있고 우리가 그 안에 있는 MAP만 바꾸고 싶음.
        (RecV(addr),rs,rr+(addr->rv))
      }
    }
    case Get(record, field) => {
      val (rv,rs,rr) = interp(record,env,sto,rec)
      rv match{
        case RecV(addr)=> (lookupRec(addr,field,rr),rs,rr)
        case _=>error(s"Not record")
      }
    }
    case Set(record, field, expr) => {
      val (rv,rs,rr) = interp(record,env,sto,rec)
      rv match{
        case RecV(addr)=> {
          val trash = lookupRec(addr,field,rr)
          val tra = lookupRecordMap(addr,rr)
          val (ev,es,er) = interp(expr,env,rs,rr)
          val temporary = tra+(field->ev)
          (ev,es,er++Map(addr->temporary))//이걸 세트해주려면 아마 newbox던지 필요할듯.
        }
        case _=>error(s"Not record")
      }
    }


  }

  def numVAdd(left:SRBFAEValue, right:SRBFAEValue):SRBFAEValue = (left,right) match {
    case (NumV(l), NumV(r)) => NumV(l + r)
    case v => error(s"The Values are not NumV")
  }

  def numVSub(left:SRBFAEValue, right:SRBFAEValue):SRBFAEValue = (left,right) match{
    case (NumV(l), NumV(r)) => NumV(l-r)
    case v => error(s"The Values are not NumV")
  }
  def lookup(id:String,env:Env):SRBFAEValue = env.getOrElse(id,error(s"No such id in env"))
  def lookupSto(addr:Addr,sto:Sto) = sto.getOrElse(addr, error(s"No such addr in sto"))
  def lookupRec(addr: Addr, field:String,reco: Rec) = {
    val temp = reco.getOrElse(addr,error(s"no such field")) //We get a Map
    temp.getOrElse(field,error(s" no such field"))
  }
  def lookupRecordMap(addr:Addr,reco:Rec) ={
    reco.getOrElse(addr,error(s"No such field"))
  }
  def mallocSto(sto:Sto):Addr = maxAddressSto(sto)+1
  def mallocRec(sto:Rec):Addr = maxAddressRec(sto)+1
  def maxAddressSto(sto: Sto):Addr = sto.keySet.+(0).max
  def maxAddressRec(sto: Rec):Addr = sto.keySet.+(0).max
  def run(str: String): String = {
    val temp = SRBFAE(str)
    val (tv,ts,tr) = interp(temp,Map(),Map(),Map())
    val ans = tv match {
      case NumV(n) => n.toString
      case CloV(param,body,env)=> "function"
      case BoxV(box) => "box"
      case RecV(rec) => "record"
    }
    ans

  }

  def tests: Unit = {
    test(run("""{{fun {b} {seqn {setbox b {+ 2 {openbox b}}}
                          {setbox b {+ 3 {openbox b}}}
                          {setbox b {+ 4 {openbox b}}}
                          {openbox b}}}
                {newbox 1}}"""), "10")
    testExc(run("{get {rec {x 1}} y}"), "no such field")
    test(run("{{fun {r} {seqn {set r x 5} {get r x}}} {rec {x 1}}}"), "5")
    test(run("{{fun {r} {seqn {get r x}}} {rec {x 1}}}"), "1")
    test(run("42"), "42")
    test(run("{fun {x} x}"), "function")
    test(run("{newbox 1}"), "box")
    test(run("{rec}"), "record")
    test(run("{get {rec{x 1} {y 2}} x}"), "1")
    test(run("{get {rec{x 1} {y 2} {y 3}} y}"), "3")
    testExc(run("{get {set {rec {x 1} {y 2}} y 3} x}"), "Not record")
    test(run("{{fun {r} {seqn {set r x 5} {set r x 3} {set r x 4} {set r y {+ 100 20}} {get r y}}} {rec {x 1} {y 3}}}"), "120")
    testExc(run("{rec {z {get {rec {z 0}} y}}}"),"no such field")
    testExc(run("{{fun {r} {seqn {set r y 5} {get r y}}} {rec {x 1}}}"), "no such field")
    test(run("{get {rec {x 1} {x 2}} x}"),"2")

    /* Write your own tests */
  }
}
