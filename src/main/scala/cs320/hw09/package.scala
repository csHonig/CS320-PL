package cs320

import java.security.KeyStore.TrustedCertificateEntry

package object hw09 extends Homework09 {
  def mustSame(l:Type,r:Type):Type ={
    if(same(l,r)) l
    else notype(s"$l is not equal to $r")
  }
  def mustSame(l:List[Type], r:List[Type]):List[Type] = {
    if(same(l,r)) l
    else notype(s"$l is not equal to $r")
  }
  def same(l:List[Type], r:List[Type]):Boolean = {
    if(l.size != r.size) false
    else{
      (l,r) match{
        case (Nil,Nil) => true
        case (hl::tl, hr::tr) => same(hl,hr) && same(tl,tr)
      }
    }
  }
  def same(l:Type, r:Type): Boolean = (l,r) match{
    case(NumT, NumT) => true
    case(BoolT, BoolT) =>true
    case(ArrowT(p1, r1), ArrowT(p2,r2)) => {
      if(p1.size !=p2.size) false
      else {
        if (same(r1, r2)) {
          (p1, p2) match {
            case (Nil, Nil) => true
            case (h1 :: t1, h2 :: t2) => same(h1, h2) && same(ArrowT(t1, r1), ArrowT(t2, r2))
          }
        }
        else false
      }
    }
    case (IdT(name), IdT(name2)) => true
    case _ => false
  }
  def notype(msg: Any): Nothing = error(s"no type: $msg")



  def validity(p:Type, tyEnv:TyEnv):Boolean = validType(p,tyEnv).isInstanceOf[Type]

  //Check whether it is a valid type
  def validType(ty: Type, tyEnv: TyEnv): Type = ty match {
    case NumT => ty
    case BoolT =>ty
    case ArrowT(p, r) => {
      ArrowT(listValid(p,tyEnv),validType(r,tyEnv))
    }
    case IdT(x) => {
      if (tyEnv.tbinds.contains(x)) ty
      else notype(s"$x is a free type")
    }
  }

  def listValid(ty:List[Type], tyEnv:TyEnv):List[Type] = {
    if(ty.forall(y => validity(y,tyEnv))) ty
    else error(s"not valid type")
  }


  def makeStringList(p:List[(String, Type)],x: List[String]): List[String] = p match {
    case Nil=> x
    case h::t => makeStringList(t,x:+ h._1)
  }
  def makeTypeList(p:List[(String, Type)],x: List[Type]): List[Type] = p match {
    case Nil=> x
    case h::t => makeTypeList(t,x:+ h._2)
  }
  def argsCheckType(args:List[Expr],tyEnv:TyEnv, l: List[Type]) : List[Type] =args match{
    case Nil => l
    case h::t => {
      val temp = typeCheck(h,tyEnv)
      argsCheckType(t,tyEnv, l :+ temp)
    }
  }


  def typeCheck(e: Expr, tyEnv: TyEnv): Type = e match{
    case Num(_) => NumT
    case Bool(_) => BoolT
    case Add(left,right)=> {
      mustSame(typeCheck(left,tyEnv),NumT)
      mustSame(typeCheck(right,tyEnv),NumT)
      NumT
    }
    case Sub(left,right) =>{
      mustSame(typeCheck(left,tyEnv),NumT)
      mustSame(typeCheck(right,tyEnv),NumT)
      NumT
    }
    case Eq(left,right) =>{
      mustSame(typeCheck(left,tyEnv),NumT)
      mustSame(typeCheck(right,tyEnv),NumT)
      BoolT
    }
    case Lt(left, right) =>{
      mustSame(typeCheck(left,tyEnv),NumT)
      mustSame(typeCheck(right,tyEnv),NumT)
      BoolT
    }
    case Id(name)=> tyEnv.varMap.getOrElse(name,error(s"$name no such field"))
    //params: String,Type 좀 이상할듯
    case Fun(params,body) =>{
      val tList = makeTypeList(params, List())
      val sList = makeStringList(params, List())
      listValid(tList,tyEnv)
      val tempMap = (sList zip tList) toMap
      val newEnv = tyEnv.copy(varMap = tyEnv.varMap ++ tempMap)
      ArrowT(tList, typeCheck(body,newEnv))
    }
    case App(func,args) => {
      val funT = typeCheck(func, tyEnv)
      val argsT = argsCheckType(args,tyEnv,List())
      funT match{
        case ArrowT(p,r) if same(p,argsT) => r
        case _ => notype(s"apply $argsT to $funT")
      }
    }
    case Block(stmts, expr) => stmts match {
      case Nil => typeCheck(expr,tyEnv)
      case h::t => {
        val temp = typeCheck(tyEnv,h)
        typeCheck(Block(t,expr),temp)
      }
    }
    case Assign(name, expr) => {
      val temp = tyEnv.varMap.getOrElse(name, error(s"$name is not in TyEnv"))
      if (tyEnv.mutables.contains(name)==false) error(s"$name is not a variable")
      mustSame(typeCheck(expr,tyEnv.addVar(name,temp,mutable = true)),temp)
    }
    case Match(expr,cases) => {
      //Cases :Map[STring, (List[STring],Expr)
      val temp = typeCheck(expr,tyEnv)
      temp match{
        case IdT(x)=> {
          val tempMap = tyEnv.tbinds.getOrElse(x,error(s"error in finding")) //String ->List[Type]
          val keyCases = cases.keys.toList //x1, x2,...,xn
          val v = cases.getOrElse(keyCases.head,error(s"No such name"))._2
          val newEnv = tyEnv.copy(varMap = tyEnv.varMap ++ makeMapCases(keyCases.head,cases,tempMap))
          val t = typeCheck(v, newEnv)
          checkCases(keyCases,tyEnv, t,cases, tempMap)
        }
        case _=> notype(s"$expr is a free type")
      }
    }
    case IfThenElse(cond, thenE, elseE) => {
      mustSame(typeCheck(cond,tyEnv), BoolT)
      mustSame(typeCheck(thenE, tyEnv), typeCheck(elseE, tyEnv))
    }
  }

  def checkCases(keyCases:List[String], tyEnv: TyEnv, t:Type, cases: Map[String,(List[String],Expr)], envMap:Map[String,List[Type]]): Type = keyCases match{
    case Nil => t
    case h::tail => {
      val v = cases.getOrElse(h,error(s"No such name"))._2
      val newEnv = tyEnv.copy(varMap = tyEnv.varMap ++ makeMapCases(h,cases,envMap))
      mustSame(t,typeCheck(v, newEnv))
      checkCases(tail,tyEnv,t,cases,envMap)
    }
  }
  def makeMapCases(x:String, cases: Map[String,(List[String],Expr)], envMap:Map[String,List[Type]]): Map[String,Type] = {
    val (strList, expr) = cases.getOrElse(x,error(s"Not in the cases"))
    val tList = envMap.getOrElse(x, error(s"Not in the envMap"))
    (strList zip tList) toMap
  }




  def typeCheck(tyEnv: TyEnv, stmt: Stmt): TyEnv = stmt match {
    case Val(_, name, ty, expr) => {
        validType(ty,tyEnv)
        mustSame(typeCheck(expr, tyEnv),ty)
        tyEnv.addVar(name,ty,false)
    }
    case Var(name, ty,expr) => {
      validType(ty,tyEnv)
      mustSame(typeCheck(expr,tyEnv),ty)
      tyEnv.addVar(name,ty,true)
    }
    case Def(name, params, retTy, body) => {
      val tList = makeTypeList(params,List())
      listValid(tList,tyEnv)
      val xList = makeStringList(params,List())
      val tyEnvT = tyEnv.addVar(name, ArrowT(tList,retTy),false)
      val tempMap = (xList zip tList) toMap
      val tempEnv = tyEnvT.copy(varMap = tyEnvT.varMap ++ tempMap)
      mustSame(typeCheck(body,tempEnv),retTy)
      tyEnvT
    }
    case Trait(name, cases) =>{
      //  case class Trait(name: String, cases: Map[String, List[Type]]) extends Stmt
      val tyEnvT = tyEnv.addTBind(name,cases) //IDt(name)
      val strList = cases.keys.toList
      val tList = cases.values.toList
      makeEnv(strList,tList, tyEnvT, IdT(name))
    }
  }
  def makeEnv(strList:List[String], tList:List[List[Type]] ,tyEnv:TyEnv, t:Type):TyEnv =   (strList,tList) match{
      case (Nil,Nil) => tyEnv
      case (h1::t1,h2::t2) =>{
        val tyEnvV = tyEnv.addVar(h1,ArrowT(h2,t),false)
        listValid(h2,tyEnvV)
        makeEnv(t1,t2,tyEnvV,t)
      }
  }
  def helpApp(args:List[Expr], params:List[String],env:Env, sto:Sto, vList: List[Value]):(Map[String,Value],Sto) = args match{
    case Nil =>{
      if(params.size==vList.size){
        val tempMap = ((params zip vList)toMap)
        (tempMap,sto)
      }
      else error(s"Check Type Checker, it shouldn't be valid")
    }
    case h::t=>{
      val (v,fsto) = interp(h,env,sto)
      helpApp(t,params,env,fsto,vList:+v)
    }
  }
  def interp(e: Expr, env: Env, sto: Sto): (Value, Sto) = e match{

    case Num(n) => (NumV(n),sto)
    case Bool(b) => (BoolV(b),sto)
    case Add(l,r) =>{
      val (lv,lsto) = interp(l,env,sto)
      val (rv,rsto) = interp(r,env,lsto)
      (numVAdd(lv,rv), rsto)
    }
    case Sub(l,r) =>{
      val (lv,lsto) = interp(l,env,sto)
      val (rv,rsto) = interp(r,env,lsto)
      (numVSub(lv,rv), rsto)
    }
    case Eq(l,r)=> {
      val (lv,lsto) = interp(l,env,sto)
      val (rv,rsto) = interp(r,env,lsto)
      (numVEq(lv,rv),rsto)
    }
    case Lt(l,r)=>{
      val (lv,lsto) = interp(l,env,sto)
      val (rv,rsto) = interp(r,env,lsto)
      (numVLt(lv,rv),rsto)
    }
    case Id(name)=> {
      val temp = env.getOrElse(name,error(s"$name is not in env"))
      temp match{
        case AddrV(addr) => {
          val tempVal = sto.getOrElse(addr,error(s"$addr is not in sto"))
          tempVal match{
            case ExprV(expr,kenv) => {
              val (v,ksto) = interp(expr,kenv,sto)
              (v,ksto+ (addr->v))
            }
            case _ => (tempVal,sto)
          }
        }
        case _=> (temp,sto)
      }
    }
    case Fun(params,body) => {
      val sList = makeStringList(params,List())
      (CloV(sList,body,env),sto)
    }
    case App(func,args) => {
      val (first,stof) = interp(func,env,sto)
      first match{
        case CloV(params,body,fenv) => {
          val (tempMap, finalSto) = helpApp(args,params,env,stof,List())
          interp(body,fenv++tempMap,finalSto)
        }
        case ConstructorV(name) => makeVariant(name,env,stof,args,List())
        case _ => error(s"$first cannot be used as app func")
      }
    }
    case Block(stms,expr) => stms match{
      case Nil => interp(expr,env,sto)
      case h::t => {
        val (fenv,fsto) = interp((env,sto),h)
        interp(Block(t,expr),fenv,fsto)
      }
    }
    case Assign(name,expr) => {
      val addrValue = env.getOrElse(name,error(s"$name is not a var"))
      addrValue match{
        case AddrV(addr) => {
          val (v,fsto) = interp(expr,env,sto)
          (v,fsto+(addr-> v))
        }
        case _ =>error(s"$addrValue should be a fu**ing addr")
      }
    }
    case Match(expr, cases) =>{
      val (tempVal, fsto) = interp(expr,env,sto)
      tempVal match {
        case VariantV(name,values) => {
          val (strList, e) = cases.getOrElse(name, error(s"$name is not in the field"))
          val newMap = ((strList zip values) toMap)
          interp(e,env++newMap,fsto)
        }
        case _ => error(s"$tempVal should be a variant")
      }
    }
    case IfThenElse(cond, thenE, elseE) => {
      val (v,csto) = interp(cond,env,sto)
      v match{
        case BoolV(x) =>{
          x match{
            case true => interp(thenE,env,csto)
            case false => interp(elseE,env,csto)
          }
        }
        case _ => error(s"Check the TypeChecker again, should be BoolV but it is $v")
      }
    }


  }


  def makeVariant(x: String, env:Env,sto:Sto, args:List[Expr], vList:List[Value]) : (Value,Sto) = args match {
    case Nil => (VariantV(x,vList),sto)
    case h::t =>{
      val (v, fsto) = interp(h,env,sto)
      makeVariant(x,env,fsto,t,vList :+v)
    }
  }
  def numVAdd(left:Value, right:Value):Value = (left,right) match {
    case (NumV(l), NumV(r)) => NumV(l + r)
    case v => error(s"The Values are not NumV")
  }

  def numVSub(left:Value, right:Value):Value = (left,right) match {
    case (NumV(l), NumV(r)) => NumV(l - r)
    case _ => error(s"The Values are not NumV. Check the Type checker")
  }
  def numVEq(left:Value, right:Value):Value = (left,right) match {
    case (NumV(l), NumV(r)) => {
      if (l==r) BoolV(true)
      else BoolV(false)
    }
    case _ => error(s"The Values are not NumV. Check the Type checker")
  }
  def numVLt(left:Value, right:Value):Value = (left,right) match {
    case (NumV(l), NumV(r)) => {
      if (l<r) BoolV(true)
      else BoolV(false)
    }
    case _ => error(s"The Values are not NumV. Check the Type checker")
  }

  def mallocSto(sto:Sto):Addr = maxAddressSto(sto)+1
  def maxAddressSto(sto: Sto):Addr = sto.keySet.+(0).max
  def interp(pair: (Env, Sto), stmt: Stmt): (Env, Sto) = stmt match{
    case Val(isLazy,name, _, expr) => {
      val (env,sto) = pair
      isLazy match{
        case false=> {
          val (v, ksto) = interp(expr,env,sto)
          (env+(name->v),ksto)
        }
        case _ =>{
          val a =mallocSto(sto)
          (env + (name->AddrV(a)), sto+(a->ExprV(expr,env)))
        }
      }
    }
    case Var(name,_,expr) => {
      val (env,sto) = pair
      val (v,ksto) = interp(expr,env,sto)
      val a = mallocSto(ksto)
      (env+(name->AddrV(a)),ksto+(a->v))
    }

    case Def(name,params,_,body)=> {
      val strList = makeStringList(params,List())
      val (env,sto) = pair
      val cloV = CloV(strList,body,env)
      cloV.env = env+ (name->cloV)
      (cloV.env,sto)
    }

    case Trait(name,cases) =>{
      val (env,sto) = pair
      val keyList = cases.keys.toList
      val conList = mkConsKList(keyList,List())
      val tempMap = ((keyList zip conList) toMap)
      (env++tempMap,sto)
    }
  }
  def mkConsKList(keyList:List[String], ans:List[ConstructorV]):List[ConstructorV] = keyList match{
    case Nil => ans
    case h::t=> mkConsKList(t,ans:+ ConstructorV(h))
  }


  def tests: Unit = {
    test(run("""
      var x: Int = 1
      val y: Int = (x = 3)
      x + y
    """), "6")
    test(run("""
      var x: Int = 1
      lazy val y: Int = (x = 3)
      x + y + x
    """), "7")
    test(run("""
      var x: Int = 0
      lazy val y: Int = (x = x + 1)
      val z: Int = y + y + y + y
      z"""), "4")
    testExc(run("""val x: Int = 42; x = 24"""), "")
    test(run("""
      trait AE
      case class Num(Int)
      case class Add(AE, AE)
      case class Sub(AE, AE)

      def interp(e: AE): Int = e match {
        case Num(n) => n
        case Add(l, r) => interp(l) + interp(r)
        case Sub(l, r) => interp(l) - interp(r)
      }

      interp(Add(Num(2), Sub(Num(3), Num(1))))
    """), "4")
    test(run("""
      trait Tree
      case class Leaf(Int)
      case class Node(Tree, Tree)

      def max(l: Int, r: Int): Int =
        if (l < r) r else l

      def depth(e: Tree): Int = e match {
        case Leaf(n) => 1
        case Node(l, r) => max(depth(l), depth(r)) + 1
      }

      depth(Node(Node(Leaf(1), Node(Leaf(2), Leaf(3))), Leaf(4)))
    """), "4")
  }

}
