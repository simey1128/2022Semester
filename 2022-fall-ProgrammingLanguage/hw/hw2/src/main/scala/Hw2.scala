package Hw2

import fastparse._
import MultiLineWhitespace._
import scala.collection.immutable.HashMap 

sealed trait Val
case class IntVal(n: Int) extends Val
case class BoolVal(b: Boolean) extends Val
case class ProcVal(v: Var, expr: Expr, env: Env) extends Val
case class RecProcVal(fv: Var, av: Var, body: Expr, expr: Expr, env: Env) extends Val

case class Env(hashmap: HashMap[Var,Val]) {
  def apply(variable: Var): Val = hashmap(variable)
  def exists(v: Var): Boolean = 
    hashmap.exists((a: (Var, Val)) => a._1 == v)
  def add(v: Var, value: Val) = Env(hashmap + (v -> value))
  
}

sealed trait Program
sealed trait Expr extends Program
case class Const(n: Int) extends Expr
case class Var(s: String) extends Expr
case class Add(l: Expr, r: Expr) extends Expr
case class Sub(l: Expr, r: Expr) extends Expr
case class Iszero(c: Expr) extends Expr
case class Ite(c: Expr, t: Expr, f: Expr) extends Expr
case class Let(name: Var, value: Expr, body: Expr) extends Expr
case class Paren(expr: Expr) extends Expr
case class Proc(v: Var, expr: Expr) extends Expr
case class PCall(ftn: Expr, arg: Expr) extends Expr
case class LetRec(fname: Var, aname: Var, fbody: Expr, ibody: Expr) extends Expr

sealed trait IntExpr
case class IntConst(n: Int) extends IntExpr
case object IntVar extends IntExpr
case class IntAdd(l: IntExpr, r: IntExpr) extends IntExpr
case class IntSub(l: IntExpr, r: IntExpr) extends IntExpr
case class IntMul(l: IntExpr, r: IntExpr) extends IntExpr
case class IntSigma(f: IntExpr, t: IntExpr, b: IntExpr) extends IntExpr
case class IntPow(b: IntExpr, e: IntExpr) extends IntExpr



package object Hw2 {

  

}

object IntInterpreter {
  def evalInt(expr: IntExpr, env: Option[Int]): Int = 
  {
    expr match {
      case IntConst(n) => n
      case IntVar => {
        env match {
          case Some(n_n) => n_n
          case _ => throw new Exception("IntVar error")
        }
      }
      case IntAdd(l,r) => evalInt(l, env) + evalInt(r, env)
      case IntSub(l,r) => evalInt(l, env) - evalInt(r, env)
      case IntMul(l,r) => evalInt(l, env) * evalInt(r, env)
      case IntSigma(f,t,b) => {
        val v1 = evalInt(f, env)
        val v2 = evalInt(t, env)
        if(v1 > v2) return 0

        val new_env = Some(v1)
        val v3 = evalInt(b, new_env)
        evalInt(IntSigma(IntConst(v1+1),t,b), env) + v3
      }
      case IntPow(b,e) => {
        val v2 = evalInt(e, env)
        if(v2 == 0) return 1

        val v1 = evalInt(b, env)
        evalInt(IntMul(IntPow(b, IntConst(v2-1)), b), env) 
      }
      
    }
  }
  def apply(s: String): Int = {
    val parsed = IntParser(s)
    evalInt(parsed, None)
  }
}

object LetRecInterpreter {
  
  def eval(env: Env, expr: Expr): Val = {
    expr match {
      case Const(n) => IntVal(n)
      case Var(s) => {
        if(env.exists(Var(s))) env.apply(Var(s))
        else throw new Exception("Var error")
      }
      case Add(l, r) => (eval(env, l), eval(env, r)) match {
        case (v_l: IntVal, v_r: IntVal) => IntVal(v_l.n+v_r.n)
        case _ => throw new Exception("Type Error") 
      }
      case Sub(l, r) => (eval(env, l), eval(env, r)) match {
        case (v_l: IntVal, v_r: IntVal) => IntVal(v_l.n - v_r.n)
        case _ => throw new Exception("Type Error")
      }
      case Iszero(c) => eval(env, c) match {
        case (v_c: IntVal) => {
          if(v_c.n==0) BoolVal(true)
          else BoolVal(false)
        }
        case _ => throw new Exception("Type Error")
      }
      case Ite(c, t, f) => eval(env, c) match {
        case (v_l: BoolVal) => {
          if(v_l.b) eval(env, t)
          else eval(env, f)
        }
        case _ => throw new Exception("Type Error")
      }
      case Let(name, value, body) => eval(env.add(name, eval(env, value)), body)
      case Paren(expr) => eval(env, expr)
      case Proc(v, expr) => ProcVal(v, expr, env)
      case PCall(ftn, arg) => {
        val v1 = eval(env, ftn)
        val v2 = eval(env, arg)
        v1 match {
          case ProcVal(v, expr, env) => eval(env.add(v,v2), expr)
          case RecProcVal(fv, av, body, expr, env) => eval(env.add(av, v2).add(fv, v1), body)
          case _ => throw new Exception("Type Error")
        }
      }
      case LetRec(fname, aname, fbody, ibody) => eval(env.add(fname, RecProcVal(fname, aname, fbody, ibody, env)), ibody)

      
    }
  }
  
  
  def apply(program: String): Val = {
    val parsed = LetRecParserDriver(program)
    eval(Env(new HashMap[Var,Val]()), parsed)
  }

}

object LetRecToString {
  def apply(expr: Expr): String = {
    expr match {
      case Const(n) => ""+n
      case Var(s) => s
      case Add(l,r) => apply(l) + " + " + apply(r)
      case Sub(l,r) => apply(l) + " - " + apply(r)
      case Iszero(c) => "iszero "+apply(c)
      case Ite(c,t,f) => "if " + apply(c) + " then " + apply(t) + " else " + apply(f)
      case Paren(expr) => "(" + apply(expr) + ")"
      case Let(name, value, body) => "let " + apply(name) + " = " + apply(value) + " in " + apply(body)
      case Proc(v, expr) => "proc " + apply(v) + " " + apply(expr)
      case PCall(ftn, arg) => apply(ftn) + " " + apply(arg)
      case LetRec(fname, aname, fbody, ibody) => "letrec " + apply(fname) + "(" + apply(aname) + ") = " + apply(fbody) + " in " + apply(ibody)
    }
  }
}

object Hw2App extends App {
  
  println("Hello from Hw2!")

  val int_prog = """x + 1"""
  val parsed = IntParser(int_prog)
  println(parsed)


}