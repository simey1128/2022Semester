package hw3

import scala.collection.immutable.HashMap 
import hw3._


package object hw3 {
  type Env = HashMap[Var,Val]
  type Loc = Int
  
}

case class Mem(m: HashMap[Loc,Val], top: Loc) {

}

sealed trait Val
case class IntVal(n: Int) extends Val
case class BoolVal(b: Boolean) extends Val
case class ProcVal(v: Var, expr: Expr, env: Env) extends Val
case class RecProcVal(fv: Var, av: Var, body: Expr, env: Env) extends Val
case class LocVal(l: Loc) extends Val


sealed trait Program
sealed trait Expr extends Program
case class Const(n: Int) extends Expr
case class Var(s: String) extends Expr
case class Add(l: Expr, r: Expr) extends Expr
case class Sub(l: Expr, r: Expr) extends Expr
case class Mul(l: Expr, r: Expr) extends Expr
case class Div(l: Expr, r: Expr) extends Expr
case class GTExpr(l: Expr, r: Expr) extends Expr
case class GEQExpr(l: Expr, r: Expr) extends Expr
case class Iszero(c: Expr) extends Expr
case class Ite(c: Expr, t: Expr, f: Expr) extends Expr
case class ValExpr(name: Var, value: Expr, body: Expr) extends Expr
case class VarExpr(name: Var, value: Expr, body: Expr) extends Expr
case class Proc(v: Var, expr: Expr) extends Expr
case class DefExpr(fname: Var, aname: Var, fbody: Expr, ibody: Expr) extends Expr
case class Asn(v: Var, e: Expr) extends Expr
case class Paren(expr: Expr) extends Expr
case class Block(f: Expr, s: Expr) extends Expr
case class PCall(ftn: Expr, arg: Expr) extends Expr







object MiniScalaInterpreter {

  case class Result(v: Val, m: Mem)
  case class UndefinedSemantics(msg: String = "", cause: Throwable = None.orNull) extends Exception("Undefined Semantics: " ++ msg, cause)
  
  def doInterpret(env: Env, mem: Mem, expr: Expr): Result = {
    expr match {
      case Const(n) => Result(IntVal(n), mem)
      case Var(s) => {
        if(env.isEmpty) throw UndefinedSemantics("at Var, env is empty")

        val temp = env(Var(s))
        temp match {
          case LocVal(l: Loc) => {
            if(mem.top == 0) throw UndefinedSemantics("at Var, mem is empty")
            val ret = mem.m(l)
            if(ret == None) throw UndefinedSemantics("at Var, LocVal(l) doesn't exist")
            Result(mem.m(l), mem)
          }
          case IntVal(n: Int) => Result(temp, mem)
          case BoolVal(b: Boolean) => Result(temp, mem)
          case ProcVal(v: Var, expr: Expr, env: Env) => Result(temp, mem)
          case RecProcVal(fv: Var, av: Var, body: Expr, env: Env) => Result(temp, mem)
          case _ => throw UndefinedSemantics("at Var")
        }
      }
      case Add(l, r) => doInterpret(env, mem, l) match {
        case Result(l_v: IntVal, l_mem: Mem) => doInterpret(env, l_mem, r) match {
            case Result(r_v: IntVal, r_mem: Mem) => Result(IntVal(l_v.n+r_v.n), r_mem)
            case _ => throw UndefinedSemantics("at Add, right expression")
        }
        case _ => throw UndefinedSemantics("at Add, left expression")
      }
      case Sub(l, r) => doInterpret(env, mem, l) match {
        case Result(l_v: IntVal, l_mem: Mem) => doInterpret(env, l_mem, r) match {
            case Result(r_v: IntVal, r_mem: Mem) => Result(IntVal(l_v.n-r_v.n), r_mem)
            case _ => throw UndefinedSemantics("at Sub, right expression")
        }
        case _ => throw UndefinedSemantics("at Sub, left expression")
      }
      case Mul(l, r) => doInterpret(env, mem, l) match {
        case Result(l_v: IntVal, l_mem: Mem) => doInterpret(env, l_mem, r) match {
            case Result(r_v: IntVal, r_mem: Mem) => Result(IntVal(l_v.n*r_v.n), r_mem)
            case _ => throw UndefinedSemantics("at Mul, right expression")
        }
        case _ => throw UndefinedSemantics("at Mul, left expression")
      }
      case Div(l, r) => doInterpret(env, mem, l) match {
        case Result(l_v: IntVal, l_mem: Mem) => doInterpret(env, l_mem, r) match {
            case Result(r_v: IntVal, r_mem: Mem) => Result(IntVal(l_v.n/r_v.n), r_mem)
            case _ => throw UndefinedSemantics("at Div, right expression")
        }
        case _ => throw UndefinedSemantics("at Div, left expression")
      }
      case GTExpr(l, r) => doInterpret(env, mem, l) match {
        case Result(l_v: IntVal, l_mem: Mem) => doInterpret(env, l_mem, r) match {
            case Result(r_v: IntVal, r_mem: Mem) => {
              if(l_v.n > r_v.n) Result(BoolVal(true), r_mem)
              else Result(BoolVal(false), r_mem)
            }
            case _ => throw UndefinedSemantics("at GTExpr, right expression")
        }
        case _ => throw UndefinedSemantics("at GTExpr, left expression")
      }
      case GEQExpr(l, r) => doInterpret(env, mem, l) match {
        case Result(l_v: IntVal, l_mem: Mem) => doInterpret(env, l_mem, r) match {
            case Result(r_v: IntVal, r_mem: Mem) => {
              if(l_v.n >= r_v.n) Result(BoolVal(true), r_mem)
              else Result(BoolVal(false), r_mem)
            }
            case _ => throw UndefinedSemantics("at GEQExpr, right expression")
        }
        case _ => throw UndefinedSemantics("at GEQExpr, left expression")
      }
      case Iszero(c) => doInterpret(env, mem, c) match {
        case Result(c_v: IntVal, c_mem: Mem) => if(c_v.n == 0) Result(BoolVal(true), c_mem) else Result(BoolVal(false), c_mem)
        case _ => throw UndefinedSemantics("at Iszero")
      }
      case Ite(c,t,f) => doInterpret(env, mem, c) match {
        case Result(c_v: BoolVal, c_mem: Mem) => {
          if(c_v.b) doInterpret(env, c_mem, t)
          else doInterpret(env, c_mem, f)
        } 
        case _ => throw UndefinedSemantics("at Ite")
      }
      case ValExpr(name, value, body) => doInterpret(env, mem, value) match {
        case Result(value_v: IntVal, value_mem: Mem) => doInterpret(env + (name -> IntVal(value_v.n)), value_mem, body)
        case _ => throw UndefinedSemantics("at ValExpr")
      }
      case VarExpr(name, value, body) => doInterpret(env, mem, value) match {
        case Result(value_v: IntVal, value_mem: Mem) => {
          val new_loc = value_mem.top + 1
          val new_env = env + (name -> LocVal(new_loc))
          val new_mem = value_mem.m + (new_loc -> value_v)
          doInterpret(new_env, Mem(new_mem, new_loc), body)
        }
        case _ => throw UndefinedSemantics("at VarExpr")
      } 
      case Proc(v: Var, expr: Expr) => Result(ProcVal(v, expr, env), mem)
      case DefExpr(fname: Var, aname: Var, fbody: Expr, ibody: Expr) => {
        val new_env = env + (fname -> RecProcVal(fname, aname, fbody, env))
        doInterpret(new_env, mem, ibody)
      }
      case Asn(v, e) => doInterpret(env, mem, e) match {
        case Result(e_v: IntVal, e_mem: Mem) => {
          if(env.isEmpty) throw UndefinedSemantics("at Asn, env is empty")
          if(e_mem.top == 0) throw UndefinedSemantics("at Asn, mem is empty")
          env(v) match {
            case LocVal(l: Loc) => {
              val new_loc = l
              val new_mem = mem.m + (new_loc -> e_v)
              Result(e_v, Mem(new_mem, new_loc))
            }
            case _ => throw UndefinedSemantics("at Asn, variable doesn't exist")
          }
        }
        case _ => throw UndefinedSemantics("at Asn")
      }
      case Paren(expr: Expr) => doInterpret(env, mem, expr)
      case Block(f, s) => doInterpret(env, mem, f) match {
        case Result(f_v: IntVal, f_mem: Mem) => doInterpret(env, f_mem, s) match {
          case Result(s_v: IntVal, s_mem: Mem) => Result(s_v, s_mem)
          case _ => throw UndefinedSemantics("at Block, second expression")
        }
        case _ => throw UndefinedSemantics("at Block, first expression")
      }
      case PCall(ftn: Expr, arg: Expr) => doInterpret(env, mem, ftn) match {
        case Result(proc: ProcVal, proc_mem: Mem) => {
          val Result(arg_v: IntVal, arg_mem: Mem) = doInterpret(env, proc_mem, arg)
          val new_env = proc.env + (proc.v -> arg_v)
          doInterpret(new_env, arg_mem, proc.expr)
        }
        case Result(rproc: RecProcVal, rproc_mem: Mem) => {
          val Result(arg_v: IntVal, arg_mem: Mem) = doInterpret(env, rproc_mem, arg)
          val new_env = rproc.env + (rproc.av -> arg_v) + (rproc.fv -> rproc)
          doInterpret(new_env, arg_mem, rproc.body)
        }
        case _ => throw UndefinedSemantics("at Pcall, no procedure call")
      }


      
      
    }
  }
  
  def apply(program: String): Val = {
    val parsed = MiniScalaParserDriver(program)
    println(s"\n program is ${program}\n")
    println(s"parsed is ${parsed}")
    doInterpret(new Env(), Mem(new HashMap[Loc,Val],0), parsed).v
    
  }

}


object Hw3App extends App {
  
  println("Hello from Hw3!")

}