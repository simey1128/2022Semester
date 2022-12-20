package hw4

import scala.collection.immutable.HashMap 
import hw4._


package object hw4 {
  type Env = HashMap[Var,LocVal]
}

case class Mem(m: HashMap[LocVal,Val], top: Int) {
  def extended(v: Val): (Mem, LocVal) = {
    val new_mem = Mem(m.updated(LocVal(top),v), top+1)
    (new_mem,LocVal(top))
  }
  def updated(l: LocVal, new_val: Val): Option[Mem] = {
    m.get(l) match {
      case Some(v) => Some(Mem(m.updated(l, new_val), top))
      case None => None
    }
  }
  def get(l: LocVal): Option[Val] = m.get(l)
  def getLocs(): List[LocVal] = m.keySet.toList
}



sealed trait Val
case object SkipVal extends Val
case class IntVal(n: Int) extends Val
case class BoolVal(b: Boolean) extends Val
case class ProcVal(args: List[Var], expr: Expr, env: Env) extends Val
case class LocVal(l: Int) extends Val
sealed trait RecordValLike extends Val
case object EmptyRecordVal extends RecordValLike
case class RecordVal(field: Var, loc: LocVal, next: RecordValLike) extends RecordValLike


sealed trait Program
sealed trait Expr extends Program
case object Skip extends Expr
case object False extends Expr
case object True extends Expr
case class NotExpr(expr: Expr) extends Expr
case class Const(n: Int) extends Expr
case class Var(s: String) extends Expr {
  override def toString = s"Var(${"\""}${s}${"\""})"
}
case class Add(l: Expr, r: Expr) extends Expr
case class Sub(l: Expr, r: Expr) extends Expr
case class Mul(l: Expr, r: Expr) extends Expr
case class Div(l: Expr, r: Expr) extends Expr
case class LTEExpr(l: Expr, r: Expr) extends Expr
case class EQExpr(l: Expr, r: Expr) extends Expr
case class Iszero(c: Expr) extends Expr
case class Ite(c: Expr, t: Expr, f: Expr) extends Expr
case class Let(i: Var, v: Expr, body: Expr) extends Expr
case class Proc(args: List[Var], expr: Expr) extends Expr
case class Asn(v: Var, e: Expr) extends Expr
case class BeginEnd(expr: Expr) extends Expr
case class FieldAccess(record: Expr, field: Var) extends Expr
case class FieldAssign(record: Expr, field: Var, new_val: Expr) extends Expr
case class Block(f: Expr, s: Expr) extends Expr
case class PCallV(ftn: Expr, arg: List[Expr]) extends Expr
case class PCallR(ftn: Expr, arg: List[Var]) extends Expr
case class WhileExpr(cond: Expr, body: Expr) extends Expr
sealed trait RecordLike extends Expr
case object EmptyRecordExpr extends RecordLike
case class RecordExpr(field: Var, initVal: Expr, next: RecordLike) extends RecordLike








object MiniCInterpreter {

  case class Result(v: Val, m: Mem)
  case class UndefinedSemantics(msg: String = "", cause: Throwable = None.orNull) extends Exception("Undefined Semantics: " ++ msg, cause)
    
  def eval_recordVal(rv: RecordValLike, field: Var): LocVal = {
        rv match {
          case EmptyRecordVal => throw new UndefinedSemantics("at RecordExpr, first expression")
          case RecordVal(f, l, n) =>{
            if(f.s == field.s) l
            else eval_recordVal(n, field)
          }
          case _ => throw new UndefinedSemantics("at RecordExpr, first expression")
        }
      }
  def eval(env: Env, mem: Mem, expr: Expr): Result = expr match {
    // Constants and Variables
    case Skip => Result(SkipVal, mem)
    case True => Result(BoolVal(true), mem)
    case False => Result(BoolVal(false), mem)
    case Const(n: Int) => Result(IntVal(n), mem)
    case Var(s: String) => {
      
      if(env.size == 0) throw new UndefinedSemantics("at Var, env is empty")
      env.get(Var(s)) match {
        case Some(l:LocVal) => {
          mem.get(l) match {
            case Some(n) => Result(n, mem)
            case _ => throw new UndefinedSemantics("at Var, loc is not in memory")
          }
        }
        case _ => throw new UndefinedSemantics("at Var, env(s) is not in memory")
      }
    }
    case Proc(args, expr) => Result(ProcVal(args, expr, env), mem)

    // Unary and Binary Operation
    case Add(l: Expr, r: Expr) => eval(env, mem, l) match {
        case Result(l_v: IntVal, l_mem: Mem) => eval(env, l_mem, r) match {
            case Result(r_v: IntVal, r_mem: Mem) => Result(IntVal(l_v.n+r_v.n), r_mem)
            case _ => throw new UndefinedSemantics("at Add, right expression")
        }
        case _ => throw new UndefinedSemantics("at Add, left expression")
    }
    case Sub(l: Expr, r: Expr) => eval(env, mem, l) match {
        case Result(l_v: IntVal, l_mem: Mem) => eval(env, l_mem, r) match {
            case Result(r_v: IntVal, r_mem: Mem) => Result(IntVal(l_v.n-r_v.n), r_mem)
            case _ => throw new UndefinedSemantics("at Sub, right expression")
        }
        case _ => throw new UndefinedSemantics("at Sub, left expression")
    }
    case Mul(l: Expr, r: Expr) => eval(env, mem, l) match {
        case Result(l_v: IntVal, l_mem: Mem) => eval(env, l_mem, r) match {
            case Result(r_v: IntVal, r_mem: Mem) => Result(IntVal(l_v.n*r_v.n), r_mem)
            case _ => throw new UndefinedSemantics("at Mul, right expression")
        }
        case _ => throw new UndefinedSemantics("at Mul, left expression")
    }
    case Div(l: Expr, r: Expr) => eval(env, mem, l) match {
        case Result(l_v: IntVal, l_mem: Mem) => eval(env, l_mem, r) match {
            case Result(r_v: IntVal, r_mem: Mem) => {
              if(r_v.n == 0) throw new UndefinedSemantics("at Div, right expression is zero")
              Result(IntVal(l_v.n/r_v.n), r_mem)
            }
            case _ => throw new UndefinedSemantics("at Div, right expression")
        }
        case _ => throw new UndefinedSemantics("at Div, left expression")
    }
    case LTEExpr(l, r) => eval(env, mem, l) match {
        case Result(l_v: IntVal, l_mem: Mem) => eval(env, l_mem, r) match {
            case Result(r_v: IntVal, r_mem: Mem) => {
              if(l_v.n <= r_v.n) Result(BoolVal(true), r_mem)
              else Result(BoolVal(false), r_mem)
            }
            case _ => throw new UndefinedSemantics("at LTEExpr, right expression")
        }
        case _ => throw new UndefinedSemantics(s"at LTEExpr, left expression ${eval(env, mem, l)}")
    }
    case EQExpr(l, r) => eval(env, mem, l) match {
        case Result(l_v: Val, l_mem) => eval(env, l_mem, r) match {
          case Result(r_v: Val, r_mem) => {
            (l_v, r_v) match {
              case (SkipVal, SkipVal) => Result(BoolVal(true), r_mem)
              case (l_vv: IntVal, r_vv: IntVal) => {
                if(l_vv.n == r_vv.n) Result(BoolVal(true), r_mem) 
                else Result(BoolVal(false), r_mem)
              }
              case (l_vv: BoolVal, r_vv: BoolVal) => {
                if(l_vv.b == r_vv.b) Result(BoolVal(true), r_mem)
                else Result(BoolVal(false), r_mem)
              }
              case _ => throw new UndefinedSemantics("at EQExpr, type mismatching")
            }
          }
          case _ => throw new UndefinedSemantics("at EQExpr, right expression")
        }
        case _ => throw new UndefinedSemantics("at EQExpr, left expression")
    }
    case NotExpr(expr: Expr) => eval(env, mem, expr) match {
      case Result(expr_v: BoolVal, expr_mem: Mem) => {
        if(expr_v.b) Result(BoolVal(false), mem)
        else Result(BoolVal(true), mem)
      }
      case _ => throw new UndefinedSemantics("at NotExpr, Expr is not BoolVal")
    }
    case Iszero(c: Expr) => eval(env, mem, c) match {
      case Result(c_v: Val, c_mem: Mem) => {
        c_v match {
          case IntVal(c_vv) => if(c_vv == 0) Result(BoolVal(true), c_mem) else Result(BoolVal(false), c_mem)
          case _ => Result(BoolVal(false), c_mem)
        }
      }
      case _ => throw UndefinedSemantics("at Iszero")
    }

    // Flow Control
    case Ite(c,t,f) => eval(env, mem, c) match {
        case Result(c_v: BoolVal, c_mem: Mem) => {
          if(c_v.b) eval(env, c_mem, t)
          else eval(env, c_mem, f)
        } 
        case _ => throw new UndefinedSemantics("at Ite")
    }
    case WhileExpr(cond: Expr, body: Expr) => eval(env, mem, cond) match {
      case Result(cond_v: BoolVal, cond_mem: Mem) => {
        if(cond_v.b == false) Result(SkipVal, cond_mem)
        else {
          eval(env, mem, body) match {
            case Result(body_v, body_mem) => eval(env, body_mem, WhileExpr(cond, body))
            case _ => throw new UndefinedSemantics("at WhileExpr, true but type error")
          }
        }
      }
      case _ => throw new UndefinedSemantics("at WhileExpr, just type error")
    }
    case Block(f, s) => eval(env, mem, f) match {
      case Result(f_v: Val, f_mem: Mem) => eval(env, f_mem, s) 
      case _ => throw new UndefinedSemantics("at Block, first expression")
    }
    case BeginEnd(expr) => eval(env, mem, expr)
    
    // Records
    case EmptyRecordExpr => Result(EmptyRecordVal, mem)
    case RecordExpr(field, initVal, next) =>  {
      val initVal_result = eval(env, mem, initVal)
      val (new_mem, new_loc) = initVal_result.m.extended(initVal_result.v)
      val r = eval(env, new_mem, next)
      r match {
        case Result(v: RecordValLike, m: Mem) => Result(RecordVal(field, new_loc, v), m)
        case _ => throw new UndefinedSemantics("at RecordExpr, first expression")
      }
    }
    case FieldAccess(record: Expr, field: Var) => {   
      eval(env, mem, record) match {
        case Result(v: RecordValLike, m: Mem) => {
          val loc = eval_recordVal(v, field)
          m.get(loc) match {
            case Some(loc_v: Val) => Result(loc_v, m)
            case _ => throw new UndefinedSemantics("at RecordExpr, first expression")
          }
        }
        case _ => throw new UndefinedSemantics("at RecordExpr, first expression")
      }
    }
    case FieldAssign(record, field, new_val) => eval(env, mem, record) match {
      case Result(r: RecordValLike, r_m: Mem) => r match {
        case EmptyRecordVal => throw new UndefinedSemantics("at RecordExpr, first expression")
        case RecordVal(f, l, next) => eval(env, r_m, new_val) match {
          case Result(new_val_v: Val, new_val_m: Mem) => {
            val new_mem = new_val_m.updated(eval_recordVal(r, field), new_val_v)
            new_mem match {
              case Some(mm: Mem) => {
                Result(new_val_v, mm)
              }
              case _ => throw new UndefinedSemantics("at RecordExpr, first expression")
            }
          }
          case _ => throw new UndefinedSemantics("at RecordExpr, first expression")
        }
        case _ => throw new UndefinedSemantics("at RecordExpr, first expression")
      }
      case _ => throw new UndefinedSemantics("at RecordExpr, first expression")
    }


    // Assignments
    case Asn(v: Var, e: Expr) => eval(env, mem, e) match {
      case Result(e_v: Val, e_mem: Mem) => {
        if(env.size == 0) throw new UndefinedSemantics("at Var, env is empty")
        env.get(v) match {
          case Some(v: LocVal) => {
            val updated_mem = e_mem.updated(v, e_v)
            updated_mem match {
              case None => throw new UndefinedSemantics("at Asn, memory error")
              case Some(u_mem: Mem) => Result(e_v, u_mem)
            }
          }
          case _ => throw new UndefinedSemantics("at Asn, env error")
        }
          
      }
    }
    case Let(i: Var, v: Expr, body: Expr) => eval(env, mem, v) match {
      case Result(i_v: Val, i_mem: Mem) => {
        val (new_mem: Mem, loc: LocVal) = i_mem.extended(i_v)
        val new_env = env + (i -> loc)
        eval(new_env, new_mem, body)
      }
    }

    // Procedure Call
    case PCallV(ftn: Expr, arg: List[Expr]) => eval(env, mem, ftn) match {
      case Result(f_v: ProcVal, f_m: Mem) => {
        // Evaluate parameter and Get updated env and memory
        case class Evaluated_parameter(e: Env, m:Mem)
        def e_parameter(args: List[Var], params: List[Expr], p_e: Env, p_m: Mem, origin_env: Env): Evaluated_parameter = {
          (args, params) match {
            case (Nil, Nil) => Evaluated_parameter(p_e, p_m)
            case (a::a_tail, p::p_tail) => {
              val Result(ep_v, ep_m) = eval(origin_env, p_m, p)
              val (new_mem ,new_loc) = ep_m.extended(ep_v)
              val new_env = p_e + (a -> new_loc)
              e_parameter(a_tail, p_tail, new_env, new_mem, origin_env)
            }
            case _ => throw new UndefinedSemantics("at PcallV, parameter evaluate error")
          }
        }

        val Evaluated_parameter(ee, mm) = e_parameter(f_v.args, arg, f_v.env, f_m, env)

        eval(ee, mm, f_v.expr)
      } 
      case _ => throw new UndefinedSemantics("at PcallV, ftn type error")
    }

    case PCallR(ftn: Expr, arg: List[Var]) => eval(env, mem, ftn) match {
      case Result(f_v: ProcVal, f_m: Mem) => {
        case class Referenced_parameter(env: Env)
        def r_parameter(xs: List[Var], ys: List[Var], env: Env, origin_env: Env): Referenced_parameter = {
          (xs, ys) match {
            case (Nil, Nil) => Referenced_parameter(env)
            case (x::x_tail, y::y_tail) => {
              if(origin_env.size == 0) throw new UndefinedSemantics("at Var, env is empty")
              origin_env.get(y) match {
                case Some(y: LocVal) => {
                  val new_env = env + (x -> y)
                  r_parameter(x_tail, y_tail, new_env, origin_env)
                }
                case _ => throw new UndefinedSemantics("at PcallR, reference type error")
              }
            }
            case _ => throw new UndefinedSemantics("at PcallR, args type error")
          }
        }
        val Referenced_parameter(ee) = r_parameter(f_v.args, arg, f_v.env, env)
        eval(ee, f_m, f_v.expr)
      }
      case _ => throw new UndefinedSemantics("at PcallR, ftn type error")
    }
    
  }

  def gc(env: Env, mem: Mem): Mem = {
    if(env.size == 0) return Mem(mem.m.drop(mem.m.size), mem.top)

    def is_in_env(env: Env, loc:LocVal): Boolean = {
        if(env.size == 0) return false
        env.head match {
          case (e_v: Var, e_l: LocVal) => {
            if(e_l.l == loc.l) return true
            else is_in_env(env.tail, loc)
          }
          case _ => false
        }
      }

    def get_min_key(list: List[LocVal], min: LocVal): LocVal = {
      list match {
        case Nil => min
        case l::tail => {
          if(l.l < min.l) get_min_key(tail, l)
          else get_min_key(tail, min)
        }
      }
    }

    case class Memories(i_m: HashMap[LocVal,Val], m: HashMap[LocVal,Val])

    def resolve_tails(kb: Boolean, kl: LocVal, v: Option[Val], i_mem: HashMap[LocVal,Val], mem:  HashMap[LocVal,Val]): Memories = {
      v match {
        case Some(v_l: LocVal) => {
          if(kb) resolve_tails(kb, v_l, i_mem.get(v_l), i_mem-kl, mem)
          else resolve_tails(kb, v_l, i_mem.get(v_l), i_mem-kl, mem-kl)
        }
        case Some(v_r: RecordValLike) => {
          
          v_r match {
            case EmptyRecordVal => {
              if(kb) return Memories(i_mem-kl, mem)
              else return Memories(i_mem-kl, mem-kl)
            }
            case RecordVal(field: Var, loc: LocVal, next: RecordValLike) => {
              if(kb) resolve_tails(kb, loc, Option(next), i_mem-kl, mem)
              else resolve_tails(kb, loc, Option(next), i_mem-kl, mem-kl)
            }
          }
        }
        case _ => {
          if(kb) return Memories(i_mem-kl, mem)
          else return Memories(i_mem-kl, mem-kl)
        }
      }
    }
    def deallocate_memory(env: Env, i_mem: HashMap[LocVal,Val], mem: HashMap[LocVal,Val]): HashMap[LocVal,Val] = {
      if(i_mem.size == 0) return mem
      i_mem.head match {
        case (l:LocVal, v: Val) => {
          val min_loc = get_min_key(i_mem.keySet.toList, l)
          i_mem.get(min_loc) match {
            case Some(mv:Val) =>{
              val Memories(im, m) = resolve_tails(is_in_env(env, min_loc), min_loc, Option(mv), i_mem, mem)
              deallocate_memory(env, im, m)
            }
            case _ => mem
          }
          
        }
      }
    }
    val deallocated_memory = deallocate_memory(env, mem.m, mem.m)
    return Mem(deallocated_memory, mem.top)
  }
  
  def apply(program: String): (Val, Mem) = {
    val parsed = MiniCParserDriver(program)
    println(s"parsed is ${parsed}\n");
    val res = eval(new Env(), Mem(new HashMap[LocVal,Val],0), parsed)
    (res.v, res.m)
  }

}


object Hw4App extends App {
  
  println("Hello from Hw4!")

}