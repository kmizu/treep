package com.github.kmizu.treep.types

import com.github.kmizu.treep.east.*
import com.github.kmizu.treep.east.ParamParser
import com.github.kmizu.treep.types.Type as T
import com.github.kmizu.treep.types.HM
import com.github.kmizu.treep.types.HM.applyTo

object Checker:
  final case class Diag(msg: String, path: List[String] = Nil)

  final case class Ctx(vars: Map[String, T], funs: Map[String, (List[T], T)])
  object Ctx:
    val empty: Ctx = Ctx(Map.empty, Map.empty)

  def check(prog: Element): List[Diag] =
    val diags = scala.collection.mutable.ListBuffer.empty[Diag]

    def parseType(s: String): Option[T] =
      s.trim match
        case "Int"    => Some(T.TInt)
        case "Bool"   => Some(T.TBool)
        case "String" => Some(T.TString)
        case "Unit"   => Some(T.TUnit)
        case t if t.startsWith("List[") && t.endsWith("]") =>
          parseType(t.stripPrefix("List[").stripSuffix("]")).map(T.TList(_))
        case t if t.startsWith("Dict[") && t.endsWith("]") =>
          val inner = t.stripPrefix("Dict[").stripSuffix("]")
          val parts = inner.split(",").map(_.trim).toList
          parts match
            case k :: v :: Nil =>
              for
                kt <- parseType(k)
                vt <- parseType(v)
              yield T.TDict(kt, vt)
            case _ => None
        case _ => None

    def addDiag(msg: String, path: List[String]): Unit = diags += Diag(msg, path)

    def unify(t1: T, t2: T, path: List[String]): Unit =
      Infer.unify(t1, t2) match
        case Left(err) => addDiag(s"type mismatch: ${t1} vs ${t2}", path)
        case Right(_) => ()

    def typeOfExpr(ctx: Ctx, e: Element, path: List[String]): T = e.kind match
      case "int"    => T.TInt
      case "bool"   => T.TBool
      case "string" => T.TString
      case "var"    => e.name.flatMap(ctx.vars.get).getOrElse(T.TVar(-1))
      case "list"   =>
        val ts = e.children.map(ch => typeOfExpr(ctx, ch, path :+ "list-elem"))
        ts.headOption match
          case Some(t0) => ts.tail.foreach(ti => unify(t0, ti, path :+ "list-elem")); T.TList(ts.headOption.getOrElse(T.TVar(-1)))
          case None     => T.TList(T.TVar(-1))
      case "dict"   =>
        val ks = scala.collection.mutable.ListBuffer.empty[T]
        val vs = scala.collection.mutable.ListBuffer.empty[T]
        e.children.foreach { pair =>
          val k = typeOfExpr(ctx, pair.children.headOption.getOrElse(Element("unit")), path :+ "dict-key")
          val v = typeOfExpr(ctx, pair.children.lift(1).getOrElse(Element("unit")), path :+ "dict-val")
          ks += k; vs += v
        }
        ks.headOption.foreach(k0 => ks.tail.foreach(k => unify(k0, k, path :+ "dict-key")))
        vs.headOption.foreach(v0 => vs.tail.foreach(v => unify(v0, v, path :+ "dict-val")))
        T.TDict(ks.headOption.getOrElse(T.TVar(-1)), vs.headOption.getOrElse(T.TVar(-1)))
      case "index"  =>
        val t = e.getChild("target").map(typeOfExpr(ctx, _, path :+ "index-target")).getOrElse(T.TVar(-1))
        val k = e.getChild("key").map(typeOfExpr(ctx, _, path :+ "index-key")).getOrElse(T.TVar(-1))
        t match
          case T.TDict(kk, vv) => unify(kk, k, path :+ "index"); vv
          case T.TList(v) => unify(k, T.TInt, path :+ "index"); v
          case _ => addDiag("index on non-dict/list", path); T.TVar(-1)
      case "call"   =>
        val name = e.name.getOrElse("?")
        val args = e.children.map(ch => typeOfExpr(ctx, ch, path :+ s"arg"))
        name match
          case "fst" =>
            val a = T.TVar(-101); val b = T.TVar(-102)
            unify(args.head, T.TTuple2(a, b), path)
            a
          case "snd" =>
            val a = T.TVar(-103); val b = T.TVar(-104)
            unify(args.head, T.TTuple2(a, b), path)
            b
          case "+" =>
            args match
              case t1 :: t2 :: Nil =>
                val addPath = path :+ "+"
                (t1, t2) match
                  case (T.TString, T.TString) => T.TString
                  case (T.TInt, T.TInt) => T.TInt
                  case (T.TString, other) =>
                    unify(other, T.TString, addPath); T.TString
                  case (other, T.TString) =>
                    unify(other, T.TString, addPath); T.TString
                  case _ =>
                    unify(t1, T.TInt, addPath)
                    unify(t2, T.TInt, addPath)
                    T.TInt
              case _ =>
                addDiag("operator + expects two operands", path)
                T.TVar(-1)
          case "-" | "*" | "/" | "%" => args.foreach(unify(_, T.TInt, path)); T.TInt
          case ">" | ">=" | "<" | "<=" => args.foreach(unify(_, T.TInt, path)); T.TBool
          case "==" | "!=" =>
            if args.length == 2 then unify(args.head, args(1), path)
            T.TBool
          case "&&" | "||" => args.foreach(unify(_, T.TBool, path)); T.TBool
          case "iter" =>
            args.headOption match
              case Some(T.TList(elem)) => T.TVar(-1) // iterator abstract
              case Some(T.TDict(_, v)) => T.TVar(-1)
              case Some(other) => addDiag(s"iter expects list/dict, got ${other}", path); T.TVar(-1)
              case None => T.TVar(-1)
          case "hasNext" => T.TBool
          case "next" => T.TVar(-1)
          case "print" =>
            if args.length != 1 then addDiag("print expects exactly one argument", path)
            T.TUnit
          case "println" =>
            if args.length != 1 then addDiag("println expects exactly one argument", path)
            T.TUnit
          case other =>
            ctx.funs.get(other) match
              case Some((ps, ret)) =>
                if ps.length != args.length then addDiag(s"arity mismatch: ${other}", path)
                ps.zipAll(args, T.TVar(-1), T.TVar(-1)).foreach { case (p, a) => unify(p, a, path :+ other) }
                ret
              case None => addDiag(s"unknown function: ${other}", path); T.TVar(-1)
      case other => T.TVar(-1)

    def checkBlock(ctx0: Ctx, blk: Element, path: List[String], expectRet: Option[T]): Ctx =
      var ctx = ctx0.copy(vars = ctx0.vars)
      blk.children.foreach { st =>
        st.kind match
          case "let" =>
            val name = st.name.getOrElse("?")
            val initE = st.getChild("init").getOrElse(Element("unit"))
            val t = typeOfExpr(ctx, initE, path :+ s"let ${name}")
            ctx = ctx.copy(vars = ctx.vars + (name -> t))
          case "assign" =>
            val name = st.name.getOrElse("?")
            val rhs = st.children.headOption.getOrElse(Element("unit"))
            val t = typeOfExpr(ctx, rhs, path :+ s"assign ${name}")
            ctx.vars.get(name).foreach(unify(_, t, path :+ s"assign ${name}"))
          case "expr" => st.children.headOption.foreach(e => typeOfExpr(ctx, e, path))
          case "if" =>
            val cond = st.getChild("cond").getOrElse(Element("bool", attrs = List(Attr("value", "true"))))
            unify(typeOfExpr(ctx, cond, path :+ "if-cond"), T.TBool, path)
            val blocks = st.children.filter(_.kind=="block")
            blocks.foreach(b => checkBlock(ctx, b, path :+ "if-branch", expectRet))
          case "while" =>
            val cond = st.getChild("cond").getOrElse(Element("bool", attrs = List(Attr("value", "true"))))
            unify(typeOfExpr(ctx, cond, path :+ "while-cond"), T.TBool, path)
            val body = st.children.find(_.kind=="block").getOrElse(Element("block"))
            checkBlock(ctx, body, path :+ "while-body", None)
          case "return" =>
            val tv = st.children.headOption.map(e => typeOfExpr(ctx, e, path :+ "return")).getOrElse(T.TUnit)
            expectRet.foreach(er => unify(er, tv, path :+ "return"))
          case "match" =>
            val tgt = st.getChild("target").getOrElse(Element("unit"))
            val tt = typeOfExpr(ctx, tgt, path :+ "match-target")
            st.children.filter(_.kind=="case").foreach { cs =>
              val pat = cs.children.headOption.getOrElse(Element("pwild"))
              val body = cs.children.lift(1).getOrElse(Element("block"))
              // very shallow: verify literal pattern types match target
              pat.kind match
                case "pint"  => unify(tt, T.TInt, path :+ "case")
                case "pstr"  => unify(tt, T.TString, path :+ "case")
                case "pbool" => unify(tt, T.TBool, path :+ "case")
                case _ => ()
              checkBlock(ctx, body, path :+ "case-body", expectRet)
            }
          case _ => ()
      }
      ctx

    // Build function signatures from top-level defs
    val funSigs: Map[String, (List[T], T)] = prog.children.collect {
      case d if d.kind == "def" =>
        val name = d.name.getOrElse("?")
        val params: List[T] =
          d.getAttr("params").filter(_.nonEmpty)
            .map(pStr => ParamParser.parseParamTypes(pStr).flatMap(parseType)).getOrElse(Nil)
        val ret: T = d.getAttr("returns").flatMap(parseType).getOrElse(T.TUnit)
        name -> (params -> ret)
    }.toMap

    val base = Ctx.empty.copy(funs = funSigs)
    // HM-based check for function bodies
    var hmEnv: Env = HM.builtinEnv
    // add function signatures to env
    funSigs.foreach { case (n, (ps, r)) => hmEnv = hmEnv.extend(n, Scheme(Set.empty, T.TFun(ps, r))) }

    def isNonExp(e: Element): Boolean = e.kind match
      case "int" | "bool" | "string" => true
      case "var" => true
      case "list" => e.children.forall(isNonExp)
      case "dict" => e.children.forall(p => p.children.headOption.forall(isNonExp))
      case _ => false

    def walkBlockHM(env0: Env, blk: Element, retT: T, fnName: String): Env =
      var env = env0
      blk.children.foreach { st =>
        st.kind match
          case "let" =>
            val initE = st.getChild("init").getOrElse(Element("unit"))
            HM.inferExpr(env, initE) match
              case Right(HM.Result(s, t)) =>
                env = s.applyTo(env)
                val sch = if isNonExp(initE) then Infer.generalize(env, t) else Scheme(Set.empty, t)
                env = env.extend(st.name.getOrElse("?"), sch)
              case Left(_) => addDiag("type inference failed in let", List(s"def ${fnName}", "let"))
          case "assign" =>
            val rhs = st.children.headOption.getOrElse(Element("unit"))
            HM.inferExpr(env, rhs) match
              case Right(HM.Result(s, t)) =>
                env = s.applyTo(env)
                st.name.foreach { varName =>
                  env.lookup(varName).foreach { sch =>
                    val tv = HM.inferExpr(env, Element("var", name = Some(varName))).toOption.get.ty
                    Infer.unify(t, tv) match
                      case Left(_) => addDiag("assignment type mismatch", List(s"def ${fnName}", "assign"))
                      case Right(us) => env = us.applyTo(env)
                  }
                }
              case Left(err) => addDiag(s"type inference failed in assign: ${err}", List(s"def ${fnName}", "assign"))
          case "expr" => st.children.headOption.foreach(e => HM.inferExpr(env, e)) // ignore result
          case "return" =>
            st.children.headOption match
              case None => Infer.unify(retT, T.TUnit) match
                case Left(_) => addDiag("return type mismatch", List(s"def ${fnName}", "return"))
                case Right(_) => ()
              case Some(ex) =>
                HM.inferExpr(env, ex) match
                  case Right(HM.Result(s, t)) =>
                    env = s.applyTo(env)
                    Infer.unify(s.apply(retT), s.apply(t)) match
                      case Left(_) => addDiag("return type mismatch", List(s"def ${fnName}", "return"))
                      case Right(us) => env = us.applyTo(env)
                  case Left(err) => addDiag(s"return type inference failed: ${err}", List(s"def ${fnName}", "return"))
          case "if" =>
            val cond = st.getChild("cond").getOrElse(Element("bool", attrs = List(Attr("value", "true"))))
            HM.inferExpr(env, cond).foreach { case HM.Result(s, t) =>
              env = s.applyTo(env); Infer.unify(t, T.TBool)
            }
            st.children.filter(_.kind=="block").foreach(b => env = walkBlockHM(env, b, retT, fnName))
          case "while" =>
            val cond = st.getChild("cond").getOrElse(Element("bool", attrs = List(Attr("value", "true"))))
            HM.inferExpr(env, cond).foreach { case HM.Result(s, t) => env = s.applyTo(env); Infer.unify(t, T.TBool) }
            val body = st.children.find(_.kind=="block").getOrElse(Element("block"))
            env = walkBlockHM(env, body, retT, fnName)
          case _ => ()
      }
      env

    // Check defs
    prog.children.foreach { ch =>
      ch.kind match
        case "def" =>
          val name = ch.name.getOrElse("?")
          val retT: T = ch.getAttr("returns").flatMap(parseType).getOrElse(T.TUnit)
          val params: List[(String,T)] =
            ch.getAttr("params").filter(_.nonEmpty)
              .map(pStr => ParamParser.parseParams(pStr).flatMap { case (n, tStr) =>
                parseType(tStr).map(t => n -> t)
              }).getOrElse(Nil)
          var env = hmEnv
          params.foreach { case (n,t) => env = env.extend(n, Scheme(Set.empty, t)) }
          val body = ch.children.headOption.getOrElse(Element("block"))
          walkBlockHM(env, body, retT, name)
        case "const" =>
          val cname = ch.name.getOrElse("")
          val init = ch.getChild("init")
          init.foreach { e =>
            HM.inferExpr(hmEnv, e) match
              case Left(err) =>
                addDiag(s"type error in const init: ${err}", List(s"const ${cname}"))
              case Right(HM.Result(s, t)) =>
                val applied = s.apply(t)
                hmEnv = s.applyTo(hmEnv)
                val scheme =
                  if isNonExp(e) then Infer.generalize(hmEnv, applied)
                  else Scheme(Set.empty, applied)
                hmEnv = hmEnv.extend(cname, scheme)
          }
        case "extension" =>
          val receiverParam = ch.getAttr("receiver-param").getOrElse("self")
          val receiverTypeStr = ch.getAttr("receiver-type").getOrElse("?")
          parseType(receiverTypeStr) match
            case Some(receiverType) =>
              ch.children.foreach { methodDef =>
                if methodDef.kind == "def" then
                  val methodName = methodDef.name.getOrElse("?")
                  val retT: T = methodDef.getAttr("returns").flatMap(parseType).getOrElse(T.TUnit)
                  val params: List[(String,T)] =
                    methodDef.getAttr("params").filter(_.nonEmpty)
                      .map(pStr => ParamParser.parseParams(pStr).flatMap { case (n, tStr) =>
                        parseType(tStr).map(t => n -> t)
                      }).getOrElse(Nil)

                  // Create a function type: (receiverType, params...) -> retT
                  val fullParams = receiverType :: params.map(_._2)
                  val funcType = T.TFun(fullParams, retT)

                  // Extract type variables from receiver type for polymorphism
                  val typeVars = Types.ftv(receiverType)
                  val paramScheme = Scheme(typeVars, funcType)

                  val extMethod = ExtensionMethod(methodName, receiverParam, receiverType, paramScheme, retT)
                  hmEnv = hmEnv.addExtension(extMethod)

                  // Also type-check the method body
                  var methodEnv = hmEnv.extend(receiverParam, Scheme(Set.empty, receiverType))
                  params.foreach { case (n,t) => methodEnv = methodEnv.extend(n, Scheme(Set.empty, t)) }
                  val body = methodDef.children.headOption.getOrElse(Element("block"))
                  walkBlockHM(methodEnv, body, retT, methodName)
              }
            case None =>
              addDiag(s"invalid receiver type: ${receiverTypeStr}", List("extension"))
        case _ => ()
    }

    diags.toList
