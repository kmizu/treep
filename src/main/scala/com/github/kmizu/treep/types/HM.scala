package com.github.kmizu.treep.types

import com.github.kmizu.treep.east.*
import com.github.kmizu.treep.east.ParamParser
import com.github.kmizu.treep.types.Type as T
import java.util.concurrent.atomic.AtomicInteger

object HM:
  final case class Result(subst: Subst, ty: T)

  private val nextId = new AtomicInteger(1000)
  private def fresh(): T = T.TVar(nextId.incrementAndGet())
  private val nextRowId = new AtomicInteger(500000)
  private def freshRowId(): Int = nextRowId.incrementAndGet()

  private def inst(s: Scheme): T = Infer.instantiate(s, () => nextId.incrementAndGet())

  def builtinEnv: Env =
    def scheme(t: T): Scheme = Scheme(Set.empty, t)
    var nextSchemeVar: Int = 1
    def freshSchemeVar(): Int =
      val id = nextSchemeVar
      nextSchemeVar += 1
      id
    def poly(count: Int)(build: List[T] => T): Scheme =
      val ids = List.fill(count)(freshSchemeVar())
      val vars = ids.map(T.TVar.apply)
      Scheme(ids.toSet, build(vars))
    val ops: Map[String, Scheme] = Map(
      "+" -> poly(1) { vars =>
        val List(a) = vars
        T.TFun(List(a, a), a)
      },
      "-" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TInt)),
      "*" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TInt)),
      "/" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TInt)),
      "%" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TInt)),
      ">" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TBool)),
      ">="-> scheme(T.TFun(List(T.TInt, T.TInt), T.TBool)),
      "<" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TBool)),
      "<="-> scheme(T.TFun(List(T.TInt, T.TInt), T.TBool)),
      "=="-> poly(1) { vars =>
        val List(a) = vars
        T.TFun(List(a, a), T.TBool)
      },
      "!="-> poly(1) { vars =>
        val List(a) = vars
        T.TFun(List(a, a), T.TBool)
      },
      "print" -> poly(1) { vars =>
        val List(a) = vars
        T.TFun(List(a), T.TUnit)
      },
      "println" -> poly(1) { vars =>
        val List(a) = vars
        T.TFun(List(a), T.TUnit)
      },
      // Collections
      "push" -> poly(1) { vars =>
        val List(a) = vars
        T.TFun(List(T.TList(a), a), T.TList(a))
      },
      // dict helpers, fully polymorphic in key/value
      "keys" -> poly(2) { vars =>
        val List(k, v) = vars
        T.TFun(List(T.TDict(k, v)), T.TList(k))
      },
      "hasKey" -> poly(2) { vars =>
        val List(k, v) = vars
        T.TFun(List(T.TDict(k, v), k), T.TBool)
      },
      // Iterators for lists and dicts
      "iter" -> poly(2) { vars =>
        val List(arg, res) = vars
        T.TFun(List(arg), res)
      }, // specialized in call branch
      "hasNext" -> poly(1) { vars =>
        val List(a) = vars
        T.TFun(List(T.TIter(a)), T.TBool)
      },
      "next" -> poly(1) { vars =>
        val List(a) = vars
        T.TFun(List(T.TIter(a)), a)
      },
      // tuple accessors
      "fst" -> poly(2) { vars =>
        val List(a, b) = vars
        T.TFun(List(T.TTuple2(a, b)), a)
      },
      "snd" -> poly(2) { vars =>
        val List(a, b) = vars
        T.TFun(List(T.TTuple2(a, b)), b)
      },
      // Math functions
      "abs" -> scheme(T.TFun(List(T.TInt), T.TInt)),
      "min" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TInt)),
      "max" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TInt)),
      "pow" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TInt))
    )
    Env(ops)

  def inferExpr(env: Env, e: Element): Either[TypeError, Result] = e.kind match
    case "int"    => Right(Result(Subst.empty, T.TInt))
    case "bool"   => Right(Result(Subst.empty, T.TBool))
    case "string" => Right(Result(Subst.empty, T.TString))
    case "var"    => e.name.flatMap(env.lookup).map(inst).toRight(TypeError.Mismatch(T.TVar(-1), T.TVar(-2))).map(t => Result(Subst.empty, t))
    case "dict"   =>
      // unify keys and values across pairs; accept both children-based and attr-based key forms
      def kvOf(pair: Element): Option[(Element, Element)] =
        if pair.children.length >= 2 then Some((pair.children.head, pair.children(1)))
        else
          pair.getAttr("key").map { keyValue =>
            val k = Element("string", attrs = List(Attr("value", keyValue)))
            val v = pair.children.headOption.getOrElse(Element("unit"))
            (k, v)
          }
      val init: Either[TypeError, (Subst, Option[T], Option[T])] = Right(Subst.empty, None, None)
      val res = e.children.foldLeft(init) { case (accE, pair) =>
        kvOf(pair) match
          case None => accE
          case Some((kexpr, vexpr)) =>
            for
              (sAcc, kOpt, vOpt) <- accE
              Result(sK, tK) <- inferExpr(sAcc.applyTo(env), kexpr)
              Result(sV, tV) <- inferExpr(sK.applyTo(env), vexpr)
              sK2 <- kOpt.map(kk => Infer.unify(sV.apply(kk), sV.apply(tK))).getOrElse(Right(sV))
              sV2 <- vOpt.map(vv => Infer.unify(sK2.apply(vv), sK2.apply(tV))).getOrElse(Right(sK2))
            yield (sV2, Some(sV2.apply(tK)), Some(sV2.apply(tV)))
      }
      res.map { case (s, kOpt, vOpt) => Result(s, T.TDict(kOpt.getOrElse(fresh()), vOpt.getOrElse(fresh()))) }
    case "index"  =>
      val tE = e.getChild("target").getOrElse(Element("unit"))
      val kE = e.getChild("key").getOrElse(Element("unit"))
      for
        Result(s1, tT) <- inferExpr(env, tE)
        Result(s2, tK) <- inferExpr(s1.applyTo(env), kE)
        tv = fresh()
        // try dict: Dict[String, tv] and key String
        tryDict = Infer.unify(s2.apply(tT), T.TDict(T.TString, tv)).flatMap { sD => Infer.unify(sD.apply(tK), T.TString).map(sK => sK.compose(sD)) }
        res <- tryDict.orElse {
          // try list: List[tv] and key Int
          for sL <- Infer.unify(s2.apply(tT), T.TList(tv)); sK <- Infer.unify(sL.apply(tK), T.TInt) yield sK.compose(sL)
        }
      yield Result(res.compose(s2).compose(s1), res.apply(tv))
    case "field" =>
      val tgt = e.children.headOption.getOrElse(Element("unit"))
      val key = e.getAttrOrElse("name", "")
      for
        Result(s1, tT) <- inferExpr(env, tgt)
        tv = fresh()
        // Try record-like first: { key: tv | ρ }
        tryRec = Infer.unify(s1.apply(tT), T.TRecord(Map(key -> tv), Some(freshRowId()))).map(s => Result(s.compose(s1), s.apply(tv)))
        out <- tryRec.orElse {
          // Fallback: Dict[String, tv]
          Infer.unify(s1.apply(tT), T.TDict(T.TString, tv)).map { s => Result(s.compose(s1), s.apply(tv)) }
        }
      yield out
    case "lambda" =>
      val body = e.children.headOption.getOrElse(Element("block"))
      val paramsAttr = e.getAttr("params")
      paramsAttr match
        case Some(sv) =>
          val namesAndTypes: List[(String, T)] = ParamParser.parseParams(sv).flatMap { case (n, tStr) =>
            parseTypeStr(tStr).map(tt => n -> tt)
          }
          val retVar = fresh()
          var env1 = env
          namesAndTypes.foreach { case (n,t) => env1 = env1.extend(n, Scheme(Set.empty, t)) }
          inferBlockReturn(env1, body, retVar) match
            case Left(err) => Left(err)
            case Right((s, found)) =>
              val s2 = if !found then Infer.unify(s.apply(retVar), s.apply(T.TUnit)).getOrElse(s) else s
              Right(Result(s2, T.TFun(namesAndTypes.map(nt => s2.apply(nt._2)), s2.apply(retVar))))
        case None =>
          // backward-compat single param form
          val pName = e.getAttrOrElse("param", "_")
          val pTypeStr = e.getAttr("ptype")
          val pType = pTypeStr.flatMap(parseTypeStr).getOrElse(fresh())
          val retVar = fresh()
          val env1 = env.extend(pName, Scheme(Set.empty, pType))
          inferBlockReturn(env1, body, retVar) match
            case Left(err) => Left(err)
            case Right((s, found)) =>
              val s2 = if !found then Infer.unify(s.apply(retVar), s.apply(T.TUnit)).getOrElse(s) else s
              Right(Result(s2, T.TFun(List(s2.apply(pType)), s2.apply(retVar))))
    case "list"   =>
      val init: Either[TypeError, (Subst, Option[T])] = Right(Subst.empty, None)
      val res = e.children.foldLeft(init) { case (accE, ch) =>
        for
          (sAcc, tOpt) <- accE
          Result(sE, tE) <- inferExpr(sAcc.applyTo(env), ch)
          sU <- tOpt.map(t0 => Infer.unify(sE.apply(t0), sE.apply(tE))).getOrElse(Right(sE))
        yield (sU, Some(sU.apply(tE)))
      }
      res.map { case (s, tOpt) => Result(s, T.TList(tOpt.getOrElse(fresh()))) }
    case "call" =>
      val name = e.name.getOrElse("?")
      // Specialize certain builtins (e.g., iter) to report errors on unsupported types
      if name == "iter" then
        e.children.headOption match
          case None => Left(TypeError.Mismatch(T.TVar(-1), T.TUnit))
          case Some(argE) =>
            for
              Result(s1, tArg) <- inferExpr(env, argE)
              a = fresh(); k = fresh(); v = fresh()
              tryList = Infer.unify(s1.apply(tArg), T.TList(a)).map(s => Result(s, T.TIter(s.apply(a))))
              tryDict = Infer.unify(s1.apply(tArg), T.TDict(k, v)).map(s => Result(s, T.TIter(T.TTuple2(s.apply(k), s.apply(v)))))
              out <- tryList.orElse(tryDict).left.map(_ => TypeError.Mismatch(T.TList(fresh()), tArg))
            yield out
      else
        val args = e.children
        val init: Either[TypeError, (Subst, List[T])] = Right((Subst.empty, Nil))
        val inferredArgs = args.foldLeft(init) { (accE, ex) =>
          for
            (sAcc, ts) <- accE
            Result(sX, tX) <- inferExpr(sAcc.applyTo(env), ex)
          yield (sX.compose(sAcc), ts :+ tX)
        }
        inferredArgs.flatMap { case (sArgs, tArgs) =>
          env.lookup(name).map(inst).toRight(TypeError.Mismatch(T.TVar(-1), T.TVar(-2))).flatMap { fT =>
            val beta = fresh()
            Infer.unify(sArgs.apply(fT), T.TFun(tArgs.map(sArgs.apply), beta)).map { sFn =>
              Result(sFn.compose(sArgs), sFn.apply(beta))
            }
          }
        }
    case "mcall" =>
      val name = e.name.getOrElse("?")
      val recvE = e.children.headOption.getOrElse(Element("unit"))
      val argsE = e.children.tail
      for
        Result(sR, tR) <- inferExpr(env, recvE)
        // infer args under substituted env
        resArgs <- argsE.foldLeft(Right((sR, List.empty[T])): Either[TypeError, (Subst, List[T])]) {
          case (accE, ex) =>
            for
              (sAcc, ts) <- accE
              Result(sX, tX) <- inferExpr(sAcc.applyTo(env), ex)
            yield (sX.compose(sAcc), ts :+ tX)
        }
        (sA, tArgs) = resArgs
        out <- name match
          case "push" =>
            val a = fresh()
            for sL <- Infer.unify(sA.apply(tR), T.TList(a)); sV <- if tArgs.nonEmpty then Infer.unify(sL.apply(tArgs.head), sL.apply(a)) else Right(sL)
            yield Result(sV, T.TList(sV.apply(a)))
          case "append" =>
            val a = fresh()
            for sL <- Infer.unify(sA.apply(tR), T.TList(a)); sV <- if tArgs.nonEmpty then Infer.unify(sL.apply(tArgs.head), sL.apply(a)) else Right(sL)
            yield Result(sV, T.TList(sV.apply(a)))
          case "concat" =>
            val a = fresh()
            val listT = T.TList(a)
            for
              sL <- Infer.unify(sA.apply(tR), listT)
              sArg <- if tArgs.nonEmpty then Infer.unify(sL.apply(tArgs.head), sL.apply(listT)) else Right(sL)
            yield Result(sArg, T.TList(sArg.apply(a)))
          case "length" =>
            val a = fresh()
            val tryList = Infer.unify(sA.apply(tR), T.TList(a)).map(s => Result(s, T.TInt))
            val tryString = Infer.unify(sA.apply(tR), T.TString).map(s => Result(s, T.TInt))
            tryList.orElse(tryString)
          case "head" =>
            val a = fresh()
            for sL <- Infer.unify(sA.apply(tR), T.TList(a)) yield Result(sL, sL.apply(a))
          case "tail" =>
            val a = fresh()
            for sL <- Infer.unify(sA.apply(tR), T.TList(a)) yield Result(sL, T.TList(sL.apply(a)))
          case "iter" =>
            val a = fresh(); val k = fresh(); val v = fresh()
            // List[a] -> Iter[a], Dict[k,v] -> Iter[(k,v)]
            val tryList = Infer.unify(sA.apply(tR), T.TList(a)).map(s => Result(s, T.TIter(s.apply(a))))
            val tryDict = Infer.unify(sA.apply(tR), T.TDict(k, v)).map(s => Result(s, T.TIter(T.TTuple2(s.apply(k), s.apply(v)))))
            tryList.orElse(tryDict)
          case "size" =>
            val k = fresh(); val v = fresh()
            for sD <- Infer.unify(sA.apply(tR), T.TDict(k, v)) yield Result(sD, T.TInt)
          case "hasKey" =>
            val k = fresh(); val v = fresh()
            for sD <- Infer.unify(sA.apply(tR), T.TDict(k, v)); sK <- if tArgs.nonEmpty then Infer.unify(sD.apply(tArgs.head), sD.apply(k)) else Right(sD)
            yield Result(sK, T.TBool)
          case "keys" =>
            val k = fresh(); val v = fresh()
            for sD <- Infer.unify(sA.apply(tR), T.TDict(k, v)) yield Result(sD, T.TList(sD.apply(k)))
          case "values" =>
            val k = fresh(); val v = fresh()
            for sD <- Infer.unify(sA.apply(tR), T.TDict(k, v)) yield Result(sD, T.TList(sD.apply(v)))
          case "entries" =>
            val k = fresh(); val v = fresh()
            for sD <- Infer.unify(sA.apply(tR), T.TDict(k, v)) yield Result(sD, T.TList(T.TTuple2(sD.apply(k), sD.apply(v))))
          case "get" =>
            val k = fresh(); val v = fresh()
            for sD <- Infer.unify(sA.apply(tR), T.TDict(k, v)); sK <- if tArgs.nonEmpty then Infer.unify(sD.apply(tArgs.head), sD.apply(k)) else Right(sD)
            yield Result(sK, sK.apply(v))
          case "getOrElse" =>
            val k = fresh(); val v = fresh()
            for
              sD <- Infer.unify(sA.apply(tR), T.TDict(k, v))
              sK <- if tArgs.nonEmpty then Infer.unify(sD.apply(tArgs.head), sD.apply(k)) else Right(sD)
              sDef <- if tArgs.lengthCompare(1) > 0 then Infer.unify(sK.apply(tArgs(1)), sK.apply(v)) else Right(sK)
            yield Result(sDef, sDef.apply(v))
          case "put" =>
            val k = fresh(); val v = fresh()
            for
              sD <- Infer.unify(sA.apply(tR), T.TDict(k, v))
              sK <- if tArgs.nonEmpty then Infer.unify(sD.apply(tArgs.head), sD.apply(k)) else Right(sD)
              sV <- if tArgs.lengthCompare(1) > 0 then Infer.unify(sK.apply(tArgs(1)), sK.apply(v)) else Right(sK)
            yield Result(sV, T.TDict(sV.apply(k), sV.apply(v)))
          case "remove" =>
            val k = fresh(); val v = fresh()
            for
              sD <- Infer.unify(sA.apply(tR), T.TDict(k, v))
              sK <- if tArgs.nonEmpty then Infer.unify(sD.apply(tArgs.head), sD.apply(k)) else Right(sD)
            yield Result(sK, T.TDict(sK.apply(k), sK.apply(v)))
          case "hasNext" =>
            val a = fresh(); for sI <- Infer.unify(sA.apply(tR), T.TIter(a)) yield Result(sI, T.TBool)
          case "next" =>
            val a = fresh(); for sI <- Infer.unify(sA.apply(tR), T.TIter(a)) yield Result(sI, sI.apply(a))
          case "toList" =>
            val a = fresh(); for sI <- Infer.unify(sA.apply(tR), T.TIter(a)) yield Result(sI, T.TList(sI.apply(a)))
          case "split" =>
            for sS <- Infer.unify(sA.apply(tR), T.TString); sD <- if tArgs.nonEmpty then Infer.unify(sS.apply(tArgs.head), T.TString) else Right(sS)
            yield Result(sD, T.TList(T.TString))
          case "substring" =>
            for
              sS <- Infer.unify(sA.apply(tR), T.TString)
              s1 <- if tArgs.nonEmpty then Infer.unify(sS.apply(tArgs.head), T.TInt) else Right(sS)
              s2 <- if tArgs.lengthCompare(1) > 0 then Infer.unify(s1.apply(tArgs(1)), T.TInt) else Right(s1)
            yield Result(s2, T.TString)
          case "contains" =>
            for sS <- Infer.unify(sA.apply(tR), T.TString); sD <- if tArgs.nonEmpty then Infer.unify(sS.apply(tArgs.head), T.TString) else Right(sS)
            yield Result(sD, T.TBool)
          case "join" =>
            val a = fresh()
            for sL <- Infer.unify(sA.apply(tR), T.TList(a)); sD <- if tArgs.nonEmpty then Infer.unify(sL.apply(tArgs.head), T.TString) else Right(sL)
            yield Result(sD, T.TString)
          case _ =>
            // Try extensions first
            val tryExtension: Option[Either[TypeError, Result]] = env.extensions.find(_.methodName == name).map { ext =>
              // Instantiate the extension's polymorphic scheme
              val extFuncType = inst(ext.paramScheme)
              // The function type is (receiverType, params...) -> returnType
              extFuncType match
                case T.TFun(recvType :: paramTypes, retType) =>
                  // Unify receiver type
                  for
                    sRecv <- Infer.unify(sA.apply(tR), sA.apply(recvType))
                    // Unify argument types
                    sArgs <- if paramTypes.length == tArgs.length then
                      val init: Either[TypeError, Subst] = Right(sRecv)
                      paramTypes.zip(tArgs).foldLeft(init) { case (accE, (pType, aType)) =>
                        for
                          acc <- accE
                          s <- Infer.unify(acc.apply(pType), acc.apply(aType))
                        yield s.compose(acc)
                      }
                    else
                      Left(TypeError.Mismatch(T.TVar(-1), T.TVar(-2)))
                  yield Result(sArgs.compose(sRecv).compose(sA), sArgs.apply(retType))
                case _ => Left(TypeError.Mismatch(T.TVar(-1), T.TVar(-2)))
            }

            tryExtension.getOrElse {
              // Try record-style method: recv has field `name` of type (args) -> beta
              val beta = fresh()
              val tryRecord = Infer.unify(sA.apply(tR), T.TRecord(Map(name -> T.TFun(tArgs, beta)), Some(freshRowId()))).map { sRec =>
                Result(sRec.compose(sA), sRec.apply(beta))
              }
              tryRecord.orElse {
                val init: Either[TypeError, (Subst, List[T])] = Right((sA, tR :: tArgs))
                for
                  (sAll, allArgs) <- init
                  fT <- env.lookup(name).map(inst).toRight(TypeError.Mismatch(T.TVar(-1), T.TVar(-2)))
                  beta = fresh()
                  sFn <- Infer.unify(sAll.apply(fT), T.TFun(allArgs.map(sAll.apply), beta))
              yield Result(sFn.compose(sAll), sFn.apply(beta))
              }
            }
      yield out
    case other => Right(Result(Subst.empty, fresh()))

  extension (s: Subst) def applyTo(env: Env): Env =
    Env(env.table.view.mapValues { sch =>
      sch.copy(body = s.apply(sch.body))
    }.toMap, env.extensions)

  private def parseTypeStr(s: String): Option[T] =
    def trim(s: String) = s.trim
    def parseAtom(s: String): Option[T] =
      if s.startsWith("List[") && s.endsWith("]") then parseTypeStr(s.substring(5, s.length-1)).map(T.TList(_))
      else if s.startsWith("Dict[") && s.endsWith("]") then
        val inner = s.substring(5, s.length-1)
        val parts = inner.split(",").toList.map(_.trim)
        parts match
          case a :: b :: Nil => for ta <- parseTypeStr(a); tb <- parseTypeStr(b) yield T.TDict(ta, tb)
          case _ => None
      else s match
        case "Int" => Some(T.TInt)
        case "Bool" => Some(T.TBool)
        case "String" => Some(T.TString)
        case "Unit" => Some(T.TUnit)
        case _ => None
    // parse right-assoc arrows
    def splitArrow(s: String): List[String] =
      var depth = 0
      val b = new StringBuilder
      val parts = scala.collection.mutable.ListBuffer.empty[String]
      var i = 0
      while i < s.length do
        val c = s.charAt(i)
        if c == '[' then depth += 1
        else if c == ']' then depth -= 1
        if depth == 0 && i+1 < s.length && s.substring(i, i+2) == "->" then
          parts += b.toString
          b.clear()
          i += 2
        else { b.append(c); i += 1 }
      parts += b.toString
      parts.toList.map(_.trim)
    val parts = splitArrow(s)
    parts match
      case a :: b :: Nil => for ta <- parseTypeStr(a); tb <- parseTypeStr(b) yield T.TFun(List(ta), tb)
      case single :: Nil => parseAtom(single)
      case a :: rest =>
        val right = rest.mkString("->")
        for ta <- parseTypeStr(a); tb <- parseTypeStr(right) yield T.TFun(List(ta), tb)
      case Nil => None

  // Infer returns in a block for lambda typing
  private def inferBlockReturn(env0: Env, blk: Element, retVar: T): Either[TypeError, (Subst, Boolean)] =
    var env = env0
    var subst: Subst = Subst.empty
    var found = false

    def goExpr(e: Element): Either[TypeError, Result] = inferExpr(env, e)
    def goBlock(b: Element): Either[TypeError, (Subst, Boolean)] = inferBlockReturn(env, b, retVar)

    def goStmt(st: Element): Either[TypeError, (Subst, Boolean)] = st.kind match
      case "let" =>
        val initE = st.children.find(_.kind=="init").flatMap(_.children.headOption).get
        goExpr(initE).map { case Result(s, t) =>
          env = s.applyTo(env)
          env = env.extend(st.name.get, Scheme(Set.empty, s.apply(t)))
          (s, false)
        }
      case "assign" =>
        val rhs = st.children.head
        goExpr(rhs).map { case Result(s, _) => env = s.applyTo(env); (s, false) }
      case "expr" => goExpr(st.children.head).map { case Result(s, _) => env = s.applyTo(env); (s, false) }
      case "return" =>
        val exprOpt = st.children.headOption
        val retT: T = exprOpt match
          case None => T.TUnit
          case Some(ex) =>
            goExpr(ex) match
              case Left(err) => return Left(err)
              case Right(Result(s, t)) => env = s.applyTo(env); s.apply(t)
        Infer.unify(retVar, retT).map { s2 =>
          subst = s2.compose(subst)
          found = true
          (s2, true)
        }
      case "if" =>
        val cond = st.children.find(_.kind=="cond").flatMap(_.children.headOption).get
        val blocks = st.children.filter(_.kind=="block")
        val thenBlk = blocks.headOption.getOrElse(Element("block"))
        val elseBlk = blocks.lift(1)
        for
          Result(sC, tC) <- goExpr(cond)
          sBool <- Infer.unify(sC.apply(tC), T.TBool)
          _ = { env = sBool.applyTo(sC.applyTo(env)); subst = sBool.compose(subst) }
          rThen <- goBlock(thenBlk)
          rElse <- elseBlk.map(goBlock).getOrElse(Right((Subst.empty, false)))
        yield {
          subst = rElse._1.compose(rThen._1).compose(subst)
          found = found || rThen._2 || rElse._2
          (subst, found)
        }
      case "while" =>
        val cond = st.children.find(_.kind=="cond").flatMap(_.children.headOption).get
        for
          Result(sC, tC) <- goExpr(cond)
          sBool <- Infer.unify(sC.apply(tC), T.TBool)
        yield { env = sBool.applyTo(sC.applyTo(env)); subst = sBool.compose(subst); (subst, false) }
      case "match" =>
        val tgt = st.children.find(_.kind=="target").flatMap(_.children.headOption).get
        for
          Result(sT, _) <- goExpr(tgt)
          _ = { env = sT.applyTo(env); subst = sT.compose(subst) }
          res <- st.children.filter(_.kind=="case").foldLeft(Right((subst, false)): Either[TypeError,(Subst,Boolean)]) { (accE, cs) =>
            accE.flatMap { case (sAcc, fAcc) =>
              goBlock(cs.children(1)).map { case (sB, fB) =>
                (sB.compose(sAcc), fAcc || fB)
              }
            }
          }
        yield { subst = res._1; found = found || res._2; (subst, found) }
      case _ => Right((Subst.empty, false))

    blk.children.foldLeft(Right((Subst.empty, false)): Either[TypeError,(Subst,Boolean)]) { (accE, st) =>
      accE.flatMap { case (sAcc, fAcc) =>
        env = sAcc.applyTo(env)
        goStmt(st).map { case (sS, fS) => (sS.compose(sAcc), fAcc || fS) }
      }
    }
