package com.github.kmizu.treep.types

import com.github.kmizu.treep.east.*
import com.github.kmizu.treep.types.Type as T

object HM:
  final case class Result(subst: Subst, ty: T)

  private var nextId: Int = 1000
  private def fresh(): T = { val id = nextId; nextId += 1; T.TVar(id) }

  private def inst(s: Scheme): T = Infer.instantiate(s, () => { val id = nextId; nextId += 1; id })

  def builtinEnv: Env =
    def scheme(t: T): Scheme = Scheme(Set.empty, t)
    val ops: Map[String, Scheme] = Map(
      "+" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TInt)),
      "-" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TInt)),
      "*" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TInt)),
      "/" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TInt)),
      "%" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TInt)),
      ">" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TBool)),
      ">="-> scheme(T.TFun(List(T.TInt, T.TInt), T.TBool)),
      "<" -> scheme(T.TFun(List(T.TInt, T.TInt), T.TBool)),
      "<="-> scheme(T.TFun(List(T.TInt, T.TInt), T.TBool)),
      "=="-> Scheme(Set(1), T.TFun(List(T.TVar(1), T.TVar(1)), T.TBool)),
      "!="-> Scheme(Set(2), T.TFun(List(T.TVar(2), T.TVar(2)), T.TBool)),
      // Collections
      "push" -> Scheme(Set(3), T.TFun(List(T.TList(T.TVar(3)), T.TVar(3)), T.TList(T.TVar(3)))),
      "keys" -> Scheme(Set(4), T.TFun(List(T.TDict(T.TString, T.TVar(4))), T.TList(T.TString))),
      "hasKey" -> scheme(T.TFun(List(T.TDict(T.TString, T.TString), T.TString), T.TBool)),
      // Iterators for lists
      "iter" -> Scheme(Set(5), T.TFun(List(T.TList(T.TVar(5))), T.TIter(T.TVar(5)))),
      "hasNext" -> Scheme(Set(6), T.TFun(List(T.TIter(T.TVar(6))), T.TBool)),
      "next" -> Scheme(Set(7), T.TFun(List(T.TIter(T.TVar(7))), T.TVar(7)))
    )
    Env(ops)

  def inferExpr(env: Env, e: Element): Either[TypeError, Result] = e.kind match
    case "int"    => Right(Result(Subst.empty, T.TInt))
    case "bool"   => Right(Result(Subst.empty, T.TBool))
    case "string" => Right(Result(Subst.empty, T.TString))
    case "var"    => env.lookup(e.name.get).map(inst).toRight(TypeError.Mismatch(T.TVar(-1), T.TVar(-2))).map(t => Result(Subst.empty, t))
    case "dict"   =>
      // unify all values, String keys
      val init: Either[TypeError, (Subst, Option[T])] = Right(Subst.empty, None)
      val res = e.children.foldLeft(init) { case (accE, pair) =>
        val vexpr = pair.children.head
        for
          (sAcc, tOpt) <- accE
          Result(sV, tV) <- inferExpr(sAcc.applyTo(env), vexpr)
        yield tOpt match
          case None => (sV, Some(tV))
          case Some(t0) => Infer.unify(sV.apply(t0), sV.apply(tV)).map(u => (u.compose(sV), Some(u.apply(t0)))).toOption.get
      }
      res.map { case (s, tvOpt) => Result(s, T.TDict(T.TString, tvOpt.getOrElse(fresh()))) }
    case "index"  =>
      val tE = e.children.find(_.kind=="target").flatMap(_.children.headOption).get
      val kE = e.children.find(_.kind=="key").flatMap(_.children.headOption).get
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
      val tgt = e.children.head
      val key = e.attrs.find(_.key=="name").map(_.value).getOrElse("")
      for
        Result(s1, tT) <- inferExpr(env, tgt)
        tv = fresh()
        sD <- Infer.unify(tT, T.TDict(T.TString, tv))
      yield Result(sD.compose(s1), sD.apply(tv))
    case "list"   =>
      val init: Either[TypeError, (Subst, Option[T])] = Right(Subst.empty, None)
      val res = e.children.foldLeft(init) { case (accE, ch) =>
        for
          (sAcc, tOpt) <- accE
          Result(sCh, tCh) <- inferExpr(sAcc.applyTo(env), ch)
          comb <- tOpt match
            case None => Right((sCh, Some(tCh)))
            case Some(t0) => Infer.unify(sCh.apply(t0), sCh.apply(tCh)).map(u => (u.compose(sCh), Some(u.apply(t0))))
        yield comb
      }
      res.map { case (s, topt) => Result(s, T.TList(topt.getOrElse(fresh()))) }
    case "call"   =>
      val name = e.name.get
      val argsE = e.children
      val init: Either[TypeError, (Subst, List[T])] = Right(Subst.empty, Nil)
      val res = argsE.foldLeft(init) { case (accE, ex) =>
        for
          (sAcc, ts) <- accE
          Result(sX, tX) <- inferExpr(sAcc.applyTo(env), ex)
        yield (sX.compose(sAcc), ts :+ tX)
      }
      name match
        case "iter" =>
          // specialize: either List[a] -> Iter[a] or Dict[String,b] -> Iter[b]
          for
            (sArgs, tArgs) <- res
            tv = fresh()
            choice = Infer.unify(tArgs.headOption.getOrElse(fresh()), T.TList(tv)).map(s => (s, tv)).orElse {
              val v = fresh(); Infer.unify(tArgs.headOption.getOrElse(fresh()), T.TDict(T.TString, v)).map(s => (s, v))
            }
            (sC, elem) <- choice
          yield Result(sC.compose(sArgs), T.TIter(elem))
        case _ =>
          for
            (sArgs, tArgs) <- res
            fT <- env.lookup(name).map(inst).toRight(TypeError.Mismatch(T.TVar(-1), T.TVar(-2)))
            beta = fresh()
            sFn <- Infer.unify(sArgs.apply(fT), T.TFun(tArgs.map(sArgs.apply), beta))
          yield Result(sFn.compose(sArgs), sFn.apply(beta))
    case other => Right(Result(Subst.empty, fresh()))

  extension (s: Subst) def applyTo(env: Env): Env =
    Env(env.table.view.mapValues { sch =>
      val t = inst(sch)
      Scheme(Set.empty, s.apply(t))
    }.toMap)
