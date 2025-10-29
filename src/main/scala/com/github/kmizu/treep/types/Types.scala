package com.github.kmizu.treep.types

enum Type:
  case TInt, TBool, TString, TUnit
  case TVar(id: Int)
  case TFun(from: List[Type], to: Type)
  case TList(elem: Type)
  case TDict(key: Type, value: Type)
  case TTuple2(a: Type, b: Type)
  case TIter(elem: Type)
  // Row-polymorphic record: fields plus optional row variable id
  case TRecord(fields: Map[String, Type], rowVar: Option[Int])

final case class Scheme(vars: Set[Int], body: Type)

final case class Subst(map: Map[Int, Type]):
  def apply(t: Type): Type = t match
    case Type.TVar(id) => map.getOrElse(id, t)
    case Type.TFun(ps, r) => Type.TFun(ps.map(apply), apply(r))
    case Type.TList(e) => Type.TList(apply(e))
    case Type.TDict(k, v) => Type.TDict(apply(k), apply(v))
    case Type.TTuple2(a, b) => Type.TTuple2(apply(a), apply(b))
    case Type.TIter(e) => Type.TIter(apply(e))
    case Type.TRecord(fs, rv) => Type.TRecord(fs.view.mapValues(apply).toMap, rv)
    case _ => t

  def compose(that: Subst): Subst =
    val thatMapped = that.map.view.mapValues(apply).toMap
    Subst(thatMapped ++ map)

object Subst:
  val empty: Subst = Subst(Map.empty)

final case class Env(table: Map[String, Scheme]):
  def lookup(x: String): Option[Scheme] = table.get(x)
  def extend(x: String, s: Scheme): Env = Env(table + (x -> s))
  def ftv: Set[Int] = table.values.flatMap(Types.ftv).toSet

object Env:
  val empty: Env = Env(Map.empty)

// Free type variables of a Type
object Types:
  def ftv(t: Type): Set[Int] = t match
    case Type.TVar(id) => Set(id)
    case Type.TFun(ps, r) => ps.flatMap(ftv).toSet ++ ftv(r)
    case Type.TList(e) => ftv(e)
    case Type.TDict(k, v) => ftv(k) ++ ftv(v)
    case Type.TTuple2(a, b) => ftv(a) ++ ftv(b)
    case Type.TIter(e) => ftv(e)
    case Type.TRecord(fs, rv) => fs.values.flatMap(ftv).toSet ++ rv.toSet
    case _ => Set.empty

  def ftv(s: Scheme): Set[Int] = ftv(s.body) diff s.vars

sealed trait TypeError derives CanEqual
object TypeError:
  case class Mismatch(expected: Type, found: Type) extends TypeError
  case class Occurs(id: Int, inType: Type) extends TypeError

object Infer:
  import TypeError.*

  private def occurs(id: Int, t: Type): Boolean = Types.ftv(t).contains(id)

  def bindVar(id: Int, t: Type): Either[TypeError, Subst] =
    if t == Type.TVar(id) then Right(Subst.empty)
    else if occurs(id, t) then Left(Occurs(id, t))
    else Right(Subst(Map(id -> t)))

  def unify(t1: Type, t2: Type): Either[TypeError, Subst] = (t1, t2) match
    case (a, b) if a == b => Right(Subst.empty)
    case (Type.TVar(id), t) => bindVar(id, t)
    case (t, Type.TVar(id)) => bindVar(id, t)
    case (Type.TList(a), Type.TList(b)) =>
      unify(a, b).map(identity)
    case (Type.TIter(a), Type.TIter(b)) =>
      unify(a, b).map(identity)
    case (Type.TDict(ka, va), Type.TDict(kb, vb)) =>
      for
        s1 <- unify(ka, kb)
        s2 <- unify(s1.apply(va), s1.apply(vb))
      yield s2.compose(s1)
    case (Type.TTuple2(a1, b1), Type.TTuple2(a2, b2)) =>
      for
        s1 <- unify(a1, a2)
        s2 <- unify(s1.apply(b1), s1.apply(b2))
      yield s2.compose(s1)
    case (Type.TRecord(fs1, rv1), Type.TRecord(fs2, rv2)) =>
      // unify common fields; extra fields require rowVar on the other side
      val common = fs1.keySet.intersect(fs2.keySet)
      val only1 = fs1.keySet.diff(fs2.keySet)
      val only2 = fs2.keySet.diff(fs1.keySet)
      if only1.nonEmpty && rv2.isEmpty then Left(TypeError.Mismatch(t1, t2))
      else if only2.nonEmpty && rv1.isEmpty then Left(TypeError.Mismatch(t1, t2))
      else
        val init: Either[TypeError, Subst] = Right(Subst.empty)
        val uCommon = common.foldLeft(init) { (accE, k) =>
          for
            acc <- accE
            s <- unify(acc.apply(fs1(k)), acc.apply(fs2(k)))
          yield s.compose(acc)
        }
        uCommon
    case (Type.TFun(as1, r1), Type.TFun(as2, r2)) if as1.length == as2.length =>
      val init: Either[TypeError, Subst] = Right(Subst.empty)
      val argsUnify = as1.zip(as2).foldLeft(init) { case (accE, (x, y)) =>
        for
          acc <- accE
          s <- unify(acc.apply(x), acc.apply(y))
        yield s.compose(acc)
      }
      for
        sArgs <- argsUnify
        sRet  <- unify(sArgs.apply(r1), sArgs.apply(r2))
      yield sRet.compose(sArgs)
    case (a, b) => Left(Mismatch(a, b))

  def instantiate(s: Scheme, nextId: () => Int): Type =
    val mapping: Map[Int, Type] = s.vars.map(v => v -> Type.TVar(nextId())).toMap
    def loop(t: Type): Type = t match
      case Type.TVar(id) => mapping.getOrElse(id, t)
      case Type.TFun(ps, r) => Type.TFun(ps.map(loop), loop(r))
      case Type.TList(e) => Type.TList(loop(e))
      case Type.TDict(k, v) => Type.TDict(loop(k), loop(v))
      case Type.TTuple2(a, b) => Type.TTuple2(loop(a), loop(b))
      case _ => t
    loop(s.body)

  def generalize(env: Env, t: Type): Scheme =
    val vars = Types.ftv(t) diff env.ftv
    Scheme(vars, t)

  // Placeholder for Algorithm W entrypoint (to be implemented)
  def infer(env: Env, any: Any): Either[TypeError, (Subst, Type)] =
    Left(TypeError.Mismatch(Type.TUnit, Type.TUnit)) // stub: replace with real inference

