package com.example

import cats.{Applicative, Eval, Monoid, Traverse}

object ExactSizeSeq {

  type NSeq[N <: Int, +A]

  def fromVector[N <: Int, A](v: Vector[A])(implicit size: ValueOf[N]): Either[String, NSeq[N, A]] =
    if (v.length != size.value) Left(s"size should be exactly ${size.value}")
    else Right(unsafeApply(v))

  implicit def toVector[N <: Int, A](v: NSeq[N, A]): Vector[A] = v.asInstanceOf[Vector[A]]

  final implicit def functorInstance[N <: Int: ValueOf]: Traverse[NSeq[N, *]] with Applicative[NSeq[N, *]] =
    new FunctorInstance[N]

  implicit def monoidInstance[N <: Int: ValueOf, A: Monoid]: Monoid[NSeq[N, A]] = Applicative.monoid

  @inline private def unsafeApply[N <: Int, A](seq: Vector[A]): NSeq[N, A] = seq.asInstanceOf[NSeq[N, A]]
  @inline private def unsafeApplyF[F[_], N <: Int, A](seq: F[Vector[A]]): F[NSeq[N, A]] =
    seq.asInstanceOf[F[NSeq[N, A]]]

  private val vectorTraverse = Traverse[Vector]

  private class FunctorInstance[N <: Int](implicit size: ValueOf[N])
    extends Traverse[NSeq[N, *]]
      with Applicative[NSeq[N, *]] {
    private val n = size.value

    override def traverse[G[_], A, B](fa: NSeq[N, A])(f: A => G[B])(implicit G: Applicative[G]): G[NSeq[N, B]] =
      unsafeApplyF(vectorTraverse.traverse(fa)(f))

    override def pure[A](x: A): NSeq[N, A] = unsafeApply(Vector.fill(n)(x))

    override def foldLeft[A, B](fa: NSeq[N, A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)

    override def foldRight[A, B](fa: NSeq[N, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      vectorTraverse.foldRight(fa, lb)(f)

    override def map2[A, B, Z](fa: NSeq[N, A], fb: NSeq[N, B])(f: (A, B) => Z): NSeq[N, Z] =
      unsafeApply(Vector.tabulate(n)(i => f(fa(i), fb(i))))

    override def ap[A, B](ff: NSeq[N, A => B])(fa: NSeq[N, A]): NSeq[N, B] =
      unsafeApply(Vector.tabulate(n)(i => ff(i)(fa(i))))

    override def map[A, B](fa: NSeq[N, A])(f: A => B): NSeq[N, B] =
      unsafeApply(fa.map(f))

    override def zipWithIndex[A](fa: NSeq[N, A]): NSeq[N, (A, Int)] =
      unsafeApply(fa.zipWithIndex)
  }
}
