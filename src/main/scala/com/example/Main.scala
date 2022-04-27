package com.example

import com.example.ExactSizeSeq.NSeq
import cats.syntax.all._

object Main {
  type Vec2[+A] = NSeq[2, A]
  def main(args: Array[String]): Unit = {
    val x: Vec2[Int] = ExactSizeSeq.fromVector[2, Int](Vector(1, 2)).getOrElse(0.pure[Vec2])
    val y: Vec2[Int] = ExactSizeSeq.fromVector[2, Int](Vector(3, 4)).getOrElse(0.pure[Vec2])
    println((x, y).mapN(_ + _))
  }
}
