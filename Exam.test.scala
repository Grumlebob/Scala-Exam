/* This file is empty on purpose.   It is added, and configured if you
 * wanted to add your own tests during the exam.  It is not graded and
 * should not be submitted.
 */
package adpro

import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{forAll, forAllNoShrink, propBoolean}
import org.scalacheck.Prop
import org.scalactic.TripleEquals.*
import org.scalacheck.Properties
import pigaro.*
import adpro.monads.*
import adpro.Good.*
import adpro.Gens.*
import adpro.parsing.*
import adpro.parsing.Sliceable.*
import adpro.IntervalParser1.*
import adpro.Member


object ExamSpec
  extends org.scalacheck.Properties("exam-2024-autumn"):

  property("A test that always passes (a sanity check)") =
    forAll { (n: Int) => n == n }
  
  property("Q1") = {
    val list1 = List(1, 2, 3, 4)
    val list2 = List(1, 3, 2)
    
    val predicate: (Int, Int) => Boolean = _ < _
    
    goodPairs(list1, predicate) == true &&
    goodPairs(list2, predicate) == false
  }

  property("Q2") = {
    val list = List(1, 2, 3)
    val predicate: (Int, Int) => Boolean = _ < _
    
    val result1 = goodPairs(list, predicate)
    val result2 = goodPairsCurried(list)(predicate)
    
    result1 == result2
  }

  property("Q3") = {
    val originalFunc: ((Int, Int) => Int) => String = f => f(2, 3).toString
    val curriedFunc: Int => Int => Int = a => b => a + b
    
    val result = curriedNested(originalFunc)(curriedFunc)
    
    result == "5"
  }

  property("Q4") = {
    val list = List(1, 2, 3)
    val predicate: Int => Int => Boolean = a => b => a < b
    
    val result1 = Good.goodPairs(list, (a, b) => predicate(a)(b))
    val result2 = Good.goodPairsHotCurry(list)(predicate)
    
    result1 == result2
  }

  property("Q7") = {
    given arbOptionInt: Arbitrary[Option[Int]] = Arbitrary(Gen.option(Gen.choose(0, 100)))
    given arbOptionString: Arbitrary[Option[String]] = Arbitrary(Gen.option(Gen.alphaStr))

    val eitherGen = genEither[Int, String]
    val sample = eitherGen.sample.get

    sample match {
      case Left(value: Int) => value.isInstanceOf[Int]
      case Right(value: String) => value.isInstanceOf[String]
      case null => false
    }
  }


  property("Q8") = {
    val parser = intBetween(10, 20)
    val result = parser.run("15")
    result == Right(Some(15))
  }

  property("Q8") = {
    val parser = intBetween(10, 20)
    val result = parser.run("25")
    result == Right(None)
  }


  property("Q10") = forAll { (normalList: List[Int], someElemenmt: Int) =>
    given memberList: Member[List] = summon[Member[List]]
    memberList.contains(normalList, someElemenmt) == normalList.contains(someElemenmt)
  }


end ExamSpec
