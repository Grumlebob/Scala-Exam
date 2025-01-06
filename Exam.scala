/* Final Exam: Advanced Programming, by Andrzej Wąsowski IT University
 * of Copenhagen, Autumn 2024: 06 January 2025
 *
 * The exam consists of 12 questions to be solved within 4 hours.
 * Solve the tasks in the file 'Exam.scala' (this file).
 *
 * You can use all functions provided in the included files,  as well as
 * functions that we implemented in the course. If the source is missing in
 * this folder, you can add it to this file (so that things compile on our
 * side). You can use the standard library functions as well. Staying closer
 * to the course API is likely to yield nicer solutions.
 *
 * You can access any static written materials, printed and online, but you
 * are not allowed to communicate with anybody or with anything (bots).
 * Using GitHub copilot, ChatGPT and similar language models during the exam
 * is not allowed. By submitting you legally declare to have solved the
 * problems alone, without communicating with anybody, and not using
 * language models.
 *
 * Do not modify this file in other ways than answering the questions or
 * adding imports and source of needed functions. Do not reorder the
 * answers, and do not remove question numbers or comments from the file.
 *
 * Submit this file and only this file to LearnIT. Do not convert to
 * any other format than .scala. Do not submit the entire zip archive.
 * The only accepted file format is '.scala'.
 *
 * Keep the solutions within 80 character columns to make grading easier.
 *
 * The answers will be graded manually. We focus on the correctness of
 * ideas, the use of concepts, clarity, and style. We will use undisclosed
 * automatic tests during grading, but not to compute the final grade, but
 * to help us debug your code.
 *
 * We do require that your hand-in compiles.  The directory has a project
 * setup so compilation with scala-cli shall work out-of-the-box. If you
 * cannot make a fragment compile, put your solution in a comment, next to
 * the three question marks. We will grade the solutions in comments as
 * well.
 *
 * We will check whether the file compiles by running
 *
 *    scala-cli compile .
 *
 * Hand-ins that do not compile will automatically fail the exam.
 *
 * There is a skeletong test file in the bundle, that you can use to test
 * your solutions.  It does not contain any useful tests. It is just there
 * to get you started with testing faster.
 *
 * We do not recommend writing and running tests if you are pressed for
 * time. It is a good idea to run and test, if you have time.  The tests
 * should not be handed in.  We only grade the answers to questions below.
 *
 * Good luck!
 **/

package adpro

import org.scalacheck.{Arbitrary, Gen, Prop}
import Arbitrary.*, Prop.*
import org.scalactic.TripleEquals.*

import adpro.laziness.LazyList
import adpro.state.*

//I am going to write my implementation thoughts, in start of every method.
//In hope that if my thoughts are correct, 
//but my implementation is wrong, i might net a few points :)

object Good:

  /* QUESTION 1 ######################################################
   *
   * Implement a function `goodPairs` that checks whether all pairs of
   * consecutive elements in a list satisfy a predicate. Choose the
   * right higher order function for the task. If you can't solve this
   * with higher order functions, using recursion still makes sense,
   * even if for less points.
   */

  def goodPairs[A](l: List[A], good: (A, A) => Boolean): Boolean = {
    //Thougts while implementing
    //I am simply using the "lazyList approach" i also used in the 
    // 2022 exam for method "primesApart"
    //But except of checking if they are N apart, i simply use function "good"
    val listMinusHead = l.drop(1)
    val consecutivePairs = l.zip(listMinusHead)

    consecutivePairs.forall(pair => good(pair._1, pair._2))
  }




  /* QUESTION 2 #####################################################
   *
   * Recall the functions  curry and uncurry from the course (week 1).
   * In this exercise we use the standard library counterparts,
   * `curried` and `uncurried` see these docs (if you don't recall
   * them):
   *
   * https://scala-lang.org/api/3.4.2/scala/Function$.html#uncurried-d4
   * https://scala-lang.org/api/3.4.2/scala/Function2.html#curried-0
   *
   * Use the right one of these functions to produce a function
   * `goodPairsCurried` by transforming goodPairs programmatically,
   * without writing it from scratch. The expected type is given below.
   *
   * This question can be solved even if you did not answer Q1. Just
   * assume you have the solution for Q1.
   */

  //Thougts while implementing
  //(From my notes): Currying is the technique of converting a function that 
  //takes multiple arguments into a sequence of 
  //functions that each takes a single argument
  // So here goodPairs takes two params our list and the function "good"
  //By curriying it, i get first param => new function.
  // "second param new function" => Final boolean result.
  //This matches our signature. Where List[A] => is our first param
  //And param "good" matches ((A, A) => Boolean)
  //So the correct function of those two above must be .curried
  //The first param list and second param good, 
  //is given to the curried version goodPairs.
  def goodPairsCurried[A]: List[A] => ((A, A) => Boolean) => Boolean =
    ((l: List[A], good: (A, A) => Boolean) => goodPairs(l, good)).curried


  /* QUESTION 3. #####################################################
   *
   * Now Implement function curriedNested that takes a higher order
   * function with the first argument being an uncurried binary
   * function and curries the first argument. See the type
   * specification below.
   *
   * This question can be solved even if you did not answer the
   * previous questions.
   */

  def curriedNested [A, B, C, D] (f: ((A,B) => C) => D)
    : (A => B => C) => D =
      //Thougts while implementing
      //I need to return something that returns D.
      //I can get that D from our function f.
      //In order to use f, I need to uncurry our anonymous curried function.
      //I can name the anonymous curried function, to use
      // the .uncurried function on it.
      //This is because f takes a tuple (A,B). Whereas our 
      // curried function takes A => B => C
      //As I stated above, curry is a sequence of functions 
      //with just a single parameter.
      //But our f, doesn't take a sequence, it takes a tuple.
      //So by uncurrying I can make the sequence of 
      // A => B => C into a tupled (A,B) => C
      //This exactly fits what our function f, in order to get D
      (someCurriedFunction: A => B => C) => 
        f(Function.uncurried(someCurriedFunction))


  /* QUESTION 4 ######################################################
   *
   * Create a function goodPairsHotCurry where both the top-level
   * function and the first argument are curried. Do not implement the
   * function from scratch but use curriedNested and standard library
   * functions to transform `goodPairs`.
   *
   * This question can be solved even if you did not answer the
   * previous questions.
   */

  def goodPairsHotCurry[A]: List[A] => (A => A => Boolean) => Boolean =
    //Thougts while implementing
    //I need some lambda l: List[A], that returns a new function.
    //The new function should look like 
    // (A => A => Boolean) => Boolean
    //I can use our curriedNested, that looks like (f: ((A,B) => C) => D)
  
    //In order to use goodPair, which has parameter 
    // good: (A, A) => Boolean): Boolean
    //I know from that, I can use set our function to be 
    // (f: (A, A) => Boolean) => Something something.
    //I know from goodPairsHotCurry signature, that 
    // the something something is a boolean.
    //Thus I just need to give the list from the inital lambda, 
    //aswell as the function defined above.
    //To goodpairs, which will return a boolean.
    (lambdaList: List[A]) => 
      curriedNested((f: (A, A) => Boolean) => goodPairs(lambdaList, f))


end Good



object MultivariateUniform:

  import pigaro.*
  import adpro.monads.*

  /* QUESTION 5 #####################################################
   *
   * Recall our probabilistic programming library Pigaro.  We want to show
   * that Pigaro's `Dist` type constructor is a monad. Provide evidence (a
   * given, an instance) of Monad for Dist.
   */

   /*
   Thougts while implementing:
   (From my lecture notes):
    A monad is an implementation of one of the minimal 
    sets of monadic combinators, 
    satisfying the laws of associativity(compose/flatmap) 
    and identity (empty/unit).


    So i need to implement Identity:Unit, and Compose:FlatMap
   */

  given Monad[Dist] with

    //Identity in Dist is Dirac.
    def unit[A](a: => A): Dist[A] = Dirac(Name.No, a)
    
    extension [A](fa: Dist[A])
      //compose is FlatMap.
      override def flatMap[B](f: A => Dist[B]): Dist[B] =
        fa.flatMap(f)


 /* QUESTION 6 #####################################################
  *
  * Implement a function `multUni`  that represents a product of
  * n identical uniform distributions, where n is its first argument.
  * A single sample from this distribution is a list of size n.
  *
  * def multUni (n: Int, values: T*): Dist[List[T]]
  *
  * You likely need to use the fact that Dist is a monad. If you do so
  * you should ensure that the function signature enforces this
  * requirement on the caller. Questions 5 and 6 are conceptually
  * related, but this one can be answered without answering Q5.
  */

  def multUni[T](n: Int, values: T*): Dist[List[T]] =
    //Thougts while implementing
    //We can summon our Monad above, to get a distMonad
    //If n is zero (or perhaps negative), we just use Unit on the empty list.
    //If n is positive. We take a uniform distribution of the values.
    //And i am asked to do "n identical uniform distributions, 
    // where n is its first argument."
    //Monad has access to "  
    //def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    //  sequence(List.fill(n)(fa))"
    //So i can use replicateM(n, theDistributions), to do just that.
    
    val distMonad = summon[Monad[Dist]]

    if n <= 0 then
      val emptyList = List.empty[T]
      distMonad.unit(emptyList)
    else
      //I forgot if i should use Pigaro.uniform(values*)
      //or if i should use Uniform(values*)
      //I assume they do the same thing.
      val singleUniformDist = Pigaro.uniform(values*)
      distMonad.replicateM(n, singleUniformDist)
  
end MultivariateUniform


object Gens:

  /* QUESTION 7 ######################################################
   *
   * Imagine we are writing some tests for a function that takes a value of
   * type Either[A,B] as an input, for some unknown types A and B (type
   * parameters).  We do not have access to any Arbitrary[A] and
   * Arbitrary[B] instances. Instead, we have access to Arbitrary[Option[A]]
   * and Arbitrary[Option[B]] instances.
   *
   * Write a function genEither[A,B] that returns a value of
   * Gen[Either[A,B]] using the Arbitrary[Option[A]] and
   * Arbitrary[Option[B]]. Your implementatation needs to ensure that the
   * arbitraries are available in the scope of the function (the type
   * checker must check for their existance).
   *
   * We are working with the scalacheck library here, so we use
   * org.scalacheck.Gen and org.scalacheck.Arbitrary, not the book's Gen.
   *
   * A direct recursion is allowed and will award maximum points in this
   * exercise. Non-recusive solutions are also possible.
   */

  import org.scalacheck.{Gen, Arbitrary}
  import org.scalacheck.Arbitrary.arbitrary


  def genEither[A, B]
  (using optionA: Arbitrary[Option[A]], optionB: Arbitrary[Option[B]])
  : Gen[Either[A, B]] =
    //Thougts while implementing
    //Remember what Thor taught two days ago, that most "default" Arbitraries, 
    //can be used without doing anything,
    //Such as integers, list[int], Options, etc.
    //So i can either do two summons, or just 
    // alter signature to two using statements on Arbitrary[Option[A]] and B
    //And it should already be possible without making custom implementation.

    //Also remember that option and either are quite similair.
    //Option is the one with Some and None
    //Either is the one with Left(usually an error, in Option world it is None)
    // or Right(correct, in option Some)
    //So here i decide that Option[A] should be Left[A]
    //And that Option[B] should be Right[B]
    //built-in Gen.oneOf, works great for giving Either[Left] or Either[Right]

    Gen.oneOf(
      optionA.arbitrary.flatMap {
        case Some(a) => Gen.const(Left(a))
        case None    => genEither(using optionA, optionB)
      },
      optionB.arbitrary.flatMap {
        case Some(b) => Gen.const(Right(b))
        case None    => genEither(using optionA, optionB)
      }
    )


end Gens



object IntervalParser1:

  import adpro.parsing.*
  import adpro.parsing.Sliceable.*

  /* QUESTION 8 ######################################################
   *
   * Implement a parser that accepts a single integer from a closed
   * interval between low and high.
   *
   *    intBetween(low: Int, high: Int): Parser[Option[Int]]*
   *
   * The parser always succeeds. It returns Some(n) if it parses an integer
   * n. It returns None, if the integer is not in the interval.
   *
   * Use the parser combinator library developed in the course. You may want
   * to use a concrete parser implemetnation. The parser `Sliceable` is
   * included in the exam project.
   */

  def intBetween(low: Int, high: Int): Parser[Option[Int]] =
    //Thougts while implementing
    //I am not sure how input would look like on negative numbers.
    //So for the sake of it, my mental input is "777" and always positive.
    //I need a regex or perhaps 
    // some built-in method in the Slicable Library to get the numbers
    //And then get the regex matched string, to be converted into an integer.
    //Then i want to check the bounds of the integer, 
    // and give Some(number) or None.
    //Remember that the assignment says to always succeed, 
    // so if Regex matches nothing, i need a case
    //where i always return succeed(None)

    val numberParser = regex("\\d+".r)

    numberParser.map { numberAsString =>
      val numberAsInt = numberAsString.toIntOption

      numberAsInt match {
        case Some(number) if number >= low && number <= high => Some(number)
        case _ => None
      }
    }
    //I added this because Assignment says "The parser always succeeds"
    // | is defined as:  def |(p2: => Parser[A]): Parser[A] = p.or (p2)
    //So if it doesn't match, we go into this "or" 
    // such as "ABC" will succeed with None.
    | succeed(None)

end IntervalParser1



object IntervalParser2:

  import adpro.parsing.*

  /* QUESTION 9 #####################################################
   *
   * Notice that `intBetween` is independent of the concrete parser
   * implementation.  We can abstract over the parser type. Implement it
   * again as an extension that works for any implementation of the
   * `Parsing` structure
   *
   * This question depends on the previous one. You need to copy your
   * answer to Q8 and generalize it to an extension of instances of
   * Parsers. Since now our parser implementation is abstract  you may
   * need to build the integer token lexer differently than in Q8 (it
   * depends a bit on which solution you proposed in Q8---you can no
   * longer use methods from Sliceable here).
   *
   * The goal is to have something like this code compile:
   *
   *  import IntervalParser2.*
   *  def f [P[+_]] (p: Parsers[ParseError, P]) =
   *    p.intBetween(0,0) ...
   *
   * HINT: The extension will be for p: Parsers[ParseError, P] for
   * some implementation of `Parsing` represented by type constructor
   * variable P[+_].
   */


  extension [P[+_]](p: Parsers[ParseError, P])
    
    def intBetween(low: Int, high: Int): P[Option[Int]] =
      //Same thoughts as before.
      //Except,i can't map directly using our numberParser, 
      //because "P" is a type constructor, and not a concrete parser.
      //But i do have small-case "p" which is a Parser, 
      // that have almost the same extension methods, as previous assignment.
      //p has extension method flatMap. 
      // Looks like this: def flatMap[B](f: A => Parser[B]): Parser[B]
      //So i will simply replace above implementation, with extension methods.

      val numberParser: P[String] = p.regex("\\d+".r)

      p.flatMap(numberParser) { numberAsString =>
        val numberAsInt = numberAsString.toIntOption
        
        numberAsInt match {
          case Some(number) if number >= low && number <= high => p.succeed(Some(number))
          case _ => p.succeed(None)
        }
      }
      //I am now not sure how to make it always succeed 
      //with the extension methods from parser.
      //My best guess is something like:
      //p.or.succeed(None)

end IntervalParser2


/* QUESTION 10 ########################################################
 *
 * Implement a type class `Member[F[+_]]` that ensures that its instances
 * provide a method `contains`:
 *
 *   def contains[A] (fa: F[A], a: A): Bolean
 *
 * The intuition is that this method can be used to check whether `fa`
 * contains the element `a` (although this intuition is irrelevant for the
 * task at hand). The type class should be implemented as an abstract trait.
 */

  
//Thougts while implementation for my own sake:
//I am not in an Object, so remember to not indent stuff, to get it to compile.
//I need an abstract trait Member[F[+_]] that has a method "contains"
//With the intuition given: 'that we should check "fa" contains element "a" '
//i am going to implement a given instance with List, that simply uses
// Standard library .contains function.
//I then need an extension function that uses the generic F[_], but uses summon
// To get Member[List], 
// and then simply pass my "fa" and "a" to its .contains method.
//Which in this case is the ordinary List.contains method.
trait Member[F[+_]]:
  def contains[A](fa: F[A], a: A): Boolean

given Member[List] with
  def contains[A](fa: List[A], a: A): Boolean = fa.contains(a)

extension [F[+_], A](fa: F[A])(using memberList: Member[F])
  def contains(a: A): Boolean = memberList.contains(fa, a)


/* QUESTION 11 ########################################################
 *
 * Read the following interface extracted from a railway ticketing system.
 * The question is formulated underneath.
 *
 * The train reservation system accepts payments and creates reservations.
 * Each of the four methods is commented below.  We assume this interface is
 * imperative, so most of the functions have side effects. But this does not
 * matter for the questions below.
 **/

object Trains :
  trait ReservationSystem:

    // Return paymentId if successfully charged the amount; otherwise error
    def pay (CreditCard: String, amount: Int): Either[String, String]

    // Create a reservation, returns a ticket number if successful, or an error
    def reserve (passenger: String, train: String, paymentId: String)
      : Either[String, String]

    // Confirms the validity of the payment with a broker.
    // True if the paymentId is valid
    def validate (paymentId: String): Boolean

    // Returns a set of passengers on the train (a manifest)
    def paxOnTrain (train: String): Set[String]


object FullyAbstractTrains:

  /* Design a fully abstract version of the ReservationSystem interface
   * shown above. In particular abstract away from the details of
   * representation of credit cards, amounts, error messages,passanger
   * names, train numbers, ticket numbers, and payment ids. The idea is not
   * to use String and Int types as representations in the fully abstract
   * version. Either and Boolean are still fine to use, as they do not
   * represent data here.
   *
   * Because we may be using a distributed data store, we want to abstract
   * away from the representation of sets as query results (So abstract away
   * `Set[_]` as well. Assume though that whatever representation we use for
   * query results, it is a Monad, so that map and flatMap are available,
   * and that we can check whether query results contain an element. The
   * latter requires using the solution of Q10.
   */

  //Thougts while implementing
  //I will simply add entries for each parameter from above.
  //So CreditCard: String becomes CreditCard: CreditCard
  //Some of them are reused, 
  //such as Reserve has parameter "Train" and so does PaxOnTrain.

  trait ReservationSystem[
    //Pay method: 
    CreditCard, 
    Amount, 
    PaymentError,
    //reserve method:
    Passenger, 
    Train,     //Also for paxOnTrain
    PaymentId, //also for validate
    TicketNumber,
    ReserveError, 
    //paxOnTrain method:
    //instead of a Set[string]
    PassengersOnTheTrain[Passenger]]:

    //I am going to keep Left as error and Right as the correct result.
    def pay (CreditCard: CreditCard, amount: Amount): Either[PaymentError, PaymentId]

    def reserve (passenger: Passenger, train: Train, paymentId: PaymentId)
      : Either[ReserveError, TicketNumber]

    def validate (paymentId: PaymentId): Boolean

    def paxOnTrain (train: Train): PassengersOnTheTrain[Passenger]

    /* QUESTION 12 ######################################################
     *
     * We want to write some property laws for the fully abstract version of
     * the train reservation system. These tests we cannot run before the
     * implementation is concrete. But they should compile, to support
     * test-first development.
     *
     * Note that this question depends on Q10-11. There are two laws to be
     * written below.
     */

    /* Law 1. A succesful Payment produces a valid PaymentId. Note that both
     * laws have to be members in your abstract version of the train
     * reservation system, so you may need to adjust indentation here to be
     * inside the trait above.
     **/

    def law1: Prop = ???
      //This is where i am stuck :(. 
      //I am unsure how to summon my new ReservationSystem types.
      //I am going to implement it how i think it should look like,
      //but outcomment it, as it currently won't compile because
      //no given instances is made for any of the types i defined.
      // forAll { (card: CreditCard, amount: Amount) =>
      //   val paymentEitherErrorOrId = pay(card, amount)
      //   paymentEitherErrorOrId match
      //     case Right(paymentId) => 
      //       val IsPaymentValid = validate(paymentId)
      //       IsPaymentValid
      //     case Left(error) => 
      //       if (error == PaymentError) then false else true
      //   }

    /* Law 2. A succesful reservation puts the passenger on the requested
     * train (relates `reserve` with `paxOnTrain`). If `reserve` succeeds
     * then paxOnTrain returns a result containing the passenger.)
     */

    def law2: Prop = ???
      //Same as above. I am stuck at law1, with same problem here.
      //I am unsure how to summon my new types.
      //I will outcomment so it compiles. But write my guess as a comment.
      //I need to reserve a ticket.
      //If that ticket is already given to some other passenger on the train
      //Then it should be false. Otherwise it should be true.
      //I need the monad from Q10, to be able to check passengers.
      //Right now we only have Member[List], 
      // I need some kind of Member[PassengersOnTheTrain]
      //The assignment also says the reservation should succeed
      //So i need a case, where if the reservation fails, test should fail.

      //forAll { (passenger: Passenger, train: Train, paymentId: PaymentId) =>
      //  val reservationEitherErrorOrTicket: Either[ReserveError, TicketNumber] = 
      //    reserve(passenger, train, paymentId)
      //  
      //  reservationEitherErrorOrTicket match
      //    case Right(ticketNumber) =>
      //      val passengersOnTrain: PassengersOnTheTrain[Passenger] = paxOnTrain(train)
      //      passengersOnTrain.contains(passenger)
//
      //    case Left(error) =>
      //      if (error == ReserveError) then false else true
      //}


end FullyAbstractTrains

// vim:tw=76:cc=70
