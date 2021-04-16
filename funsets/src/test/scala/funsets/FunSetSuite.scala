package funsets

import org.junit._

import scala.math.abs

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite {

  import FunSets._

  @Test def `contains is implemented`: Unit = {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  trait FunctionalTestSets {
    def evens (x : Int) : Boolean = x % 2 == 0
    def odds (x : Int) : Boolean = abs(x % 2) == 1
  }

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remvoe the
   * @Ignore annotation.
   */
  @Test def `singleton set one contains one`: Unit = {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  @Test def `union contains all elements of each set`: Unit = {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  @Test def `forall test`: Unit = {
    new FunctionalTestSets {
      assert(forall(evens, x => x % 2 == 0))
      assert(forall(odds, x => abs(x % 2) == 1))
    }
  }

  @Test def `evens/odds contains test`: Unit = {
    new FunctionalTestSets {
      assert(contains(evens, 0))
      assert(contains(evens, 2))
      assert(contains(odds, 1))
      assert(contains(odds, -1))
    }
  }

  @Test def `evens contain divisible by 10, not contain odds`: Unit = {
    new FunctionalTestSets {
      assert(exists(evens, x => x % 10 == 0))
      assert(!exists(evens, x => x % 2 == 1))
    }
  }

  @Test def `odds contain divisible by 5, not contain divisible by 10`: Unit = {
    new FunctionalTestSets {
      assert(exists(odds, x => x % 5 == 0))
      assert(!exists(odds, x => x % 10 == 0))
    }
  }

  @Test def `evens and odds intersection is empty`: Unit = {
    new FunctionalTestSets {
      assert(
        !exists(intersect(evens, odds),
          _ => true))
    }
  }



  @Test def `map function test for mapping each even number to uneven number`: Unit = {
    def evens (x : Int) : Boolean = x % 2 == 0

    def inc (x : Int) = x + 1

    val set = map(evens, inc)


    assert(contains(set, 1))
    assert(contains(set, -1))
    assert(forall(set, value => abs(value % 2) == 1))
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
