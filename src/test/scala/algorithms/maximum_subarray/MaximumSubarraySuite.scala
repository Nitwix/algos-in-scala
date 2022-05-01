// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
import scala.util.Random
class MaximumSubarray extends munit.FunSuite {
  import algorithms.maximum_subarray.MaximumSubarray._
  test("Edge cases are treated correctly") {
    assertEquals(
      maximumSubarray(List.empty),
      ListWithSum(List.empty, List[Int]().sum)
    )
    assertEquals(maximumSubarray(List(1)), ListWithSum(List(1), 1))
  }

  test("Example of figure 4.2 works correctly") {
    val list = List(1, -4, 3, -4)
    assertEquals(maximumSubarray(list), ListWithSum(List(3), 3))
  }

  test("maxLeftAlignedSubarray works on edge cases") {
    assertEquals(
      maxLeftAlignedSubarray(List.empty[Int]),
      ListWithSum(List.empty[Int], 0)
    )
  }

  test("maxLeftAlignedSubarray works on normal cases") {
    val list = List(1, -3, 4)
    assertEquals(maxLeftAlignedSubarray(list), ListWithSum(list, list.sum))

    val list2 = List(1, -3, 2)
    assertEquals(maxLeftAlignedSubarray(list2), ListWithSum(List(1), 1))
  }

  test("maxRightAlignedSubarray works on normal cases") {
    val list = List(4, -3, 1)
    assertEquals(maxRightAlignedSubarray(list), ListWithSum(list, list.sum))

    val list2 = List(2, -3, 1)
    assertEquals(maxRightAlignedSubarray(list2), ListWithSum(List(1), 1))
  }

  test("Example of Figure 4.3 works") {
    val list =
      List(13, -3, -25, 20, -3, -16, -23, 18, 20, -7, 12, -5, -22, 15, -4, 7)
    val maxSubarray = list.slice(7, 11)
    assertEquals(maximumSubarray(list), ListWithSum(maxSubarray, maxSubarray.sum))
  }

  test("Max subarray of random array"){
    val list = (1 to 100).map(n => Random.between(-10, 11)).toList
    println("Original list:")
    println(list)
    println("Max subarray:")
    println(maximumSubarray(list).list)

  }
}
