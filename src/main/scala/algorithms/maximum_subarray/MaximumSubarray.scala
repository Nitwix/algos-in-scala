package algorithms.maximum_subarray

object MaximumSubarray {
  case class ListWithSum(list: List[Int], sum: Int)

  /** Returns the subarray of the given array whose sum is maximal.
    * @param xs
    *   input array
    * @return
    *   maximum subarray of the input array
    */
  def maximumSubarray(xs: List[Int]): ListWithSum =
    if xs.length <= 150 then 
      bruteforceMaximumSubarray(xs)
    else
      // mid is the index of the element just after the middle of the list
      val mid = xs.length / 2
      val (leftSA, rightSA) = xs.splitAt(mid)
      val leftMaxSA = maximumSubarray(leftSA)
      val rightMaxSA = maximumSubarray(rightSA)
      val maxCrossingSA = maximumCrossingSA(leftSA, rightSA)

      if leftMaxSA.sum >= rightMaxSA.sum &&
        leftMaxSA.sum >= maxCrossingSA.sum
      then leftMaxSA
      else if rightMaxSA.sum >= leftMaxSA.sum &&
        rightMaxSA.sum >= maxCrossingSA.sum
      then rightMaxSA
      else maxCrossingSA

  /** Returns the maximum crossing subarray, given the left and right array
   * @param leftSA left subarray
   * @param rightSA right subarray
   * @return the subarray crossing the left and right arrays that has a maximal sum
   */
  def maximumCrossingSA(leftSA: List[Int], rightSA: List[Int]): ListWithSum =
    val maxLeftSA = maxRightAlignedSubarray(leftSA)
    val maxRightSA = maxLeftAlignedSubarray(rightSA)
    val maxCrossingSA: List[Int] = maxLeftSA.list ++ maxRightSA.list
    val sumMaxCrossingSA = maxLeftSA.sum + maxRightSA.sum
    ListWithSum(maxCrossingSA, sumMaxCrossingSA)

  /** Returns the maximum subarray that is left aligned of the given array
   * For example:
   *   List(1, -3, 4) -> List(1, -3, 4) but
   *   List(1, -3, 2) -> List(1)
   * @param rightSA the given array
   */
  def maxLeftAlignedSubarray(rightSA: List[Int]): ListWithSum = 
    if rightSA.isEmpty then
      ListWithSum(List.empty[Int], 0)
    else
      val sublists = (1 to rightSA.length).map(n => {
        val sublist = rightSA.take(n)
        ListWithSum(sublist, sublist.sum)
      })
      sublists.maxBy(_.sum)

  /** Returns the maximum right aligned subarray of the given array
   * For example:
   *    List(4, -3, 1) -> List(4, -3, 1) but
   *    List(2, -3, 1) -> List(1)
   * @param leftSA given array
   */
  def maxRightAlignedSubarray(leftSA: List[Int]): ListWithSum = 
    val reversedMax = maxLeftAlignedSubarray(leftSA.reverse)
    ListWithSum(reversedMax.list.reverse, reversedMax.sum)
  
  def bruteforceMaximumSubarray(xs: List[Int]): ListWithSum =
    var maxSum = Integer.MIN_VALUE
    var (startIndexOfMax, endIndexOfMax) = (0,0)
    for(startIndex <- 0 until xs.length){
      for(endIndex <- startIndex until xs.length){
        val subarray = xs.slice(startIndex, endIndex+1)
        if subarray.sum > maxSum then
          maxSum = subarray.sum
          startIndexOfMax = startIndex
          endIndexOfMax = endIndex
      }
    }
    ListWithSum(xs.slice(startIndexOfMax, endIndexOfMax+1), maxSum)
}
