class CombinationOfDigitsWithSumk {
  def combinationSum(nums: Array[Int], target: Int): List[List[Int]] = {
    def backtrack(start: Int, target: Int, path: List[Int], result: List[List[Int]]): List[List[Int]] = {
      if (target < 0) {
        Nil
      } else if (target == 0) {
        path.reverse :: result
      } else if (start == nums.length) {
        Nil
      } else {
        backtrack(start + 1, target - nums(start), nums(start) :: path, result) ++ backtrack(start + 1, target, path, result)
      }
    }

    backtrack(0, target, Nil, Nil).distinct
  }
}

object CombinationOfDigitsWithSumk extends App {
  val nums = Array(2, 3, 6, 7)
  val target = 7
  val combinationOfDigitsWithSumk = new CombinationOfDigitsWithSumk
  val result = combinationOfDigitsWithSumk.combinationSum(nums, target)
  println(result)
}
