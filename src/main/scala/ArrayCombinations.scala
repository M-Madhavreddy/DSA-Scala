class ArrayCombinations {

  def findCombinations(arr: Array[Int], n: Int): List[List[Int]] = {

    def Backtracking(index: Int, sum: Int, currentCombination: List[Int], result: List[List[Int]]): List[List[Int]] = {
      if (index == arr.length) {
        if (sum == n) currentCombination.reverse :: result
        else result
      }
      else if (sum > n) {
        result
      }
      else {
        val includingElement = Backtracking(index + 1, sum + arr(index), arr(index) :: currentCombination, result)
        val excludingElement = Backtracking(index + 1, sum, currentCombination, result)
        includingElement ::: excludingElement
      }
    }

    Backtracking(0, 0, List(), List())
  }

}

object Main extends App {

  val arr = Array(1, 2, 3, 4, 5,6)
  val targetSum = 10
  val arrayCombinations = new ArrayCombinations
  val combinations = arrayCombinations.findCombinations(arr, targetSum)
  println("Combinations whose sum is " + targetSum + ":")
  combinations.foreach(println)

}
