import scala.reflect.ClassTag

object MergeSort {
  def mergeSort[T: ClassTag](arr: Array[T])(implicit ord: Ordering[T]): Array[T] = {
    def merge(left: Array[T], right: Array[T]): Array[T] = {
      var mergedArray = Array.empty[T]
      var (leftIndex, rightIndex) = (0, 0)

      while (leftIndex < left.length && rightIndex < right.length) {
        if (ord.lteq(left(leftIndex), right(rightIndex))) {
          mergedArray :+= left(leftIndex)
          leftIndex += 1
        } else {
          mergedArray :+= right(rightIndex)
          rightIndex += 1
        }
      }

      if (leftIndex < left.length) mergedArray ++= left.slice(leftIndex, left.length)
      if (rightIndex < right.length) mergedArray ++= right.slice(rightIndex, right.length)

      mergedArray
    }

    val n = arr.length
    if (n <= 1) arr
    else {
      val middle = n / 2
      val (left, right) = arr.splitAt(middle)
      merge(mergeSort(left), mergeSort(right))
    }
  }

  def main(args: Array[String]): Unit = {
    val unsortedArray = Array(64, 34, 25, 12, 22, 11, 90)
    val sortedArray = mergeSort(unsortedArray)
    println(s"Sorted Array: ${sortedArray.mkString(", ")}")
  }
}

