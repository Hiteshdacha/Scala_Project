import scala.reflect.ClassTag

object QuickSort {
  def sort[T: ClassTag](arr: Array[T])(implicit ord: Ordering[T]): Array[T] = {
    if (arr.length <= 1) arr
    else {
      val pivot = arr(arr.length / 2)
      Array.concat(
        sort(arr.filter(ord.lt(_, pivot))),
        arr.filter(ord.equiv(_, pivot)),
        sort(arr.filter(ord.gt(_, pivot)))
      )
    }
  }

  def main(args: Array[String]): Unit = {
    val unsortedArray = Array(64, 34, 25, 12, 22, 11, 90)
    val sortedArray = sort(unsortedArray)
    println(s"Sorted Array: ${sortedArray.mkString(", ")}")
  }
}

3(Bubblesort)

object BubbleSort {
  def sort[T](arr: Array[T])(implicit ord: Ordering[T]): Array[T] = {
    val n = arr.length
    for (i <- 0 until n - 1; j <- 0 until n - i - 1) {
      if (ord.gt(arr(j), arr(j + 1))) {
        // Swap elements if they are in the wrong order
        val temp = arr(j)
        arr(j) = arr(j + 1)
        arr(j + 1) = temp
      }
    }
    arr
  }

  def main(args: Array[String]): Unit = {
    val unsortedArray = Array(64, 34, 25, 12, 22, 11, 90)
    val sortedArray = sort(unsortedArray)
    println(s"Sorted Array: ${sortedArray.mkString(", ")}")
  }
}

4(Linkedlist)

class Node[T](val value: T, var next: Option[Node[T]] = None)

class LinkedList[T] {
  var head: Option[Node[T]] = None

  def append(value: T): Unit = {
    val newNode = new Node(value)
    if (head.isEmpty) {
      head = Some(newNode)
    } else {
      var current = head
      while (current.get.next.isDefined) {
        current = current.get.next
      }
      current.get.next = Some(newNode)
    }
  }

  def printList(): Unit = {
    var current = head
    while (current.isDefined) {
      print(s"${current.get.value} -> ")
      current = current.get.next
    }
    println("null")
  }
}

// Example usage
val myList = new LinkedList[Int]
myList.append(1)
myList.append(2)
myList.append(3)

myList.printList()
