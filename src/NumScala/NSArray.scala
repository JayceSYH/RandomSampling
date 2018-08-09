package NumScala

class NSArray[T : Manifest](constructContent: T*)(implicit nev: Numeric[T]) extends Seq[T] {
  // import operators
  import nev._

  // base content
  private val content = Array[T](constructContent: _*)
  var length : Int = constructContent.length

  // operators
  def apply(index: Int): T = content(index)
  def apply(startIndex: Integer = null, endIndex: Integer = null): NSArray[T] = {
    if (startIndex == null && endIndex == null)
      NSArray(content)
    else {
      var (si, ei) = (startIndex, endIndex)
      if (startIndex == null)
        si = 0
      else if (endIndex == null)
        ei = length - 1

      if (si <= ei && si >= 0 && ei < length) {
        var retList = List[T]()
        (si.toInt to ei.toInt).foreach(i => retList = retList :+ content(i))
        NSArray(retList)
      }
      else
        NSArray[T](List())
    }
  }

  def update(index: Int, x: T): Unit = content(index) = x

  def +(v: NSArray[T]): NSArray[T] = {
    var retArr = content.clone()
    content.indices.foreach(i => retArr(i) += v(i))
    new NSArray[T](retArr: _*)
  }

  def +(scalar: T): NSArray[T] = {
    var retArr = content.clone()
    content.indices.foreach(i => retArr(i) += scalar)
    new NSArray[T](retArr: _*)
  }

  def -(v: NSArray[T]): NSArray[T] = {
    var retArr = content.clone()
    content.indices.foreach(i => retArr(i) -= v(i))
    new NSArray[T](retArr: _*)
  }

  def -(scalar: T): NSArray[T] = {
    var retArr = content.clone()
    content.indices.foreach(i => retArr(i) -= scalar)
    new NSArray[T](retArr: _*)
  }

  def *(v: NSArray[T]): NSArray[T] = {
    var retArr = content.clone()
    content.indices.foreach(i => retArr(i) *= v(i))
    new NSArray[T](retArr: _*)
  }

  def *(scalar: T): NSArray[T] = {
    var retArr = content.clone()
    content.indices.foreach(i => retArr(i) *= scalar)
    new NSArray[T](retArr: _*)
  }

  def /(v: NSArray[T]): NSArray[Double] = {
    var retArr : Array[Double] = content.map(_.toDouble())
    content.indices.foreach(i => retArr(i) /= v(i).toDouble())

    new NSArray[Double](retArr: _*)
  }

  def /(scalar: T): NSArray[Double] = {
    var retArr : Array[Double] = content.map(_.toDouble())
    content.indices.foreach(i => retArr(i) /= scalar.toDouble())
    new NSArray[Double](retArr: _*)
  }

  def unary_-(): NSArray[T] = new NSArray[T](content.map(nev.negate): _*)

  def abs(): NSArray[T] = new NSArray[T](content.map(nev.abs): _*)

  // NSArray operation
  def concat(arr: NSArray[T]): NSArray[T] = {
    new NSArray[T](content ++ arr.content: _*)
  }

  def append(elements: T*): NSArray[T] = {
    new NSArray[T](content ++ elements: _*)
  }

  def asMatrix(colv: Boolean = true): NSMatrix[T] = {
    if (colv)
      NSMatrix[T](List(this.content.toList)).t()
    else
      NSMatrix[T](List(this.content.toList))
  }

  // etc
  override def toString: String = {
    if (length == 0)
      "[]"
    else if (length == 1)
      "[" + content(0) + "]"
    else
      "[" + content.foldLeft("")(_ + ", " + _).substring(2) + "]"
  }

  // sum min max ...
  def sum(): T = {
    content.tail.foldLeft(content.head)(_ + _)
  }

  def mean(): Double = {
    sum().toDouble() / content.length
  }

  def max(): T = {
    content.tail.foldLeft(content.head)((x, y) => if (x > y) x else y)
  }

  def min(): T = {
    content.tail.foldLeft(content.head)((x, y) => if (x < y) x else y)
  }

  override def iterator: Iterator[T] = {
    content.iterator
  }
}


object NSArray {
  def apply[T : Manifest](constructContent: T*)(implicit nev: Numeric[T]): NSArray[T] = new NSArray(constructContent: _*)
  def apply[T : Manifest](arr: Array[T])(implicit nev: Numeric[T]): NSArray[T] = new NSArray(arr: _*)
  def apply[T : Manifest](lst: List[T])(implicit nev: Numeric[T]): NSArray[T] = new NSArray(lst: _*)
}


object llll {
  def main(arg: Array[String]): Unit = {
    println(NSArray(List[Double]()))
  }
}