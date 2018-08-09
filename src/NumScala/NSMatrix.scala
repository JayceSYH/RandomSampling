package NumScala


trait BaseMatrix[T] {
  def apply(row: Int, col: Int) : T
  val shape: Array[Int]

  override def toString: String = {
    if (shape(0) * shape(1) == 0)
      "[[]]"
    else {
      var retStr = "["
      for (i <- 0 until shape(0)) {
        if (i == 0)
          retStr += "["
        else
          retStr += " ["
        (0 until shape(1)).foreach(j => retStr = retStr + this (i, j) + ", ")
        retStr = retStr.substring(0, retStr.length - 2) + "]\n"
      }
      retStr.substring(0, retStr.length - 1) + "]"
    }
  }

  def toNSArray: NSArray[T]

  // sum min max ...
  def sum(): T

  def mean(): Double

  def max(): T

  def min(): T
}

class NSMatrix[T : Manifest](constructArray: Array[T], rows: Int, cols: Int)(implicit nev: Numeric[T]) extends BaseMatrix[T] {
  // import operators
  import nev._

  // validate matrix format
  private val validation = {
    if (constructArray.length != rows * cols)
      throw new Exception("NSMatrix size not match parameter")

    true
  }

  // base content
  private val content = constructArray.clone()

  val shape: Array[Int] = Array(rows, cols)

  // operators
  def apply(row: Int, col: Int): T = content(row * shape(1) + col)

  def apply(row: Integer, col: Integer): NSArray[T] = {
    if (row == null && col == null)
      throw new Exception("one of row and col must be specified")
    else if (row == null)
      sub()(col).toNSArray
    else
      sub(row)().toNSArray
  }

  def update(row: Int, col: Int, x: T): Unit = content(row * shape(1) + col) = x
  def update(row: Integer, col: Integer, v: Seq[T]): Unit = {
    if (row == null && col == null)
      throw new Exception("one of row and col must be specified")
    else if (row == null) {
      if (shape(0) != v.length)
        throw new Exception("length not match")
      else
        (0 until shape(0)).foreach(i => this(i, col) = v(i))
    }
    else if (col == null) {
      if (shape(1) != v.length)
        throw new Exception("length not match")
      else
        (0 until shape(1)).foreach(i => this(row, i) = v(i))
    }
  }

  def +(v: NSMatrix[T]): NSMatrix[T] = {
    assert(shape sameElements v.shape , "Matrix shape not match")
    var retMatrix = content.clone()
    content.indices.foreach(i => retMatrix(i) += v.content(i))
    NSMatrix[T](retMatrix, rows, cols)
  }

  def +[A](v: NSMatrix[A])(implicit nev1: Numeric[A]): NSMatrix[Double] = {
    assert(shape sameElements v.shape , "Matrix shape not match")
    var retMatrix : Array[Double] = content.map(_.toDouble())
    content.indices.foreach(i => retMatrix(i) += nev1.toDouble(v.content(i)))
    NSMatrix[Double](retMatrix, rows, cols)
  }

  def +(scalar: T): NSMatrix[T] = {
    var retMatrix = content.clone()
    content.indices.foreach(i => retMatrix(i) += scalar)
    NSMatrix[T](retMatrix, rows, cols)
  }

  def -(v: NSMatrix[T]): NSMatrix[T] = {
    assert(shape sameElements v.shape , "Matrix shape not match")
    var retMatrix = content.clone()
    content.indices.foreach(i => retMatrix(i) -= v.content(i))
    NSMatrix[T](retMatrix, rows, cols)
  }

  def -[A](v: NSMatrix[A])(implicit nev1: Numeric[A]): NSMatrix[Double] = {
    assert(shape sameElements v.shape , "Matrix shape not match")
    var retMatrix : Array[Double] = content.map(_.toDouble())
    content.indices.foreach(i => retMatrix(i) -= nev1.toDouble(v.content(i)))
    NSMatrix[Double](retMatrix, rows, cols)
  }

  def -(scalar: T): NSMatrix[T] = {
    var retMatrix = content.clone()
    content.indices.foreach(i => retMatrix(i) -= scalar)
    NSMatrix[T](retMatrix, rows, cols)
  }

  def *(v: NSMatrix[T]): NSMatrix[T] = {
    assert(shape(1) == v.shape(0) , "Matrix shape not match")
    var retMatrix = NSMatrix.zeroMatrix[T](shape(0), v.shape(1))
    for (row <- 0 until retMatrix.shape(0))
      for (col <- 0 until retMatrix.shape(1))
        retMatrix(row, col) = {var sum = 0.0.asInstanceOf[T]; (0 until shape(1)).foreach(i => sum = sum + (this(row, i) * v(i, col))); sum}
    retMatrix
  }

  def *[A](v: NSMatrix[A])(implicit nev1: Numeric[A]): NSMatrix[Double] = {
    assert(shape(1) == v.shape(0) , "Matrix shape not match")
    var retMatrix = NSMatrix.zeroMatrix[Double](shape(0), v.shape(1))
    for (row <- 0 until retMatrix.shape(0))
      for (col <- 0 until retMatrix.shape(1))
        retMatrix(row, col) = {var sum = 0.0; (0 until shape(1)).foreach(i => sum = sum + this(row, i).toDouble() * nev1.toDouble(v(i, col))); sum}
    retMatrix
  }

  def *(scalar: T): NSMatrix[T] = {
    var retMatrix = content.clone()
    content.indices.foreach(i => retMatrix(i) *= scalar)
    NSMatrix[T](retMatrix, rows, cols)
  }

  def /(scalar: T): NSMatrix[Double] = {
    var retMatrix : Array[Double] = content.map(_.toDouble())
    content.indices.foreach(i => retMatrix(i) /= scalar.toDouble())
    NSMatrix[Double](retMatrix, rows, cols)
  }

  def unary_-(): NSArray[T] = new NSArray[T](content.map(nev.negate): _*)

  def abs(): NSArray[T] = new NSArray[T](content.map(nev.abs): _*)

  def print(): Unit = {
    (0 until shape(0)).foreach(i => {(0 until shape(1)).foreach(j => printf("%s ", this(i, j))); println()})
  }

  def asDouble(): NSMatrix[Double] = {
    NSMatrix[Double](content.map(_.toDouble()), rows, cols)
  }

  def asFloat(): NSMatrix[Float] = {
    NSMatrix[Float](content.map(_.toFloat()), rows, cols)
  }

  def asLong(): NSMatrix[Long] = {
    NSMatrix[Long](content.map(_.toLong()), rows, cols)
  }

  def asInt(): NSMatrix[Int] = {
    NSMatrix[Int](content.map(_.toInt()), rows, cols)
  }

  override def toNSArray: NSArray[T] = {
    NSArray(content)
  }

  def t(): NSMatrix[T] = {
    val retMatrix = NSMatrix(content, cols, rows)
    (0 until shape(1)).foreach(i => (0 until shape(0)).foreach(j => retMatrix(i, j) = this(j, i)))
    retMatrix
  }

  def inv(): NSMatrix[Double] = {
    assert(shape(0) == shape(1), "matrix is not squared")

    // prepare matrix
    val retMatrix = NSMatrix.zeroMatrix[Double](shape(0), shape(0) * 2)
    (0 until shape(0)).foreach(i => retMatrix(null, i) = sub()(i).asDouble().toNSArray)
    (shape(0) until shape(0) * 2).foreach(i => retMatrix(i - shape(0), i) = 1.0)

    // G-J method
    for (i <- 0 until shape(0)) {
      if (retMatrix(i, i) == 0.0) {
        for (j <- i + 1 until shape(0))
          if (retMatrix(j, i) != 0) {
            val exchange = (retMatrix(i, null), retMatrix(j, null))
            retMatrix(i, null) = exchange._2
            retMatrix(j, null) = exchange._1
          }

        if (retMatrix(i, i) == 0)
          throw new Exception("matrix is singular")
      }

      val stdRow = retMatrix.sub(i)().toNSArray / retMatrix(i, i)
      retMatrix(i, null) = stdRow
      for (j <- 0 until shape(0)) {
        if (i != j) {
          retMatrix(j, null) = retMatrix(j, null) - stdRow * retMatrix(j, i)
          retMatrix(j, i) = 0
        }
      }
    }

    retMatrix.sub()(shape(0), shape(0) * 2 - 1)
  }

  def sub(startRow: Integer = null, endRow: Integer = null)(startCol: Integer = null, endCol: Integer = null): NSMatrix[T] = {
    val sr: Int = if (startRow == null) 0 else startRow
    var er: Int = if (endRow == null) {if (startRow == null) shape(0) - 1 else startRow} else endRow
    val sc: Int = if (startCol == null) 0 else startCol
    var ec: Int = if (endCol == null) {if (startCol == null) shape(1) - 1 else startCol} else endCol

    if (!(sr >= 0 && sr <= er && er < rows && sc >= 0 && sc <= ec && ec < cols))
      NSMatrix(List(), 0, 0)
    else {
      var subList = List[T]()
      (sr to er).foreach(i => (sc to ec).foreach(j => subList = subList :+ this (i, j)))

      NSMatrix(subList, er - sr + 1, ec - sc + 1)
    }
  }

  def subRef(startRow: Integer = null, endRow: Integer = null)(startCol: Integer = null, endCol: Integer = null): MatrixRef[T] = {
    val sr: Int = if (startRow == null) 0 else startRow
    var er: Int = if (endRow == null) {if (startRow == null) shape(0) - 1 else startRow} else endRow
    val sc: Int = if (startCol == null) 0 else startCol
    var ec: Int = if (endCol == null) {if (startCol == null) shape(1) - 1 else startCol} else endCol

    if (!(sr >= 0 && sr <= er && er < rows && sc >= 0 && sc <= ec && ec < cols))
      new MatrixRef[T](this, 0, 0 ,(x, y) => (0 ,0))
    else {
      new MatrixRef[T](this, er - sr + 1, ec - sc + 1, (x, y) => (x + sr, y + sc))
    }
  }

  def det(): Double = {
    if (shape(0) != shape(1))
     throw new Exception("matrix is not squared")

    val border = shape(0)
    def recurDet(matrix: BaseMatrix[T]): Double = {
      if (matrix.shape(0) == 2)
        (matrix(0, 0) * matrix(1, 1) - matrix(0, 1) * matrix(1, 0)).toDouble()
      else {
          var retVal = 0.0
          (0 until matrix.shape(1)).foreach(i => retVal += (if (i % 2 ==0) 1 else -1) * matrix(i, 0).toDouble() *
            recurDet(new MatrixRef[T](matrix, matrix.shape(0) - 1, matrix.shape(1) - 1,
              (x, y) => (if (x < i) x else x + 1, y + 1))))
          retVal
      }
    }

    recurDet(this)
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

  def horiConcat(matrix: NSMatrix[T]): NSMatrix[T] = {
    if (0 == matrix.shape(0) * matrix.shape(1))
      this
    else if (0 == shape(0) * shape(1))
      matrix
    else {
      if (shape(0) != matrix.shape(0))
        throw new Exception("matrixes has different rows")

      val retMatrix = NSMatrix.zeroMatrix(shape(0), shape(1) + matrix.shape(1))
      (0 until shape(1) + matrix.shape(1)).foreach(i => {
        if (i < shape(1)) {
          retMatrix(null, i) = this (null, i)
        } else {
          retMatrix(null, i) = matrix(null, i)
        }
      })
      retMatrix
    }
  }

  def vertConcat(matrix: NSMatrix[T]): NSMatrix[T] = {
    if (0 == matrix.shape(0) * matrix.shape(1))
      this
    else if (0 == shape(0) * shape(1))
      matrix
    else {
      if (shape(1) != matrix.shape(1))
        throw new Exception("matrixes has different rows")

      val retMatrix = NSMatrix.zeroMatrix(shape(0) + matrix.shape(0), shape(1))
      (0 until shape(0) + matrix.shape(0)).foreach(i => {
        if (i < shape(0)) {
          retMatrix(i, null) = this (i, null)
        } else {
          retMatrix(i, null) = matrix(i, null)
        }
      })
      retMatrix
    }
  }
}


class MatrixRef[T : Manifest](matrix: BaseMatrix[T], rows: Int, cols: Int, mapping: (Int, Int)=>(Int, Int))(implicit nev: Numeric[T]) extends BaseMatrix[T] {
  def apply(row: Int, col: Int): T = {
    val (or, oc) = mapping(row, col)
    matrix(or, oc)
  }

  override def toNSArray: NSArray[T] = {
    NSArray((0 until rows).foldLeft(List[T]())((lst, i) => (0 until cols).foldLeft(lst)((lst1, j) => lst1 :+ matrix(i, j))))
  }

  // sum min max ...
  def sum(): T = {
    var s = 0.asInstanceOf[T]
    (0 until shape(0)).foreach(i => (0 until shape(1)).foreach(j => s = nev.plus(s, this(i, j))))
    s
  }

  def mean(): Double = {
    nev.toDouble(sum()) / (shape(0) * shape(1))
  }

  def max(): T = {
    var m = this(0, 0)
    (0 until shape(0)).foreach(i => (0 until shape(1)).foreach(j => m = nev.max(m, this(i, j))))
    m
  }

  def min(): T = {
    var m = this(0, 0)
    (0 until shape(0)).foreach(i => (0 until shape(1)).foreach(j => m = nev.min(m, this(i, j))))
    m
  }

  override val shape: Array[Int] = Array(rows, cols)
}


object NSMatrix {
  def apply[T : Manifest](constructArray: Array[T], rows: Int, cols: Int)(implicit nev: Numeric[T]): NSMatrix[T] = new NSMatrix(constructArray, rows, cols)
  def apply[T : Manifest](constructList: Seq[T], rows: Int, cols: Int)(implicit nev: Numeric[T]): NSMatrix[T] = new NSMatrix(Array(constructList: _*), rows, cols)
  def apply[T : Manifest](constructList: Seq[Seq[T]])(implicit nev: Numeric[T]): NSMatrix[T] = {
    if (constructList == null || constructList == Nil || constructList.isEmpty || constructList.head.isEmpty)
      new NSMatrix[T](new Array(0), 0, 0)
    else {
      var isMatrix = true
      var colLength = constructList.head.length
      var i = 0

      while (i < constructList.length && isMatrix) {
        isMatrix = colLength == constructList(i).length
        i += 1
      }

      if (!isMatrix)
        throw new Exception("NSMatrix has rows with different length")

      new NSMatrix(Array(constructList.flatten: _*), constructList.length, constructList.head.length)
    }
  }

  def zeroMatrix[T : Manifest](rows: Int, cols: Int)(implicit  nev: Numeric[T]): NSMatrix[T] = {
    new NSMatrix[T](new Array[T](rows * cols), rows, cols)
  }
}


object lll {
  def main(arg:Array[String]): Unit = {
    var a = NSMatrix(List(List(1,2,4), List(5,6,7), List(8,9,10)))
    var b = NSMatrix.zeroMatrix[Double](5, 6)
    var c = NSMatrix(List(List(1,1,1), List(1,2,3), List(1,5,1)))
    println(c)
    println(c.sub(3, null)(3, null).horiConcat(c.sub(1, 2)(1,2)))
  }
}