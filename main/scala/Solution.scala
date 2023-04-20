import util.{Pixel, Util}

import java.awt.Dimension

// Online viewer: https://0xc0de.fr/webppm/
object Solution {
  type Image = List[List[Pixel]]
  type GrayscaleImage = List[List[Double]]

  // prerequisites
  def fromStringPPM(image: List[Char]): Image = {
    val IntList = image.drop(3).mkString("").split("[\\s\n]+").map(_.toInt).toList
    val length = IntList(0)
    val height = IntList(1)

    def foldLeft(list: List[Int], i: Image, dimension: Integer)(f: (Integer, Pixel, Image) => Image): Image = {
      def aux(acc: Image, list: List[Int], dim: Integer): Image = {
        list match {
          case Nil => acc
          case x :: xs =>
            if (dim == length)
              aux(f(dim, Pixel(list(2), list(1), list(0)), acc), list.drop(3), 1)
            else
              aux(f(dim, Pixel(list(2), list(1), list(0)), acc), list.drop(3), dim + 1)
        }
      }

      aux(i, list, dimension)
    }

    def makeImage(dimension: Integer, pixel: Pixel, acc: Image): Image = {
      acc match {
        case Nil => List(List(pixel))
        case x :: xs =>
          if (dimension == length) {
            (pixel :: Nil) :: acc
          } else {
            (pixel :: x) :: xs
          }
      }
    }

    foldLeft(IntList.drop(3).reverse, Nil: Image, 0)(makeImage)
  }

  def toStringPPM(image: Image): List[Char] = {
    val dimension = image.flatten.size
    val height = image.size
    val length = (dimension / height)

    val line1 = List[Char]('P', '3', '\n')
    val line2 = s"$length $height\n".toList
    val line3 = List[Char]('2', '5', '5', '\n')

    val pixels = image.view.flatMap {
      list =>
        list.flatMap { pixel =>
          pixel match {
            case Pixel(r, g, b) =>
              s"$r $g $b\n"
          }
        }
    }

    val imagePPM = line1 ++ line2 ++ line3 ++ pixels
    imagePPM
  }

  // ex 1
  def verticalConcat(image1: Image, image2: Image): Image = {
    image1 ++ image2
  }

  // ex 2
  def horizontalConcat(image1: Image, image2: Image): Image = {
    image1.zip(image2).flatMap {
      case (innerList1, innerList2) => List(innerList1 ++ innerList2)
    }
  }

  // ex 3
  def rotate(image: Image, degrees: Integer): Image = {
    if (degrees == 90)
      image.transpose.reverse
    else if (degrees == 180)
      image.map(_.reverse).reverse
    else if (degrees == 270)
      image.transpose.map(_.reverse)
    else
      image

  }

  // ex 4
  val gaussianBlurKernel: GrayscaleImage = List[List[Double]](
    List(1, 4, 7, 4, 1),
    List(4, 16, 26, 16, 4),
    List(7, 26, 41, 26, 7),
    List(4, 16, 26, 16, 4),
    List(1, 4, 7, 4, 1)
  ).map(_.map(_ / 273))

  val Gx: GrayscaleImage = List(
    List(-1, 0, 1),
    List(-2, 0, 2),
    List(-1, 0, 1)
  )

  val Gy: GrayscaleImage = List(
    List(1, 2, 1),
    List(0, 0, 0),
    List(-1, -2, -1)
  )

    def sum(a: Double, b: Double): Double = a + b

    def edgeDetection(image: Image, threshold : Double): Image = {
      val matrixGray = image.map(_.map(Util.toGrayScale))
      val matrixBlurr = applyConvolution(matrixGray, gaussianBlurKernel)
      applyConvolution(matrixBlurr, Gx)
        .zip(applyConvolution(matrixBlurr, Gy))
        .map {
        case(row1, row2) =>
          row1.zip(row2).map(
            pair =>
              if (pair._1 < 0)
                if (pair._2 < 0)
                  pair._1 * -1 + pair._2 * -1
                else
                  pair._1 * -1 + pair._2
              else if (pair._2 < 0)
                pair._1 + pair._2 * -1
              else
                pair._1 + pair._2
          )
      }.map(_.map(
        (a) =>
          if (a <= threshold)
            Pixel(0, 0, 0)
          else
            Pixel(255, 255, 255)
        )
      )
    }

    def applyConvolution(image: GrayscaleImage, kernel: GrayscaleImage) : GrayscaleImage = {
      def convol(m1: GrayscaleImage, m2: GrayscaleImage): Double =
        m1.zip(m2).map(
          pair => pair._1.zip(pair._2).map(
            pair => pair._1 * pair._2
          ).foldRight(0: Double)(sum)
        ).foldRight(0: Double)(sum)

      Util.getNeighbors(image, kernel.size / 2).map(_.map(convol(_, kernel)))
    }

  // ex 5
  def moduloPascal(m: Integer, funct: Integer => Pixel, size: Integer): Image = {
    def pascalsTriangle(n: Int): List[List[Int]] = {
      if (n == 1) List(List(1))
      else {
        val previousTriangle = pascalsTriangle(n - 1)
        val previousRow = previousTriangle.last.padTo(previousTriangle.size, 0)
        val newRow = (0 :: previousRow).zip (previousRow :+ 0).map { case (a, b) => (a + b) % m }
        previousTriangle :+ newRow
      }
    }

    val triangle = pascalsTriangle(size).map(
      subList => subList.padTo(size, -1)
    )

    triangle.map(_.map(funct(_)))
  }
}
