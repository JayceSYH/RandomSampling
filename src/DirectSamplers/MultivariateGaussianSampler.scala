package DirectSamplers

import java.io.PrintWriter

import Common.{OneDimSampler, Sampler}
import NumScala.{NSArray, NSMatrix}
import RandomSampling.GibbsSampler

class MultivariateGaussianSampler(u: NSArray[Double], sigma: NSMatrix[Double]) extends Sampler {
  override val pdf: (Double*) => Double = gaussian _
  override val nDim: Int = u.length
  private val gibbsSampler = new GibbsSampler(gaussian _, nDim, conditionalDistributionSamplerGenerator _)

  def conditionalDistributionSamplerGenerator(axis: Int, xs: Double*): OneDimSampler = {
    val sigma11 = sigma.sub(axis, axis)(axis, axis)
    val sigma22 = sigma.sub(null, axis - 1)(null, axis - 1).horiConcat(sigma.sub(null, axis - 1)(axis + 1, null)).vertConcat(
      sigma.sub(axis + 1, null)(null, axis - 1).horiConcat(sigma.sub(axis + 1, null)(axis + 1, null)))
    val sigma12 = sigma.sub(axis, axis)(null, axis - 1).horiConcat(sigma.sub(axis, axis)(axis + 1, null))
    val sigma21 = sigma.sub(null, axis - 1)(axis, axis).vertConcat(sigma.sub(axis + 1, null)(axis, axis))

    val u1 = u(axis, axis)
    val u2 = u(null, axis - 1).concat(u(axis + 1, null))

    val vx = NSArray(xs: _*)
    val x2 = vx(null, axis - 1).concat(vx(axis  + 1, null))

    val u1c2 = (u1.asMatrix() + sigma12 * sigma22.inv() * (x2.asMatrix() - u2.asMatrix())).sum()
    val sigma1c2 = (sigma11 - sigma12 * sigma22.inv() * sigma21).sum()
    new NormSampler(u1c2, sigma1c2)
  }

  def gaussian(xargs: Double*): Double = {
    val X = NSArray(xargs: _*)
    val xmu = (X - u).asMatrix(colv = false)
    math.pow(2 * math.Pi, -X.length / 2) *  math.pow(sigma.det(), -0.5) * math.exp(-0.5 * (xmu * sigma.inv() * xmu.t()).sum())
  }

  override def sample(): NSArray[Double] = {
    gibbsSampler.sample()
  }
}


object TestGauss {
  def main(args:Array[String]): Unit = {
    val ms = new MultivariateGaussianSampler(NSArray(0, 0), NSMatrix(List(List(1.0, 0.8), List(0.8, 1.0))))
    val samples = (0 to 1000).foldLeft(List[NSArray[Double]]())((lst, _) => lst :+ ms.sample())
    val pw = new PrintWriter("data.txt")
    pw.write(samples.mkString(","))
    pw.close()
  }
}