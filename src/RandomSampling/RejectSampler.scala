package RandomSampling

import java.io.PrintWriter

import Common.Sampler
import DirectSamplers.NormSampler
import NumScala.NSArray

class RejectSampler(sampledPdf: (Double*)=>Double, coverSampler: Sampler, coverCoeff: Double) extends Sampler{
  val pdf: (Double*) => Double = sampledPdf
  val nDim: Int = coverSampler.nDim

  override def sample(): NSArray[Double] = {
    var sample: NSArray[Double] = null
    var alpha = 0.0

    do {
      sample = coverSampler.sample()
      alpha = sampledPdf(sample: _*) / (coverSampler.pdf(sample: _*) * coverCoeff)
    } while(math.random() > alpha)

    sample
  }
}


object RSTest {
  def main(args:Array[String]): Unit = {
    val rjs = new RejectSampler(new NormSampler().pdf, new NormSampler(1, 1), 100)
    val samples = (0 to 10000).foldLeft(List[Double]())((lst, _) => lst :+ rjs.sample().head)
    val pw = new PrintWriter("data.txt")
    pw.write(samples.mkString(","))
    pw.close()
  }
}



