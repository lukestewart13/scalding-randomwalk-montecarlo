/**
 * Created by lukestewart on 4/7/15.
 */
import com.twitter.algebird.AveragedValue
import com.twitter.scalding._
import scala.util.Random

class DrunkGuyJob(args: Args) extends Job(args) {

  val n = 1000
  val e = 121

  case class Drunk(private val initPos: (Int, Int) = (0, 0)) {
    private var x: Int = initPos._1
    private var y: Int = initPos._2
    def step(): Unit = {
      x += Random.nextInt(3) - 1
      y += Random.nextInt(3) - 1
    }
    def pos: (Int, Int) = (x, y)
    def distanceTraveled: Double = Math.sqrt(Math.pow(x - initPos._1, 2) + Math.pow(y - initPos._2, 2))
  }

  def makeDrunks(n: Int): List[Drunk] = List.fill(n)(Drunk())
  val drunkPipe = TypedPipe.from(makeDrunks(n))
  val lostDrunkPipe = drunkPipe map { drunk =>
    (1 to e).foreach(_ => drunk.step())
    drunk
  }
  val avgAgg = AveragedValue.aggregator.composePrepare[Drunk](_.distanceTraveled)
  val avgDistanceTraveled = lostDrunkPipe.aggregate(avgAgg)
  avgDistanceTraveled.write(TypedTsv(args("output")))

}
