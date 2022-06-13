import scala.annotation.tailrec

object Main extends App {

  sealed trait Event {
    def time: Int
  }
  object Event {
    def fromIntervals(intervals: Seq[(Int, Int)]): List[Event] =
      intervals.flatMap {
        case (startTime, endTime) => List(Start(startTime), End(endTime))
      }.toList
  }
  case class Start(time: Int) extends Event
  case class End(time: Int) extends Event

  @tailrec
  def parseEventsTailRec(events: List[Event], runningRoomCount: Int, maxRoomCount: Int): Int =
    events match {
      case event::remainingEvents =>
        val updatedRunningRoomCount = event match {
          case _: Start => runningRoomCount + 1
          case _: End => runningRoomCount - 1
        }
        val updatedMaxRoomCount = Math.max(updatedRunningRoomCount, maxRoomCount)
        parseEventsTailRec(remainingEvents, updatedRunningRoomCount, updatedMaxRoomCount)

      case Nil => maxRoomCount
    }

  def countMaxRoomNeeded(intervals: Seq[(Int, Int)]): Int =
    parseEventsTailRec(Event.fromIntervals(intervals).sortBy(_.time), 0, 0)



  val intervals1: Array[(Int, Int)] = Array((0, 5), (2, 7), (5, 6))
  val intervals2: Array[(Int, Int)] = Array((0, 8), (1, 3), (2, 4), (3, 7), (5, 9))

  println(s"Room count for intervals 1: ${countMaxRoomNeeded(intervals1)}")
  println(s"Room count for intervals 2: ${countMaxRoomNeeded(intervals2)}")
}
