package unix.utility

import scala.io.Source
import java.io.File
import scala.annotation.tailrec

object WcApp {

  def main(args: Array[String]): Unit = {
    println("Hello, world!")
  }

  def byteCount(file: File): Long = file.length
  def characterCount(file: File): Long = {
    @tailrec def charCount(iter: Iterator[String])(implicit acc: Long = 0): Long = {
      if (iter.hasNext) charCount(iter)(acc + iter.next().length)
      else acc
    }
    charCount(getLines(file))
  }
  def wordCount(file: File): Long = getLines(file).flatMap(_.split("\\W+")).length
  def lineCount(file: File): Long = getLines(file).length
  def longestLineSize(file: File): Long = {
    @tailrec def longestLine(iter: Iterator[String])(implicit max: Long = 0): Long = {
      if (iter.hasNext) {
        val lineSize = iter.next().length
        longestLine(iter)(max.max(lineSize))
      } else max
    }
    longestLine(getLines(file))
  }

  def getLines(file: File): Iterator[String] = Source.fromFile(file).getLines()
}
