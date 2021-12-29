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
    @tailrec def charCount(acc: Long = 0, iter: Iterator[String]): Long = {
      if (iter.hasNext) charCount(acc + iter.next().length, iter)
      else acc
    }
    charCount(iter = getLines(file))
  }
  def wordCount(file: File): Long = getLines(file).flatMap(_.split("\\W+")).length
  def lineCount(file: File): Long = getLines(file).length
  def longestLineSize(file: File): Long = {
    @tailrec def longestLine(max: Long = 0, iter: Iterator[String]): Long = {
      if (iter.hasNext) {
        val lineSize = iter.next().length
        longestLine(max.max(lineSize), iter)
      } else max
    }
    longestLine(iter = getLines(file))
  }

  def getLines(file: File): Iterator[String] = Source.fromFile(file).getLines()
}
