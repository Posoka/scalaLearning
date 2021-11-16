package unix.utility

import org.scalatest.funspec.AnyFunSpec

import java.io.File

class WcAppTest extends AnyFunSpec {

  val shortFile = new File("/Users/jacekbosiacki/IdeaProjects/scalaLearning/src/test/resources/shortText.txt")
  val file = new File("/Users/jacekbosiacki/IdeaProjects/scalaLearning/src/test/resources/test.txt")

  describe("a file") {
    describe("when counting") {
      describe("bytes") {
        it("should return 5") {
          assert(WcApp.byteCount(shortFile) == 5)
        }
        it("should return 40") {
          assert(WcApp.byteCount(file) == 50)
        }
      }
      describe("characters") {
        it("should return 5") {
          assert(WcApp.characterCount(shortFile) == 5)
        }
        it("should return 50") {
          assert(WcApp.characterCount(file) == 47)
        }
      }
      describe("words") {
        it("should return 1") {
          assert(WcApp.wordCount(shortFile) == 1)
        }
        it("should return 10") {
          assert(WcApp.wordCount(file) == 10)
        }
      }
      describe("lines") {
        it("should return 1") {
          assert(WcApp.lineCount(shortFile) == 1)
        }
        it("should return 4") {
          assert(WcApp.lineCount(file) == 4)
        }
      }
      describe("longest line") {
        it("should return 5") {
          assert(WcApp.longestLineSize(shortFile) == 5)
        }
        it("should return 14") {
          assert(WcApp.longestLineSize(file) == 14)
        }
      }
    }

  }
}
