package collections.list

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.{a, convertToAnyShouldWrapper}

class ListTest extends AnyFunSpec {

  describe("ListTest") {
    describe("should take") {
      val list = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
      it(" 5") {
        val list2 = list.take(5)
        list2.size shouldBe 5
        list2 shouldBe a [List[Int]]
      }
      it(" 0") {
        val list2 = list.take(0)
        list2.size shouldBe 0
        list2 shouldBe a [List[Nothing]]
      }
    }

    it("should init") {

    }

    it("should reverse") {

    }

    it("should takeRight") {

    }

    it("should cons") {

    }

    it("should exists") {

    }

    it("should foldLeft") {

    }

    it("should zip") {

    }

    it("should filter") {

    }

    it("should isEmpty") {

    }

    it("should collect") {

    }

    it("should flatMap") {

    }

    it("should map") {

    }

    it("should last") {

    }

    it("should head") {

    }

    it("should zipWithIndex") {

    }

    it("should forAll") {

    }

    it("should foreach") {

    }

    it("should append") {

    }

    it("should headOpt") {

    }

    it("should count") {

    }

    it("should tail") {

    }

    it("should indexOf") {

    }

    it("should lastOpt") {

    }

    it("should foldRight") {

    }

    it("should withFilter") {

    }

    it("should size") {

    }

    it("should find") {

    }

    it("should fill") {

    }

    it("should apply") {

    }

    it("should empty") {

    }
  }
}
