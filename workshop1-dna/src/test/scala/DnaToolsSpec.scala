import models._
import org.scalacheck.Prop.True

// Specs format (WordSpec here) http://www.scalatest.org/user_guide/selecting_a_style
// Matchers documentation: http://www.scalatest.org/user_guide/using_matchers

class DnaToolsSpec extends UnitTest {

  "Nucleobase factory" should {
    "Safely parse base from String" in {
      Base.get('A') shouldEqual Some(A)
      Base.get('T') shouldEqual Some(T)
      Base.get('C') shouldEqual Some(C)
      Base.get('G') shouldEqual Some(G)
    }

    "Reject invalid chars by returning None" in {
      Base.get('B') shouldEqual None
    }

    "Reject invalid chars by returning None (type-checked)" in {
      forAll { (c: Char) =>
        whenever(c != 'A' & c != 'T' & c != 'C' & c != 'G') {
          Base.get(c) shouldEqual None
        }
      }
    }
  }

  "DnaTools" should {
    "Safely parse DNA strand" in {
      DnaTools.parseDNA("ACGT") shouldEqual Seq(A, C, G, T)
      DnaTools.parseDNA("ATGC") shouldEqual Seq(A, T, G, C)
      DnaTools.parseDNA("") shouldEqual Seq()

    }

    "Reject invalid chars by returning IllegalArgumentException" in {
      assertThrows[IllegalArgumentException] {
        DnaTools.parseDNA("ACGB") shouldEqual Seq(A, C, G)
      }
      assertThrows[IllegalArgumentException] {
        DnaTools.parseDNA("ACFG") shouldEqual Seq(A, C, G)
      }
    }

    /**
      * def complementary(dna: DNA)
      */
    "Safely complementary DNA" in {
      DnaTools.complementary(Seq(A, T, C, G)) shouldEqual Seq(T, A, G, C)
      DnaTools.complementary(Seq(A, G, C, T)) shouldEqual Seq(T, C, G, A)
      DnaTools.complementary(Seq()) shouldEqual Seq()
    }

    /**
      * def countBases(dna: DNA)
      *
      */
    "Safely Count number of each base in the DNA sequence" in {
      DnaTools.countBases(Seq(A, T, C, G)) shouldEqual Map(A -> 1, T -> 1, G -> 1, C -> 1)
      DnaTools.countBases(Seq(A, G, T, C, G, T)) shouldEqual Map(A -> 1, T -> 2, C -> 1, G -> 2)
      DnaTools.countBases(Seq(A, T, C)) shouldEqual Map(A -> 1, T -> 1, C -> 1)
      DnaTools.countBases(Seq()) shouldEqual Map()
    }

    /**
      * def contains(dna: DNA, subsequence: DNA)
      */
    "Safely subsequence is contained in the main DNA sequence" in {
      DnaTools.contains(Seq(A, T, C, G), Seq(T, C)) shouldEqual true
      DnaTools.contains(Seq(A, G, T, C, G, T), Seq(G, T, C)) shouldEqual true
      DnaTools.contains(Seq(A, G, T, C, G, T), Seq()) shouldEqual true
      DnaTools.contains(Seq(), Seq()) shouldEqual true
    }
    "Safely subsequence is not contained in the main DNA sequence" in {
      DnaTools.contains(Seq(A, T, G), Seq(T, C)) shouldEqual false
      DnaTools.contains(Seq(A, G, T, C, G, T), Seq(T, C, T)) shouldEqual false
    }

    /**
      * def insertSubsequence(dna: DNA, subsequence: DNA, index: Int): DNA
      */
    "Insert the `subsequence` at the `index` position of the DNA sequence (0-indexed)" in {
      DnaTools.insertSubsequence(Seq(A, T, C, G), Seq(T, C), 2) shouldEqual Seq(A, T, T, C, C, G)
      DnaTools.insertSubsequence(Seq(A, G, T, C, G, T), Seq(G, T, C), 6) shouldEqual Seq(A, G, T, C, G, T, G, T, C)
      DnaTools.insertSubsequence(Seq(A, G, T, C, G, T), Seq(), 3) shouldEqual Seq(A, G, T, C, G, T)
      DnaTools.insertSubsequence(Seq(), Seq(), 3) shouldEqual Seq()
    }

    /**
      * def hammingDistance(dna1: DNA, dna2: DNA): Long
      */
    "Safely return the hamming distance of dna1 and dna2" in {
      DnaTools.hammingDistance(Seq(), Seq()) shouldEqual 0
      DnaTools.hammingDistance(Seq(A,T,C,G), Seq(A,T,C,G)) shouldEqual 0
      DnaTools.hammingDistance(Seq(A,T,C,G), Seq(A,T,G,G)) shouldEqual 1
      DnaTools.hammingDistance(Seq(A,T,C,G), Seq(T,A,G,C)) shouldEqual 4
      DnaTools.hammingDistance(Seq(T,T,A,A,T), Seq(T,T,A,A,G,C,A)) shouldEqual 1
    }

    /**
      * def basesDifferences(dna1: DNA, dna2: DNA): Seq[Int]
      */
    "Safely return indices of the differences between the two sequences" in {
      DnaTools.basesDifferences(Seq(A,T,C,G), Seq(A,T,G,G)) shouldEqual Seq(2)
      DnaTools.basesDifferences(Seq(A,T,C,G), Seq(T,A,G,C)) shouldEqual Seq(0, 1, 2, 3)
      DnaTools.basesDifferences(Seq(T,T,A,A,T), Seq(T,T,A,A,G,C,A)) shouldEqual Seq(4)
      DnaTools.basesDifferences(Seq(T,T,A,A), Seq(T,T,A,A)) shouldEqual Seq()
      DnaTools.basesDifferences(Seq(), Seq()) shouldEqual Seq()
    }

    /**
      * def translate(dna: DNA): Seq[String]
      */
    "Safely return the 3 possible translations for a DNA sequence" in {
      DnaTools.translate(Seq()) shouldEqual Seq()
      DnaTools.translate(Seq(A)) shouldEqual Seq()
      DnaTools.translate(Seq(A,G)) shouldEqual Seq()
      DnaTools.translate(Seq(A,G,G,T,G,A,C,A,C,C,G,C,A,A,G,C,C,T,T,A,T,A,T,T,A,G,C)) shouldEqual Seq("R*HRKPYIS", "GDTASLIL", "VTPQALY*")
    }

    /**
      * def longestSequences(dna: DNA): Map[Base, Int]
      */
    "Safely return Map of the longest streak by nucleobase" in {
      DnaTools.longestSequences(Seq(A,T,T,T,T,A,A,C,C,C,C,G,C,G)) shouldEqual Map(A->2, T->4, C->4, G->1)
      DnaTools.longestSequences(Seq(A)) shouldEqual Map(A->1)
      DnaTools.longestSequences(Seq(A, T, C, G)) shouldEqual Map(A->1, T->1, G->1, C->1)
      DnaTools.longestSequences(Seq()) shouldEqual Map()
    }



  }

}
