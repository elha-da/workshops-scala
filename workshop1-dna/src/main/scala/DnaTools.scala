import models._

object DnaTools {

  type DNA = Seq[Base]

  /**
    * Parse a DNA sequence from String as a sequence of nucleobase
    *
    * @throws IllegalArgumentException if the sequence is not valid
    */

  def parseDNA(str: String): DNA = {
    def errorOnParse(c: Char) = Base.get(c) match {
      case Some(b) => b
      case None => throw new IllegalArgumentException
    }

    str map errorOnParse
  }

  //  def parseDNA(str: String): DNA = str flatMap {c => Base.get(c)}


  /**
    * Return the complementary sequences of a DNA sequence.
    *
    * Nucleobase A/T are complements of each other, as C and G.
    */
  def complementary(dna: DNA): DNA = dna flatMap (_ match {
    case A => Some(T)
    case T => Some(A)
    case C => Some(G)
    case G => Some(C)
    //    case _  => None
  })


  /**
    * Count the number of each base in the DNA sequence
    */
  def countBases(dna: DNA): Map[Base, Int] = {
    //    dna.groupBy(i => i).map(b => (b._1, b._2.length))
    dna.groupBy(identity).mapValues(_.size)
  }

  /**
    * Check if the `subsequence` is contained in the main DNA sequence.
    */
  //  def contains(dna: DNA, subsequence: DNA): Boolean = dna.containsSlice(subsequence)
  def contains(dna: DNA, subsequence: DNA): Boolean = dna.tails exists (_.startsWith(subsequence))


  /**
    * Insert the `subsequence` at the `index` position of the DNA sequence (0-indexed)
    */
  def insertSubsequence(dna: DNA, subsequence: DNA, index: Int): DNA = {
    dna.splitAt(index)._1 ++ subsequence ++ dna.splitAt(index)._2
  }

  /**
    * Process the Hamming distance of two DNA sequences.
    *
    * The hamming distance is calculated by comparing two DNA strands
    * and counting how many of the nucleotides are different from their equivalent
    * in the other string.
    *
    * Note: The Hamming distance is only defined for sequences of equal length.
    * You must count the differences in the first N bases, where `N = min(dna1.size, dna2.size)`
    *
    * Eg:
    *   - Distance ATCG & ATGG = 1
    *   - Distance ATCG & TAGC = 4
    *   - Distance TTAAT & TTAAGCA = 1
    *
    * @return the hamming distance of dna1 and dna2
    */
  def hammingDistance(dna1: DNA, dna2: DNA): Long = {
    for {
      c <- (dna2 zip dna1)
      if (c._1 != c._2)
      x: Long = 1
    } yield x
  }.sum

  /**
    * Search the differences between two DNA sequences.
    *
    * Sames rules as the Hamming distance
    *
    * @return The indices (0 based) of the differences between the two sequences
    */
  def basesDifferences(dna1: DNA, dna2: DNA): Seq[Int] = for {
    ((b1, b2), i) <- (dna2 zip dna1).zipWithIndex
    if b1 != b2
  } yield i


  private val translationTableSource =
    """
      |FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG
      |TTTTTTTTTTTTTTTTCCCCCCCCCCCCCCCCAAAAAAAAAAAAAAAAGGGGGGGGGGGGGGGG
      |TTTTCCCCAAAAGGGGTTTTCCCCAAAAGGGGTTTTCCCCAAAAGGGGTTTTCCCCAAAAGGGG
      |TCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAG
    """.trim.stripMargin

  private def parse(tab: Array[String]): Map[String, Char] = {
    def loop(acu: Map[String, Char], i: Int): Map[String, Char] = {
      if (tab(0).size == i)
        acu
      else {
        val one = tab(1)(i)
        val two = tab(2)(i)
        val three = tab(3)(i)
        val aa = tab(0)(i)
        loop(acu + (s"$one$two$three" -> aa), i + 1)
      }
    }

    loop(Map.empty, 0)
  }

  private val translationTableSourceParsed = parse(translationTableSource.split("\n"))

  /**
    * Translate a DNA sequence in 6 frames
    *
    * In genetics a reading frame is a way to divide a sequence of nucleotides (DNA bases) into a set of consecutive
    * non-overlapping triplets (also called codon).
    * Each of this triplets is translated into an amino-acid during a translation process to create proteins.
    *
    * Eg: AGGTGACACCGCAAGCCTTATATTAGC has 3 frames
    * Frame 1: AGG·TGA·CAC·CGC·AAG·CCT·TAT·ATT·AGC
    * Frame 2: A·GGT·GAC·ACC·GCA·AGC·CTT·ATA·TTA·GC
    * Frame 3: AG·GTG·ACA·CCG·CAA·GCC·TTA·TAT·TAG·C
    *
    * Translate all these frame with the "standard table of genetic code" (available in `translationTableSource`.
    * Line1 : translated tri-nucléotide. Line 2, 3 and 4 : the three elements of the tri-nucleotide
    *
    * Eg:
    *  - AGG = R
    *  - TGA = *
    *
    * Translations of the 3 frames defined above: R*HRKPYIS, GDTASLIL, VTPQALY*
    *
    * @return the 3 possible translations for a DNA sequence
    */
  def translate(dna: DNA): Seq[String] = {
    def doTranslate(acu: Seq[String] = Seq(), i: Int = 0, nbrFrames: Int): Seq[String] = {
      val dnaTmp = dna.drop(i)
      if (dnaTmp.size < 3 || i == nbrFrames) {
        acu
      }
      else {
        val rst = dnaTmp.grouped(3).toList map {
          e => translationTableSourceParsed.getOrElse(e mkString, "")
        }
        doTranslate(acu ++ Seq(rst mkString), i + 1, nbrFrames)
      }
    }
    doTranslate(nbrFrames = 3)
  }


  /**
    * Count the longest streak (uninterrupted sequence) of each nucleobase in the given DNA sequence
    *
    * Eg: ATTTTAACCCCGCG
    * Returns: A->2, T->4, C->4, G->1
    *
    * @return Map of the longest streak by nucleobase
    */
  /*def longestSequences(dna: DNA): Map[Base, Int] = {
    def loop(sub: DNA, i: Int): List[DNA] = {
      if (i == dna.size)
        List(sub)
      else {
        if (sub(0) == dna(i))
          loop(sub.:+(dna(i)), i + 1)
        else
          sub :: loop(dna.slice(i, i + 1), i + 1)
      }
    }

    if (dna.size < 1)
      Map.empty
    else
      loop(dna.take(1), 1)
        .groupBy(d => d(0))
        .mapValues(l => l.maxBy(d => d.size).size)
  }*/
  def longestSequences(dna: DNA): Map[Base, Int] = {
    val dnaMapped = dna.zip(dna.drop(1)).zipWithIndex.filter { case ((a, b), i) => (a != b) }
    def loop(accu: Seq[DNA] = Seq(), dna2: DNA, i: Int = 0): Seq[DNA] = {
      if (i == 0) {
        loop(accu ++ Seq(dna2.splitAt(1)._1), dna2.drop(1), i + 1)
      }
      else if (dna2.size != 1 && i < dnaMapped.size) {
        val index = (dnaMapped(i)._2)-(dnaMapped(i-1)._2)
        loop(accu ++ Seq(dna2.splitAt(index)._1), dna2.drop(index), i+1)
      }
      else {
        if (dna2.size != 0)
          accu ++ Seq(dna2)
        else
          accu
      }
    }
    if (dna.size >= 1)
      loop(dna2 = dna)
        .groupBy(d => d(0))
        .mapValues(l => l.maxBy(d => d.size).size)
    else
      Map.empty
  }
}
