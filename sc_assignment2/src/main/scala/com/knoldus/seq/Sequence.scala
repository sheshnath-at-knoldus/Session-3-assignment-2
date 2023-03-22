package com.knoldus.seq

class Sequence {
  private var sequenceString: Seq[String] = Seq.empty[String]
  private var sequenceInt: Seq[Int] = Seq.empty[Int]

  // Check the type of element, if it is int save it to sequenceInt, if it is string save it to sequenceString, else return IllegalArgumentException,  return the size
  def store(element: Any): Int = {
    element match {
      case stringValue :String => sequenceString =sequenceString :+stringValue
      val sizeOfSequenceString=sequenceString.size
          sizeOfSequenceString
      case integerValue :Int => sequenceInt = sequenceInt :+ integerValue
      val sizeOfStringInt =sequenceInt.size
          sizeOfStringInt
      case _ => throw new IllegalArgumentException("Neither Integer Nor String")
    }
  }

  // Check the type of element, if it is int remove it from sequenceInt, if it is string remove it from sequenceString, else return IllegalArgumentException,  return the size
  def removeElement(element: Any): Int = {
    element match {
      case stringValue: String => {
        sequenceString = sequenceString.filterNot(_ == stringValue)
        val sizeOfStringSequence =sequenceString.size
        sizeOfStringSequence
      }
      case integerValue: Int => {
        sequenceInt = sequenceInt.filterNot(_ == integerValue)
        val sizeOfSequenceInt =sequenceInt.size
        sizeOfSequenceInt
      }
      case _ => throw new IllegalArgumentException("Neither Integer Nor String")
    }
  }

}
