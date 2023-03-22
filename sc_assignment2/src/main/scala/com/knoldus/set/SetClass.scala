package com.knoldus.set

class SetClass {
  private var set = Set.empty[Int]

  // Add element into set, return the size
  def addElement(elem: Int): Int = {
    set =set +elem
    set.size
  }

  // add the elements of the list to the set, return the size
  def addElement(elements: List[Int]): Int = {
    set =set ++ elements
    set.size
  }

  // get all the subsets of the set(db), make sure to check the types.
  def getSubSetList: List[List[Int]] = {
    set.subsets.map(subset => subset.toList).toList
  }

  // verify it the given list is a subset of the db set.
  def verifyIfSubSet(list: List[Int]): Boolean = {
   val listToSet = list.toSet
    listToSet.subsetOf(set)
  }

  // verify if the element is present in the set.
  def checkIfExists(elem: Int): Boolean = {
      if(set.contains(elem)) true
      else
        false
  }

  // remove if the element is present in the set, return the size
  def removeElement(elem: Int): Int = {
    if(set.contains(elem)) {
      set = set - elem
    }
    set.size
  }

  // return all the odd elements from the set
  def getOddElement: Set[Int] = {
    set.filter(elements => elements % 2 != 0)
  }
}
