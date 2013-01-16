package com.fefelov.hexic

import util.Random

/**
 * Created with IntelliJ IDEA.
 * User: fvlad
 * Date: 12/17/12
 * Time: 10:25 PM
 */

class HexicTable(rows: Int, columns: Int, numOfColors: Int) {

  val rand = new Random()
  val table = Array.tabulate[Int](rows, columns)((_, _) => rand.nextInt(numOfColors))

  def print() {

    def oddRowCell(row: Int, column: Int) = table(row - 1)(column * 2 - 1)

    def evenRowCell(row: Int, column: Int) = table(row - 1)(column * 2 - 2)


    for (row <- table){
      for (el <- row){
        printf(el.toString)
      }
      println()
    }

    def printStartRow() {
      (1 to columns / 2) foreach { _ => printf("   _") }
      println()
    }

    def printFirstRow(){
      printf(" _/")
      (1 to columns / 2 - 1) foreach { c => printf("%d\\_/", oddRowCell(1, c)) }
      printf("%d\\", oddRowCell(1, columns / 2))
      println()
    }

    def printLastRow() {
      (1 to columns / 2) foreach { _ => printf("\\_/ ") }
      println()
    }

    def printOddRow(row: Int){
      (1 to columns / 2) foreach { c => printf("\\_/%d", oddRowCell(row, c)) }
      println("\\")
    }


    def printEvenRow(row: Int){
      (1 to columns / 2) foreach { c => printf("/%d\\_", evenRowCell(row, c)) }
      println("/")
    }


    printStartRow()
    printFirstRow()
    printEvenRow(1)

    (2 to rows) foreach { row =>
      printOddRow(row)
      printEvenRow(row)
    }

    printLastRow()

  }
}
