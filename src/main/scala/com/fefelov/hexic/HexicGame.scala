package com.fefelov.hexic

/**
 * Created with IntelliJ IDEA.
 * User: fvlad
 * Date: 12/17/12
 * Time: 7:35 PM
 */
class HexicGame {

  def launch(){
    val table = new HexicTable(20, 20, 2)
    draw(table)
  }

  def draw(table: HexicTable) {

    val (columns, rows) = (table.columns, table.rows)

    def oddRowCell(row: Int, column: Int) = table(row, column * 2)

    def evenRowCell(row: Int, column: Int) = table(row, column * 2 - 1)

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
