package com.fefelov.hexic

/**
 * Created with IntelliJ IDEA.
 * User: fvlad
 * Date: 12/17/12
 * Time: 7:35 PM
 */
class HexicGame {

  def launch(){
    print("Welcom to hexic. Press ENTER key to continue...")
    readLine()
    println("Initializing board...")
    new HexicTable(8, 8, 10).print()    // second argument should be even
  }

}
