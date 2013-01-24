package com.fefelov.hexic

import util.Random
import collection.GenTraversableOnce
import collection.immutable.IndexedSeq

/**
 * Created with IntelliJ IDEA.
 * User: fvlad
 * Date: 12/17/12
 * Time: 10:25 PM
 */

class HexicTable(val rows: Int, val columns: Int, numOfColors: Int) { hexicTable =>

  lazy val rand = new Random()

  //TODO: make sure there are no one color clusters right after init
  lazy val table = Array.tabulate[Int](rows, columns)((_, _) => rand.nextInt(numOfColors))

  def apply(row: Int, column: Int): Int = table(row - 1)(column - 1)

  def apply(point: Point): Int = apply(point.row, point.column)

  def allPoints = for (row <- (1 to rows); column <- (1 to columns) ) yield Point(row, column)

  def belongs(point: Point) = (1 to rows).contains(point.row) && (1 to columns).contains(point.column)

  def clustersFor(point: Point): Set[Cluster] = {
    if (!belongs(point)){
      throw new IllegalArgumentException("%s doesn't belong the board.".format(point))
    }

    for {
      neighbor <- point.neighbors filter belongs
      neighborOfNeighbor <- neighbor.neighbors filter belongs
      if (neighborOfNeighbor isNeighbor point)
    } yield Cluster(Set(point, neighbor, neighborOfNeighbor))
  }

  def sameColor(c1: Cluster)(c2: Cluster) = Set(c1.point(0), c2.point(0)).map(apply).size == 1

  lazy val allClusters: Set[Cluster] = allPoints.flatMap(clustersFor).toSet

  def oneColor(cluster: Cluster) = cluster.points.map(apply).toSet.size == 1

  def oneColorClusters: Set[Cluster] = allClusters filter oneColor

  def oneColorMaxCluster: Option[Cluster] = Stream.iterate(oneColorClusters){ clusters =>
    clusters.flatMap(c => clusters.view filter (sameColor(c)) find (c isCommon) map (c join))
  }.takeWhile(_.size > 0).lastOption.flatMap(_.toSeq.sortWith(_.size > _.size).headOption)

  case class Rotation(rotatedCluster: Cluster, shift: Int)
    extends HexicTable(rows, columns, numOfColors){

    override lazy val table: Array[Array[Int]] = hexicTable.table

    override lazy val allClusters: Set[Cluster] = hexicTable.allClusters

    def checkRotated(point: Point) = if (!rotatedCluster.contains(point)) point else {
      val pointIndex = rotatedCluster.points.toSeq.indexOf(point)
      rotatedCluster.point((pointIndex + shift) % rotatedCluster.size)
    }

    override def apply(row: Int, column: Int): Int = {
      val point = checkRotated(Point(row, column))
      super.apply(point.row, point.column)
    }
  }

  def rotations = for (cluster <- allClusters; shift <- Seq(1, 2)) yield Rotation(cluster, shift)

  def bestRotation = rotations.map(r => (r.oneColorMaxCluster.map(_.size).getOrElse(0), r))
    .reduceLeft((a, b) => if (a._1 > b._1) a else b)

}

case class Point(row: Int, column: Int){

  /**
   * Specifies, whether the point
   * belongs to odd row or not.
   *
   * @return true, if it belongs to odd row.
   */
  lazy val isOdd = column % 2 == 0

  def neighborRows = Set(row - 1, row, row + 1)

  def neighborColumns(r: Int) = if (r == row) Set(column - 1, column + 1)
  else if (r == row + 1 && isOdd || r == row - 1 && !isOdd) Set(column)
  else Set(column - 1, column, column + 1)

  def neighbors: Set[Point] = for {
    r <- neighborRows
    c <- neighborColumns(r)
  } yield Point(r, c)

  def isNeighbor(point: Point) = neighbors contains point

}

case class Cluster(points: Set[Point]){

  def isCommon(c: Cluster) = c != this && c.points.filter(points.contains).size > 1

  def contains(point: Point) = points contains point

  def join(c: Cluster) = Cluster(points ++ c.points)

  def size = points.size

  def without(point: Point) = Cluster(points filterNot Seq(point).contains)

  def point(n: Int) = points.toSeq(n)

}