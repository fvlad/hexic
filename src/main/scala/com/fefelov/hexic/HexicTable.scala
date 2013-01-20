package com.fefelov.hexic

import util.Random
import collection.GenTraversableOnce

/**
 * Created with IntelliJ IDEA.
 * User: fvlad
 * Date: 12/17/12
 * Time: 10:25 PM
 */

class HexicTable(val rows: Int, val columns: Int, numOfColors: Int) {

  val rand = new Random()

  val table = Array.tabulate[Int](rows, columns)((_, _) => rand.nextInt(numOfColors))

  def apply(row: Int, column: Int): Int = table(row - 1)(column - 1)

  def apply(point: Point): Int = apply(point.row, point.column)

  def allPoints = for (row <- (1 to rows); column <- (1 to columns) ) yield Point(row, column)

  def belongs(point: Point) = (1 to rows).contains(point.row) && (1 to columns).contains(point.column)

  def clustersFor(point: Point) = {
    if (!belongs(point)){
      throw new IllegalArgumentException("Point doesn't belong the board.")
    }

    (for {
      neighbor <- point.neighbors.filter(belongs)
      neighborOfNeighbor <- neighbor.neighbors.filter(belongs)
      if (neighborOfNeighbor isNeighbor point)
    } yield Cluster(Set(point, neighbor, neighborOfNeighbor))).toSet
  }

  def sameColor(c1: Cluster)(c2: Cluster) = Set(c1.point, c2.point).map(apply).size == 1

  def allClusters = allPoints.flatMap(clustersFor).toSet

  def oneColorClusters = allClusters.filter(_.points.map(apply).toSet.size == 1)

  def oneColorMaxClusters: Option[Cluster] = Stream.iterate(oneColorClusters){ clusters =>
    (for (c1 <- clusters; c2 <- clusters if (c1 != c2)) yield c1 join c2).flatten.toSet
  }.takeWhile(_.size > 0).lastOption.flatMap(_.toSeq.sortWith(_.size > _.size).headOption)

}

case class Point(row: Int, column: Int){

  /**
   * Specifies, whether the point
   * belongs to odd row or not.
   *
   * @return true, if it belongs to odd row.
   */
  lazy val isOdd = column % 2 == 0

  def neighborRows = (row - 1 to row + 1)

  def neighborColumns(r: Int) = if (r == row) Seq(column - 1, column + 1)
  else if (r == row + 1 && isOdd || r == row - 1 && !isOdd) Seq(column)
  else (column - 1 to column + 1)

  def neighbors = for {
    r <- neighborRows
    c <- neighborColumns(r)
  } yield Point(r, c)

  def isNeighbor(point: Point) = neighbors contains point

}

case class Cluster(points: Set[Point]){

  def isCommon(cluster: Cluster) = cluster.points.filter(points.contains).size > 1

  def contains(cluster: Cluster) = cluster.points.filter(points.contains).size == cluster.points.size

  def join(cluster: Cluster) = if (isCommon(cluster)) Some(Cluster(points ++ cluster.points)) else None

  def size = points.size

  def point = points.headOption.get

}