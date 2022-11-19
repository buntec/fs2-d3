package d3.selection

import org.scalajs.dom
import scalajs.js
import scala.collection.mutable.ArrayBuffer

trait Selection {

  def select(selector: String): Selection

  def selectAll(selector: String): Selection

  def selectChild(selector: String): Selection

}

object Selection {

  def apply(
      groups: Seq[Seq[dom.Element]],
      parents: Seq[dom.Element]
  ): Selection = new Selection {

    override def selectChild(selector: String): Selection = ???

    private val _groups = groups.toArray.map(_.toArray)
    private val _parents = parents.toArray

    override def selectAll(selector: String): Selection = {

      val m = groups.length
      val subgroups = ArrayBuffer.empty[Array[dom.Element]]
      val newParents = ArrayBuffer.empty[dom.Element]
      var j = 0
      while (j < m) {
        val group = _groups(j)
        val n = group.length
        var i = 0
        while (i < n) {
          val node = group(i)
          if (node != null) {
            subgroups.addOne(node.querySelectorAll(selector).toArray)
            newParents.addOne(node)
          }
          i += 1
        }
        j += 1
      }
      apply(subgroups.map(_.toSeq).toSeq, newParents.toSeq)

    }

    override def select(selector: String): Selection = {
      val m = groups.length
      val subgroups = Array.ofDim[Array[dom.Element]](m)
      var j = 0
      while (j < m) {
        val group = _groups(j)
        val n = group.length
        subgroups(j) = Array.ofDim[dom.Element](n)
        val subgroup = subgroups(j)
        var i = 0
        while (i < n) {
          val node = group(i)
          val subnode = node.querySelector(selector)
          if (node != null && subnode != null) {
            if (node.asInstanceOf[js.Dynamic].__data__ != null) {
              subnode.asInstanceOf[js.Dynamic].__data__ == node
                .asInstanceOf[js.Dynamic]
                .__data__
            }
            subgroup(i) = subnode
          }
          i += 1
        }
        j += 1
      }
      apply(subgroups.map(_.toSeq).toSeq, parents)
    }

  }
}
