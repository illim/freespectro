package priv.sp.bot

import collection._

trait BotTree {
  type TreeLabel

  class Tree(val label : TreeLabel, val subforest : mutable.ArrayBuffer[Tree] = mutable.ArrayBuffer.empty) {
    def loc = new TreeP(this, 0, None, 0)
  }

  // tree pointer
  class TreeP(var tree : Tree, var pos : Int, val parent : Option[TreeP] = None, val depth : Int = 0){
    def hasNext = parent.exists(_.tree.subforest.size > pos +1)

    def goNext() = goto(pos + 1)
    def goto(p : Int) {
      pos = p
      tree = parent.get.tree.subforest(p)
    }

    def hasChild = tree.subforest.nonEmpty
    def child = new TreeP(tree.subforest.head, 0, Some(this), depth + 1)
    def backPropagate(f : Tree => Unit){
      parent.foreach{ p =>
        f(p.tree)
        p.backPropagate(f)
      }
    }
  }

}
