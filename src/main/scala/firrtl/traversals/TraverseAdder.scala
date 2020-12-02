// SPDX-License-Identifier: Apache-2.0

package traversals

import firrtl.{CircuitState, HighForm, Transform, Utils}
import firrtl.ir.{DefModule, Expression, Mux, Statement, DefNode, DoPrim, Connect}
import firrtl.Mappers._
import scala.collection.mutable

class TraverseAdder extends Transform {

  def inputForm = HighForm

  def outputForm = HighForm

  def execute(state: CircuitState): CircuitState = {
    val circuit = state.circuit

    println("Going to walk module")
    circuit.map(walkModule())

    // Return an unchanged [[firrtl.CircuitState CircuitState]]
    state
  }

  def walkModule()(m: DefModule): DefModule = {
    println("Made it into module: " + m.name)
    m.map(walkStatement())
  }

  def walkStatement()(s: Statement): Statement = {
    if (s.isInstanceOf[DefNode]) {
      val defNode = s.asInstanceOf[DefNode]
      println("Node Defn Statement: " +  defNode.name)
    } else if (s.isInstanceOf[Connect]) {
      val connection = s.asInstanceOf[Connect]
      println("Connection Statement: " + connection.info)
    }
    s.map(walkExpression())
    s.map(walkStatement())
  }


  def walkExpression()(e: Expression): Expression = {
    if (e.isInstanceOf[DoPrim]) {
      val doPrim = e.asInstanceOf[DoPrim]
      println("DoPrimitive Operation Expr: " +  doPrim.op + " with " + doPrim.args.length + " arguments.")
    } else {
      // println("Statement: " + s)
    }
    e.map(walkExpression())
  }
}
