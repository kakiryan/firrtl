// SPDX-License-Identifier: Apache-2.0

package traversals

import firrtl.{CircuitState, HighForm, Transform, Utils}
import firrtl.ir.{DefModule, Expression, Mux, Statement}
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
    println("Made it into module: " + m)
    m.map(walkStatement())
  }

  def walkStatement()(s: Statement): Statement = {
    println("Statement: " + s)
    s.map(walkExpression())
    s.map(walkStatement())
  }


  def walkExpression()(e: Expression): Expression = {
    println(" Expression:" + e )
    e.map(walkExpression())
  }
}
