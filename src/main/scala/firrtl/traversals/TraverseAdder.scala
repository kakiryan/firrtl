// SPDX-License-Identifier: Apache-2.0

package traversals

import firrtl.{CircuitState, HighForm, Transform, Utils}
import firrtl.ir.{DefModule, Expression, Mux, Statement, DefNode, DoPrim, Connect, SubField, Reference,
BundleType}
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
      println("Node Defn Statement: " +  defNode.name + " is initalized with ")
      print("\t")
      val value = defNode.value
      s.map(walkExpression())
    } else if (s.isInstanceOf[Connect]) {
      val connection = s.asInstanceOf[Connect]
      // the following line will print the lines in the design where the connection is made
      // println("Connection Statement: " + connection.info)
      val loc = connection.loc
      val ref = connection.expr
      if (loc.isInstanceOf[SubField]) {
        val flow = loc.asInstanceOf[SubField].flow
        // println("Connection Flow: " + flow)
        if (ref.isInstanceOf[Reference]) {
          println("Connection!")
          println(ref.asInstanceOf[Reference].name + " assigned to " + loc.asInstanceOf[SubField].name)
        } 
      } else {
        println("Connection location:" + connection.loc)
      }
    } 
 
    s.map(walkStatement())
  }


  def walkExpression()(e: Expression): Expression = {
    if (e.isInstanceOf[DoPrim]) {
      val doPrim = e.asInstanceOf[DoPrim]
      println("DoPrimitive Operation Expr: " +  doPrim.op + " with " + doPrim.args.length + " arguments.")
    } else if (e.isInstanceOf[Reference]) {
      // if (!e.asInstanceOf[Reference].tpe.isInstanceOf[BundleType])
      print("\t\t")
      println(e.asInstanceOf[Reference].name)
    }
    
    else {
      // println("Statement: " + s)
    }
    e.map(walkExpression())
  }
}
