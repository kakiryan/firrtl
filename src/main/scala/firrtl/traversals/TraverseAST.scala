// SPDX-License-Identifier: Apache-2.0

package traversals

import firrtl.{CircuitState, HighForm, Transform, Utils, Flow}
import firrtl.ir.{DefModule, Expression, DefRegister, Mux, Statement, DefNode, DoPrim, Connect, SubField, Reference,
BundleType}
import firrtl.Mappers._
import scala.collection.mutable

class TraverseAST extends Transform {

  def inputForm = HighForm

  def outputForm = HighForm

  def execute(state: CircuitState): CircuitState = {
    val circuit = state.circuit
    circuit.map(walkModule())

    // Return an unchanged [[firrtl.CircuitState CircuitState]]
    state
  }

  def walkModule()(m: DefModule): DefModule = {
    println("Module: " + m.name)
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
          print("Connection Statement: ")
          println(ref.asInstanceOf[Reference].name + " assigned to " + loc.asInstanceOf[SubField].name)
        } 
      } else if (loc.isInstanceOf[Reference] && ref.isInstanceOf[Reference]) {
        val refLoc = loc.asInstanceOf[Reference]
        println("Connection Statement: " + ref.asInstanceOf[Reference].name + " assigned to " + refLoc.name )
      } else {
        println("Connection location:" + connection.loc)
      }
    } else if (s.isInstanceOf[DefRegister]) {
      val reg = s.asInstanceOf[DefRegister]
      println("Register Defn Statement: " + reg.name + " initalized with " + reg.init)
    }
 
    s.map(walkStatement())
  }


  def walkExpression()(e: Expression): Expression = {
    // print("Walking expression")
    if (e.isInstanceOf[DoPrim]) {
      val doPrim = e.asInstanceOf[DoPrim]
      println("DoPrimitive Operation Expr: " +  doPrim.op + " with " + doPrim.args.length + " arguments.")
    } else if (e.isInstanceOf[Reference]) {
        if (!e.asInstanceOf[Reference].tpe.isInstanceOf[BundleType]) {
          print("\t\t")
          println(e.asInstanceOf[Reference].name)
        } 
      }
      else if (e.isInstanceOf[SubField]) {
        // prints subfields of a bundle type
        print("\t\t")
        println(e.asInstanceOf[SubField].name)
      } else if (e.isInstanceOf[Mux]) {
        val mux = e.asInstanceOf[Mux]
        println("Mux Expression")
      }
    
    e.map(walkExpression())
  }
}
