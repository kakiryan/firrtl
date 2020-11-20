;buildInfoPackage: chisel3, version: 3.3.1, scalaVersion: 2.12.11, sbtVersion: 1.3.10
circuit Adder : 
  module Adder : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip value1 : UInt<1>, flip value2 : UInt<1>, flip carryIn : UInt<1>, carryOut : UInt<1>, sum : UInt<1>}
    
    node _T = xor(io.value1, io.value2) @[Adder.scala 19:25]
    node _T_1 = xor(_T, io.carryIn) @[Adder.scala 19:37]
    io.sum <= _T_1 @[Adder.scala 19:12]
    node _T_2 = and(io.value1, io.value2) @[Adder.scala 20:31]
    node _T_3 = and(io.value1, io.carryIn) @[Adder.scala 20:57]
    node _T_4 = or(_T_2, _T_3) @[Adder.scala 20:44]
    node _T_5 = and(io.value2, io.carryIn) @[Adder.scala 20:84]
    node _T_6 = or(_T_4, _T_5) @[Adder.scala 20:71]
    io.carryOut <= _T_6 @[Adder.scala 20:17]
    
