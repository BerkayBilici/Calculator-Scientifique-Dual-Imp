# Calculator Scientifique: Dual-Paradigm Toolchain

A language processing project for the Calculator Scientifique DSL, implemented using two contrasting paradigms:
- C (Flex/Bison) for symbolic polynomial computation
- Scheme for static semantic analysis

## Overview
This repository contains two implementations that target the same source language. The language supports polynomial expressions, user-defined functions, and symbolic calculus operations (derivation/integration).

### 1) Symbolic Polynomial Engine (C Implementation)
A syntax-directed interpreter built with Flex & Bison. It parses expressions and directly constructs a polynomial representation using custom linked lists.
- Capabilities: Polynomial arithmetic, function evaluation, symbolic Derivation and Integration
- Representation: Linked-list terms with integer coefficients (as rational numbers) and variable powers
- Memory: Manual allocation/deallocation (malloc/free)

### 2) Static Semantic Analyzer (Scheme Implementation)
A static checker that validates programs without executing them.
- Detects: Redefined functions, undefined function parameters, arity contradictions, and related semantic errors
- Technique: Recursive list processing and functional composition

## Project Structure
c-symbolic-engine/
  src/
  tests/

scheme-static-analyzer/
  src/
  tests/

## Getting Started

### Running the C Engine
Prerequisites: gcc, flex, bison, make

Commands:
  cd c-symbolic-engine/src
  make
  ./calculator < ../tests/input_sample.txt

### Running the Scheme Analyzer
Prerequisites: MIT-Scheme (or compatible Scheme)

Commands (example):
  mit-scheme --load src/analyzer.scm
  (load "tests/example_program.scm")

## Grammar Specification (BNF)
<program> ::= <definition_block> <calculation_block>

<definition_block> ::= ε | <def_stmt> <definition_block>
<def_stmt> ::= <fn_def> | ";"

<calculation_block> ::= ε | <calc_stmt> <calculation_block>
<calc_stmt> ::= <calc> | ";"

<fn_def> ::= <identifier> "(" <var_parameters> ")" "=" <oe> ";"
          | <identifier> "(" ")" "=" <oe> ";"

<var_parameters> ::= <identifier> | <var_parameters> "," <identifier>

<oe>  ::= <oe> "+" <oe1> | <oe> "-" <oe1> | <oe1>
<oe1> ::= <oe1> "*" <oe2> | <oe1> <oe2> | <oe2>
<oe2> ::= <integer> | <identifier> | "(" <oe> ")"
       | <identifier> "^" <integer> | <integer> "^" <integer>

<calc> ::= "calculate" <ee> ";"

<ee>  ::= <ee> "+" <ee1> | <ee> "-" <ee1> | <ee1>
<ee1> ::= <ee1> "*" <ee2> | <ee1> <ee2> | <ee2>
<ee2> ::= <integer> | <identifier> | "(" ")"
       | <identifier> "(" <ee3> ")" | "(" <ee3> ")" | <identifier> "(" ")"
       | <identifier> "^" <integer> | <integer> "^" <integer>
       | "I" "(" <identifier> "," <identifier> "," <integer> ")"
       | "D" "(" <identifier> "," <identifier> "," <integer> ")"

<ee3> ::= <ee> | <ee3> "," <ee>

<identifier> ::= [a-z]
<integer>    ::= "0" | [1-9][0-9]*

## Example (C Engine)
Input:
  f(x)=x^2+2x;
  calculate D(f,x,1);

Output:
  D(f,x,1)=2x+2

---
Developed by Berkay Bilici as a comparative study in Compiler Design.
