# **OLIVIA** 

The langauge is inspired by the [Guarded Command Language](https://en.wikipedia.org/wiki/Guarded_Command_Language), defined by Dijkstra.

The purpose of this language is to serve as both a learning exercise of working with Haskell but more importantly my final year project for my undergraduate course in Computer Science. 

The project will undoubtedly be subject to change over the course of the next year. The main goal of my project is to implementation my language as a non-deterministic finite automaton and to offer proof assistance within the REPL over a collection of algorithms. 

### An Olivia Program

Olivia is a weakly typed, dynamically typed language whose data structures, the only data structure in fact is immutable. 

Every program is written not to dissimilar to an imperative Do-While loop. Observe the following program that finds the product of a list of integers. 

```
f := [1, 2, 3, 4, 5]
n, r := 0, 1

Do n != len(f) ->
                  n, r := n + 1, r * f.n
Od
```

Since non-determinisim arrises in GCL, I too aim to implement it into Olivia. Non-determinsim doesn't arrise in simple programs like the above however it does arrise when the program becomes more complex with the inclusion of if-blocks (Selection). 


### Progress 
At the moment, progress is somewhat slow as I had to reimplement my parser. The directory H contains the prototype to Olivia. I hope to have my parser completed as the time of writing and afterwords I will begin to work upon code generation in both Haskell itself and in using LLVM with the use of [Stephen Diehls tutorial on creating compilers in Haskell with LLVM.](http://www.stephendiehl.com/llvm/)


### Packages and Other Dependencies
- GHC 
- [Parsec](http://hackage.haskell.org/package/parsec)
- [LLVM](https://github.com/llvm-hs/llvm-hs/blob/llvm-4/README.md#installing-llvm)






