import Arbre.Expressions
import Arbre.Box
import Arbre.Eval
import Arbre.Short
import Arbre.NativeTypes
import Arbre.Native

testLiteral = modul [Def "main" (num 486)]
testSymref = modul [
    Def "x" (num 486),
    Def "main" $ self "x"
  ]
testApply = modul [
    Def "main" (Apply (block ["x"] $ Symref Local "x") [num 486])
  ]
testApply2 = modul [
    Def "x" (num 486),
    Def "main" (Apply (block ["y"] $ Symref Local "y") [Symref Self "x"])
  ]
testNative = modul [
    Def "main" (NativeCall IntegerAdd [(num 1), (num 2)])
  ]
testWrappedNative = modul [
    Def "main" (Call (env "+") [(num 1), (num 2)])
  ]
testCall = modul [
    Def "addone" (block ["x"] (NativeCall IntegerAdd [(num 1), (local "x")])),
    Def "main" (Call (self "addone") [(num 2)])
  ]

testAdd = modul [
    Def "main" (Call (env "+") [(num 137), (num 349)])
  ]
testAdd2 = modul [
    Def "main" (Call (env "==") [
        (Call (env "+") [(num 137), (num 349)]),
        (num 486)
      ]
    )
  ]
testMult = modul [
    Def "main" (Call (env "*") [
        (num 137),
        (num 349)
      ]
    )
  ]
testNested = modul [
    Def "main" (Call (env "==") [
        (Call (env "*") [
            (Call (env "+") [
                (num 2),
                (Call (env "*") [
                    (num 4),
                    (num 6)
                  ])
              ]),
            (Call (env "+") [
                (Call (env "+") [
                    (num 3),
                    (num 5)
                  ]),
                (num 7)
              ])
          ]),
          (num 390)
      ]
    )
  ]
testSquare = modul [
    Def "square" (block ["x"] (
        Call (env "*") [
            (local "x"),
            (local "x")
          ]
      )),
    Def "main" (Call (env "==") [
        (Call (self "square") [(num 21)]),
        (num 441)
      ]
    )
  ]
testSumOfSquares = modul [
    Def "square" (block ["x"] (
        Call (env "*") [
            (local "x"),
            (local "x")
          ]
      )),
    Def "sumOfSquares" (block ["i", "j"] (
        Call (env "+") [
            (Call (self "square") [local "i"]),
            (Call (self "square") [local "j"])
          ]
      )),
    Def "main" (Call (env "==") [
        (Call (self "sumOfSquares") [num 4, num 3]),
        (num 25)
      ]
    )
  ]
testAbs = modul [
    Def "abs" (block ["x"] (
        Call (env "if") [
            (Call (env ">") [local "x", num 0]),
            (block [] $ env "x"),
            (block [] $ Call (env "*") [env "x", num (-1)])
          ]
     )),
    Def "main" (Call (env "==") [
        (Call (self "abs") [num 5]),
        (num 5)
      ]
     )
  ]
testAbs2 = modul [
    Def "abs" (block ["x"] (
        Call (env "if") [
            (Call (env ">") [local "x", num 0]),
            (block [] $ env "x"),
            (block [] $ Call (env "*") [env "x", num (-1)])
          ]
     )),
    Def "main" (Call (env "==") [
        (Call (self "abs") [num (-5)]),
        (num 5)
      ]
     )
  ]
testIf = modul [
    Def "truth" true,
    Def "main" (Call (env "==") [
        Call (env "if") [
            (self "truth"),
            (block [] $ num 5),
            (block [] $ num 0)
          ],
        (num 5)
      ]
     )
  ]
testIf2 = modul [
    Def "truth" false,
    Def "main" (Call (env "==") [
        Call (env "if") [
            (self "truth"),
            (block [] $ num 5),
            (block [] $ num 0)
          ],
        (num 0)
      ]
     )
  ]
testAnd = modul [
    Def "main" (Call (env "and") [true, false])
  ]
testAnd2 = modul [
    Def "main" (Call (env "and") [true, true])
  ]
testAnd3 = modul [
    Def "main" (Call (env "and") [false, false])
  ]
testOr = modul [
    Def "main" (Call (env "or") [true, false])
  ]
testOr2 = modul [
    Def "main" (Call (env "or") [true, true])
  ]
testOr3 = modul [
    Def "main" (Call (env "or") [false, false])
  ]
testNot = modul [
    Def "main" (Call (env "not") [true])
  ]
testNot2 = modul [
    Def "main" (Call (env "not") [false])
  ]

testSqrt = modul [
    Def "square" (block ["victim"] (
        Call (env "*f") [
            (local "victim"),
            (local "victim")
          ]
      )),
    Def "abs" (block ["number"] (
        Call (env "if") [
            (Call (env ">f") [local "number", float 0]),
            (block [] $ env "number"),
            (block [] $ Call (env "*f") [env "number", float (-1)])
          ]
     )),
    Def "goodenough" (block ["guess3", "match"] (
        Call (env ">f") [
            (float 0.001),
            (Call (self "abs") [
                (Call (env "-f") [
                    (Call (self "square") [
                        (local "guess3")
                    ]),
                    (local "match")
                ])
           ])
        ]
    )),
    Def "average" (block ["i", "j"]
        (Call (env "/f") [
            (Call (env "+f") [
                (local "i"),
                (local "j")
            ]),
            (float 2)
        ])
       ),
    Def "improve" (block ["guess2", "desired"]
        (Call (self "average") [
            (local "guess2"),
            (Call (env "/f") [
                (local "desired"),
                (local "guess2")
            ])
           ])
        ),
    Def "sqrtiter" (block ["guess1", "goal"]
        (Call (env "if") [
            (Call (self "goodenough") [local "guess1", local "goal"]),
            (block [] $ env "guess1"),
            (block [] $ (Call (self "sqrtiter") [
                    (Call (self "improve") [env "guess1", env "goal"]),
                    (env "goal")
                ]))
           ]
       )),
    Def "sqrt" (block ["target"] (Call (self "sqrtiter") [float 1, local "target"])),
    Def "main" (Call (self "sqrt") [float 9])
  ]

testSqrt2 = modul [
    Def "square" (block ["x"] (
        Call (env "*f") [
            (local "x"),
            (local "x")
          ]
      )),
    Def "abs" (block ["x"] (
        Call (env "if") [
            (Call (env ">f") [local "x", float 0]),
            (block [] $ env "x"),
            (block [] $ Call (env "*f") [env "x", float (-1)])
          ]
     )),
    Def "goodenough" (block ["guess", "x"] (
        Call (env ">f") [
            (float 0.001),
            (Call (self "abs") [
                (Call (env "-f") [
                    (Call (self "square") [
                        (local "guess")
                    ]),
                    (local "x")
                ])
           ])
        ]
    )),
    Def "average" (block ["x", "y"]
        (Call (env "/f") [
            (Call (env "+f") [
                (local "x"),
                (local "y")
            ]),
            (float 2)
        ])
       ),
    Def "improve" (block ["guess", "x"]
        (Call (self "average") [
            (local "guess"),
            (Call (env "/f") [
                (local "x"),
                (local "guess")
            ])
           ])
        ),
    Def "sqrtiter" (block ["guess", "x"]
        (Call (env "if") [
            (Call (self "goodenough") [local "guess", local "x"]),
            (block [] $ env "guess"),
            (block [] $ (Call (self "sqrtiter") [
                    (Call (self "improve") [env "guess", env "x"]),
                    (env "x")
                ]))
           ]
       )),
    Def "sqrt" (block ["x"] (Call (self "sqrtiter") [float 1, local "x"])),
    Def "main" (Call (self "sqrt") [float 9])
  ]

testSqrtIter = modul [
    Def "square" (block ["x"] (
        Call (env "*f") [
            (local "x"),
            (local "x")
          ]
      )),
    Def "abs" (block ["x"] (
        Call (env "if") [
            (Call (env ">f") [local "x", float 0]),
            (block [] $ env "x"),
            (block [] $ Call (env "*f") [env "x", float (-1)])
          ]
     )),
    Def "goodenough" (block ["x"] (
        Call (env ">f") [
            (float 0.001),
            (Call (self "abs") [
                (Call (env "-f") [
                    (Call (self "square") [
                        (dyn "guess")
                    ]),
                    (local "x")
                ])
           ])
        ]
    )),
    Def "average" (block ["x", "y"]
        (Call (env "/f") [
            (Call (env "+f") [
                (local "x"),
                (local "y")
            ]),
            (float 2)
        ])
       ),
    Def "improve" (block ["x"]
        (Call (self "average") [
            (dyn "guess"),
            (Call (env "/f") [
                (local "x"),
                (dyn "guess")
            ])
           ])
        ),
    Def "sqrt" (block ["x"]
        (Call (env "if") [
            (Call (self "goodenough") [local "x"]),
            (block [] $ dyn "guess"),
            (block [] $ set "guess" (Call (self "improve") [env "x"]))
           ])
       ),
    Def "start" (define "guess" (float 1)),
    Def "step" (Call (self "sqrt") [float 1234567891011121])
  ]

testFac = modul [
    Def "fac" (block ["n"]
        (Call (env "if") [
            (Call (env "==") [local "n", num 1]),
            (block [] $ num 1),
            (block [] $ (Call (env "*") [
                env "n",
                (Call (self "fac") [
                    (Call (env "-") [
                        env "n",
                        num 1
                      ])
                  ])
              ]))
          ])
      ),
    Def "main" (Call (self "fac") [num 1])
  ]
testFac2 = modul [
    Def "fac" (block ["n"]
        (Call (env "if") [
            (Call (env "==") [local "n", num 1]),
            (block [] $ num 1),
            (block [] $ (Call (env "*") [
                env "n",
                (Call (self "fac") [
                    (Call (env "-") [
                        env "n",
                        num 1
                      ])
                  ])
              ]))
          ])
      ),
    Def "main" (Call (self "fac") [num 10])
  ]

testFib = modul [
    Def "fib" (block ["n"]
        (Call (env "if") [
            (Call (env "==") [local "n", num 0]),
            (block [] $ num 0),
            (block [] $ (Call (env "if") [
                (Call (env "==") [env "n", num 1]),
                (block [] $ num 1),
                (block [] $ (Call (env "+") [
                    (Call (self "fib") [
                        (Call (env "-") [
                            env "n",
                            num 1
                          ])
                      ]),
                    (Call (self "fib") [
                        (Call (env "-") [
                            env "n",
                            num 2
                          ])
                      ])
                    ]))
              ]))
          ])
      ),
    Def "main" (Call (self "fib") [num 1])
  ]
testFib2 = modul [
    Def "fib" (block ["n"]
        (Call (env "if") [
            (Call (env "==") [local "n", num 0]),
            (block [] $ num 0),
            (block [] $ (Call (env "if") [
                (Call (env "==") [env "n", num 1]),
                (block [] $ num 1),
                (block [] $ (Call (env "+") [
                    (Call (self "fib") [
                        (Call (env "-") [
                            env "n",
                            num 1
                          ])
                      ]),
                    (Call (self "fib") [
                        (Call (env "-") [
                            env "n",
                            num 2
                          ])
                      ])
                    ]))
              ]))
          ])
      ),
    Def "main" (Call (self "fib") [num 7])
  ]

testPiSum = modul [
    Def "sum" (block ["term", "a", "next", "b"]
      (Call (env "if") [
          Call (env ">f") [local "a", local "b"],
          block [] $ float 0,
          block [] $ Call (env "+f") [
              Call (env "term") [env "a"],
              Call (self "sum") [
                  env "term",
                  Call (env "next") [env "a"],
                  env "next",
                  env "b"
                ]
            ]
        ])),
    Def "piterm" (block ["x"]
      (Call (env "/f") [
          float 1,
          Call (env "*f") [
              local "x",
              Call (env "+f") [
                  local "x",
                  float 2
                ]
            ]
        ])
      ),
    Def "pinext" (block ["x"] (Call (env "+f") [local "x", float 4])),
    Def "pisum" (block ["a", "b"]
        (Call (env "*f") [
            float 8,
            Call (self "sum") [self "piterm", local "a", self "pinext", local "b"]
          ])
      ),
    Def "inc" (block ["x"] (Call (env "+f") [local "x", float 1])),
    Def "identity" (block ["x"] (local "x")),
--    Def "main" (Call (self "pisum") [float 1, float 1000])
    Def "main" (Call (self "pisum") [float 1, float 90])
  ]

testPrint = modul [
    Def "print1" (block [] $ Event Print (num 1)),
    Def "loop" (Call (self "print1") [])
  ]

testPrintIntegers = modul [
    Def "printn" (block [] $ Event Print (dyn "n")),
    Def "incn" (block [] $ set "n" (Call (env "+") [dyn "n", num 1])),
    Def "start" (define "n" (num 1)),
    Def "step" (Combine (Call (self "printn") []) (Call (self "incn") []))
  ]

testPrintFib = modul [
    Def "printa" (block [] $ Event Print (dyn "a")),
    Def "fibit" (block [] $ Combine (set "a" (dyn "b")) (set "b" (
        Call (env "+") [dyn "a", dyn "b"]
      ))),
    Def "start" (Combine (define "a" (num 1)) (define "b" (num 1))),
    Def "step" (Combine (Call (self "printa") []) (Call (self "fibit") []))
  ]

testPrintPi = modul [
    Def "printDigit" (block ["d"] $ Event Print (local "d")),
    Def "pi" (block [] $ (Call (env "if") [
        Call (env "<") [ -- 4*q+r-t < n*t
          Call (env "-") [
            Call (env "+") [
              Call (env "*") [num 4, dyn "q"],
              dyn "r"
            ],
            dyn "t"
          ],
          Call (env "*") [dyn "n", dyn "t"]
        ],
        block [] $
        combine [
          Call (self "printDigit") [dyn "n"],
          superset [
            ("q", Call (env "*") [num 10, dyn "q"]),
            ("r", Call (env "*") [
              num 10,
              Call (env "-") [
                dyn "r",
                Call (env "*") [dyn "n", dyn "t"]
              ]
            ]),
            ("n", Call (env "-") [
              Call (env "/") [
                Call (env "*") [
                  num 10,
                  Call (env "+") [
                    dyn "r",
                    Call (env "*") [num 3, dyn "q"]
                  ]
                ],
                dyn "t"
              ],
              Call (env "*") [num 10, dyn "n"]
            ])
          ]
        ],
        block [] $
        superset [
            ("q", Call (env "*") [dyn "q", dyn "k"]),
            ("r", Call (env "*") [
              dyn "l",
              Call (env "+") [
                dyn "r",
                Call (env "*") [num 2, dyn "q"]
              ]
            ]),
            ("t", Call (env "*") [dyn "t", dyn "l"]),
            ("k", Call (env "+") [dyn "k", num 1]),
            ("n", Call (env "/") [
              Call (env "+") [
                Call (env "*") [
                  dyn "q",
                  Call (env "+") [
                    num 2,
                    Call (env "*") [num 7, dyn "k"]
                  ]
                ],
                Call (env "*") [dyn "r", dyn "l"]
              ],
              Call (env "*") [dyn "t", dyn "l"]
            ]),
            ("l", Call (env "+") [dyn "l", num 2])
        ]
      ])),
    Def "start" (superdefine [("q",1),("r",0),("t",1),("k",1),("n",3),("l",3)]),
    Def "step" (Call (self "pi") [])
  ]

testEchoStdin = modul [
    Def "main" (Receiver Stdin (block ["input"] $ Event Print (local "input")))
  ]

testReadline = modul [
    Def "getline" (block ["buffer", "char"] (
     Call (env "if") [
        (Call (env "==s") [local "char", string "\n"]),
        block [] (env "buffer"),
        block [] $ (Receiver Stdin $ block ["input"] (
          Call (self "getline") [
            Call (env "append") [env "buffer", env "char"],
            local "input"
           ]
         ))
      ]
     )),
     Def "readline" (block [] (
       Receiver Stdin (block ["input"] $ (
         Call (self "getline") [string "", local "input"]
        ))
      )),
     Def "main" (Call (self "readline") [])
  ]

testReadline2 = modul [
    Def "getline" (block ["buffer", "char"] (
     Call (env "if") [
        (Call (env "==s") [local "char", string "\n"]),
        block [] (prnt $ env "buffer"),
        block [] $ (Receiver Stdin $ block ["input"] (
          Call (self "getline") [
            Call (env "append") [env "buffer", env "char"],
            local "input"
           ]
         ))
      ]
     )),
     Def "readline" (block [] (
       Receiver Stdin (block ["input"] $ (
         Call (self "getline") [string "", local "input"]
        ))
      )),
     Def "main" (Call (self "readline") [])
  ]

testReadline3 = modul [
    Def "getline" (block ["buffer", "char"] (
     Call (env "if") [
        (Call (env "==s") [local "char", string "\n"]),
        block [] (prnt $ env "char"),
        block [] $ (Combine (prnt $ env "char")
         (stdin (Call (self "getline") [
             Call (env "append") [env "buffer", env "char"],
             local "input"
            ]))
         )
      ]
     )),
     Def "readline" (block [] $ stdin $ Call (self "getline") [
       string "", local "input"
      ]),
     Def "main" (Call (self "readline") [])
  ]

testHelloWorld = modul [
    Def "getline" (block ["char"] (
     Call (env "if") [
        (Call (env "==s") [local "char", string "\n"]),
        block [] $ (Call (local "sayHello") []),
        block [] $ (Combine
           (set "buffer" (Call (env "append") [dyn "buffer", env "char"]))
           (stdin $ block ["input"] (Call (self "getline") [local "input"]))
         )
      ]
     )),
     Def "sayHello" (block ["name"] (
       prnt $ Call (env "append") [
         string "Hello, ",
         local "name"
        ]
      )),
     Def "helloworld" (block [] $ Combine
       (prnt $ string "What is your name?")
       (stdin $ block ["input"] (Call (self "getline") [local "input"]))
      ),
     Def "start" (define "buffer" (boxString "")),
     Def "main" (Call (self "helloworld") [])
  ]

testPrograms = modul [
  Def "main" $ ProgramExp $ Emit $ Event Print $ boxString "hello world"
 ]

testPrograms2 = modul [
  Def "main" (ProgramExp $ Sequence
    (Emit $ Event Print $ boxString "hello")
    (Emit $ Event Print $ boxString "world")
   )
 ]

testPrograms3 = modul [
  Def "helloworld" (block [] $ prnt $ string "hello world"),
  Def "main" $ ProgramExp $ Emit (Call (self "helloworld") [])
 ]

testSuper = modul [
     Def "sayHello" (block ["name"] (
       prnt $ Call (env "append") [
         string "Hello, ",
         local "name"
        ]
      )),
     Def "helloworld" (block [] $ Combine
       (prnt $ string "What is your name?")
       (stdin $ Call (self "sayHello") [local "input"])
      ),
     Def "main" (Call (self "helloworld") [])
 ]

testSuper2 = modul [
     Def "getline" (block ["callback"] (stdin $ Call (env "callback") [local "input"])),
     Def "sayHello" (block ["name"] (
       prnt $ Call (env "append") [
         string "Hello, ",
         local "name"
        ]
      )),
     Def "helloworld" (block [] $ Combine
       (prnt $ string "What is your name?")
       (Call (self "getline") [
         block ["line"] $ Call (self "sayHello") [local "line"]
        ])
      ),
     Def "main" (Call (self "helloworld") [])
 ]

testSuper3 = modul [
    Def "readline" (block ["callback", "buffer"] (stdin $
     (Call (env "if") [
        (Call (env "==s") [local "input", string "\n"]),
        block [] $ (Call (env "callback") [env "buffer"]),
        block [] $ (stdin $ (Call (self "getline") [
          env "callback",
          Call (env "append") [env "buffer", local "input"]
         ]))
      ])
     )),
     Def "getline" (block ["callback"] (Call (self "readline") [local "callback", boxString ""])),
     Def "sayHello" (block ["name"] (
       prnt $ Call (env "append") [
         string "Hello, ",
         local "name"
        ]
      )),
     Def "helloworld" (block [] $ Combine
       (prnt $ string "What is your name?")
       (Call (self "getline") [
         block ["line"] $ Call (self "sayHello") [local "line"]
        ])
      ),
     Def "main" (Call (self "helloworld") [])
 ]
  
main = do {-
  evalMainModule testLiteral
  evalMainModule testSymref
  evalMainModule testApply
  evalMainModule testApply2
  evalMainModule testNative
  evalMainModule testWrappedNative
  evalMainModule testCall

  evalMainModule testAdd
  evalMainModule testAdd2
  evalMainModule testMult
  evalMainModule testNested
  evalMainModule testSquare
  evalMainModule testSumOfSquares
  evalMainModule testAbs
  evalMainModule testAbs2
  evalMainModule testIf
  evalMainModule testIf2
  evalMainModule testAnd
  evalMainModule testAnd2
  evalMainModule testAnd3
  evalMainModule testOr
  evalMainModule testOr2
  evalMainModule testOr3
  evalMainModule testNot
  evalMainModule testNot2
  evalMainModule testSqrt
  evalMainModule testSqrt2
  evalIterModule testSqrtIter
  evalMainModule testFac
  evalMainModule testFac2
  evalMainModule testFib
  evalMainModule testFib2
  evalMainModule testPiSum
  evalEventLoopModule testPrint
  evalEventIterModule testPrintIntegers
  evalEventIterModule testPrintFib
  evalEventIterModule testPrintPi-}
--  evalReceiverEventModule testEchoStdin
--  evalReceiverIterModule testReadline
--  evalReceiverIterModule testReadline2
--  evalReceiverIterModule testReadline3
--  evalReceiverIterModule testHelloWorld
--  evalProgram testPrograms
--  evalProgram testPrograms2
--  evalProgram testPrograms3
--  supereval testSuper
--  supereval testSuper2
  supereval testSuper3

{-
intsum:
  type: method
  params: [a, b]
  code:
    [func, [sum, [
      [symref, identity],
      [symref, a],
      [symref, inc],
      [symref, b]
    ]]]
testIntSum:
  type: method
  params: []
  code:
    [func, [send, [
      [func, [intsum, [
        [literal, 1],
        [literal, 10]
      ]]],
      [symref, io]
    ]]]
testPiSum:
  type: method
  params: []
  code:
    [func, [send, [
      [func, ["*", [
        [literal, 8],
        [func, [pisum, [
          [literal, 1],
          [literal, 1000]
        ]]]
      ]]],
      [symref, io]
    ]]]
scalelist:
  type: method
  params: [items, factor]
  code:
    [func, [for, [
      [symref, items],
      [block, {
        type: method,
        params: [item],
        code:
          [func, ["*", [
            [symref, item],
            [symref, factor]
          ]]]
      }]
    ]]]
testFor:
  type: method
  params: []
  code:
    [func, [send, [
      [func, [scalelist, [
        [literal, [[literal, 1], [literal, 2], [literal, 3], [literal, 4]]],
        [literal, 10]
      ]]],
      [symref, io]
    ]]]
testWith:
  type: method
  params: []
  code:
    [func, [send, [
      [func, [with, [
        [symdef, size],
        [literal, 2],
        [block, {
          type: method,
          params: [],
          code:
            [func, ['+', [
              [symref, size],
              [literal, 0]
            ]]]
        }]
      ]]],0.001
      [symref, io]
    ]]]
testWith2:
  type: method
  params: []
  code:
    [func, [send, [
      [func, [with, [
        [symdef, pi],
        [literal, 3.14159],
        [block, {
          type: method,
          params: [],
          code:
            [func, [with, [
              [symdef, radius],
              [literal, 10],
              [block, {
                type: method,
                params: [],
                code:
                    [func, ['==', [
                      [func, ['*', [
                        [symref, pi],
                        [func, ['*', [
                          [symref, radius],
                          [symref, radius]
                        ]]]
                      ]]],
                      [literal, 314.159]
                    ]]],
              }]
            ]]]
         }]
      ]]],
      [symref, io]
    ]]]        
-}
