import Arbre.Program
import Arbre.Eval

testLiteral = ObjectDef [Def "main" (LiteralExp $ IntegerLit 486)]
testSymref = ObjectDef [
    Def "x" (LiteralExp $ IntegerLit 486),
    Def "main" (Symref "x")
  ]
testApply = ObjectDef [
    Def "main" (Apply (Block ["x"] $ Symref "x") [LiteralExp $ IntegerLit 486])
  ]
testApply2 = ObjectDef [
    Def "x" (LiteralExp $ IntegerLit 486),
    Def "main" (Apply (Block ["y"] $ Symref "y") [Symref "x"])
  ]
testNative = ObjectDef [
    Def "main" (NativeCall "+" [(LiteralExp $ IntegerLit 1), (LiteralExp $ IntegerLit 2)])
  ]
testWrappedNative = ObjectDef [
    Def "main" (Call "+" [(LiteralExp $ IntegerLit 1), (LiteralExp $ IntegerLit 2)])
  ]
testCall = ObjectDef [
    Def "addone" (BlockExp $ Block ["x"] (NativeCall "+" [(LiteralExp $ IntegerLit 1), (Symref "x")])),
    Def "main" (Call "addone" [(LiteralExp $ IntegerLit 2)])
  ]

testAdd = ObjectDef [
    Def "main" (Call "+" [(LiteralExp $ IntegerLit 137), (LiteralExp $ IntegerLit 349)])
  ]
testAdd2 = ObjectDef [
    Def "main" (Call "==" [
        (Call "+" [(LiteralExp $ IntegerLit 137), (LiteralExp $ IntegerLit 349)]),
        (LiteralExp $ IntegerLit 486)
      ]
    )
  ]
testMult = ObjectDef [
    Def "main" (Call "*" [
        (LiteralExp $ IntegerLit 137),
        (LiteralExp $ IntegerLit 349)
      ]
    )
  ]
testNested = ObjectDef [
    Def "main" (Call "==" [
        (Call "*" [
            (Call "+" [
                (LiteralExp $ IntegerLit 2),
                (Call "*" [
                    (LiteralExp $ IntegerLit 4),
                    (LiteralExp $ IntegerLit 6)
                  ])
              ]),
            (Call "+" [
                (Call "+" [
                    (LiteralExp $ IntegerLit 3),
                    (LiteralExp $ IntegerLit 5)
                  ]),
                (LiteralExp $ IntegerLit 7)
              ])
          ]),
          (LiteralExp $ IntegerLit 390)
      ]
    )
  ]
testSquare = ObjectDef [
    Def "square" (BlockExp $ Block ["x"] (
        Call "*" [
            (Symref "x"),
            (Symref "x")
          ]
      )),
    Def "main" (Call "==" [
        (Call "square" [(LiteralExp $ IntegerLit 21)]),
        (LiteralExp $ IntegerLit 441)
      ]
    )
  ]
testSumOfSquares = ObjectDef [
    Def "square" (BlockExp $ Block ["x"] (
        Call "*" [
            (Symref "x"),
            (Symref "x")
          ]
      )),
    Def "sumOfSquares" (BlockExp $ Block ["i", "j"] (
        Call "+" [
            (Call "square" [Symref "i"]),
            (Call "square" [Symref "j"])
          ]
      )),
    Def "main" (Call "==" [
        (Call "sumOfSquares" [LiteralExp $ IntegerLit 4, LiteralExp $ IntegerLit 3]),
        (LiteralExp $ IntegerLit 25)
      ]
    )
  ]

main = do
  evalMainObjectDef testLiteral
  evalMainObjectDef testSymref
  evalMainObjectDef testApply
  evalMainObjectDef testApply2
  evalMainObjectDef testNative
  evalMainObjectDef testWrappedNative
  evalMainObjectDef testCall

  evalMainObjectDef testAdd
  evalMainObjectDef testAdd2
  evalMainObjectDef testMult
  evalMainObjectDef testNested
  evalMainObjectDef testSquare
  evalMainObjectDef testSumOfSquares

{-
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
      ]]],
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
sumOfSquares:
  type: method
  params: [i, j]
  code:
    [func, ["+", [
      [func, [square, [
        [symref, i]
      ]]],
      [func, [square, [
        [symref, j]
      ]]]
    ]]]
testSumOfSquares:
  type: method
  params: []
  code:
    [func, [send, [
      [func, ["==", [
        [func, [sumOfSquares, [
          [literal, 4],
          [literal, 3]
        ]]],
        [literal, 25]
      ]]],
      [symref, io]
    ]]]
abs:
  type: method
  params: [x]
  code:
    [func, [if, [
      [func, ['>', [
        [symref, x],
        [literal, 0]
      ]]],
      [block, {
        type: method,
        params: [],
        code:
          [func, [return, [
            [symref, x]
          ]]]
      }],
      [block, {
        type: method,
        params: [],
        code:
          [func, ["*", [
            [symref, x],
            [literal, -1]
          ]]]
      }]
    ]]]
testIf:
  type: method
  params: []
  code:
    [func, [if, [
      [literal, True],
      [block, {
        type: method,
        params: [],
        code:
          [func, [send, [
            [literal, 'It is true'],
            [symref, io]
          ]]]
      }],
      [block, {
        type: method,
        params: [],
        code:
          [func, [send, [
            [literal, 'It is false'],
            [symref, io]
          ]]]
      }]
    ]]]
testIf2:
  type: method
  params: []
  code:
    [func, [if, [
      [literal, False],
      [block, {
        type: method,
        params: [],
        code:
          [func, [send, [
            [literal, 'It is true'],
            [symref, io]
          ]]]
      }],
      [block, {
        type: method,
        params: [],
        code:
          [func, [send, [
            [literal, 'It is false'],
            [symref, io]
          ]]]
      }]
    ]]]
testAbs:
  type: method
  params: []
  code:
    [func, [send, [
      [func, ["==", [
        [func, [abs, [
          [literal, 5]
        ]]],
        [literal, 5]
      ]]],
      [symref, io]
    ]]]
testAbs2:
  type: method
  params: []
  code:
    [func, [send, [
      [func, ["==", [
        [func, [abs, [
          [literal, -5]
        ]]],
        [literal, 5]
      ]]],
      [symref, io]
    ]]]
testAbs3:
  type: method
  params: []
  code:
    [func, [send, [
      [func, ["==", [
        [func, [abs, [
          [literal, -5]
        ]]],
        [func, [abs, [
          [literal, 5]
        ]]]
      ]]],
      [symref, io]
    ]]]
testAnd:
  type: method
  params: []
  code:
    [func, [send, [
      [func, [and, [
        [literal, true],
        [literal, false]
      ]]],
      [symref, io]
    ]]]
testOr:
  type: method
  params: []
  code:
    [func, [send, [
      [func, [or, [
        [literal, true],
        [literal, false]
      ]]],
      [symref, io]
    ]]]
testNot:
  type: method
  params: []
  code:
    [func, [send, [
      [func, [not, [
        [literal, false]
      ]]],
      [symref, io]
    ]]]
goodenough:
  type: method
  params: [guess, x]
  code:
    [func, ["<", [
      [func, [abs, [
        [func, ["-", [
          [func, [square, [
            [symref, guess]
          ]]],
          [symref, x]
        ]]]
      ]]],
      [literal, 0.001]
    ]]]
average:
  type: method
  params: [x, y]
  code:
    [func, ["/", [
      [func, ["+", [
        [symref, x],
        [symref, y]
      ]]],
      [literal, 2]
    ]]]
improve:
  type: method
  params: [guess, x]
  code:
    [func, [average, [
      [symref, guess],
      [func, ["/", [
        [symref, x],
        [symref, guess]
      ]]]
    ]]]
sqrtiter:
  type: method
  params: [guess, x]
  code:
    [func, [if, [
      [func, [goodenough, [
        [symref, guess],
        [symref, x]
      ]]],
      [block, {
        type: method,
        params: [],
        code:
          [func, [return, [
            [symref, guess]
          ]]]
      }],
      [block, {
        type: method,
        params: [],
        code:
          [func, [sqrtiter, [
            [func, [improve, [
              [symref, guess],
              [symref, x]
            ]]],
            [symref, x]
          ]]]
      }]
    ]]]
sqrt:
  type: method
  params: [x]
  code:
    [func, [sqrtiter, [
      [literal, 1],
      [symref, x]
    ]]]
testSqrt:
  type: method
  params: []
  code:
    [func, [send, [
      [func, [sqrt, [
        [literal, 9]
      ]]],
      [symref, io]
    ]]]
sqrt2:
  goodenough2:
    type: method
    params: [guess, x]
    code:
      [func, ["<", [
        [func, [abs, [
          [func, ["-", [
            [func, [square, [
              [symref, guess]
            ]]],
            [symref, x]
          ]]]
        ]]],
        [literal, 0.001]
      ]]]
  average2:
    type: method
    params: [x, y]
    code:
      [func, ["/", [
        [func, ["+", [
          [symref, x],
          [symref, y]
        ]]],
        [literal, 2]
      ]]]
  improve:
    type: method
    params: [guess, x]
    code:
      [func, [average2, [
        [symref, guess],
        [func, ["/", [
          [symref, x],
          [symref, guess]
        ]]]
      ]]]
  sqrtiter:
    type: method
    params: [guess, x]
    code:
      [func, [if, [
        [func, [goodenough2, [
          [symref, guess],
          [symref, x]
        ]]],
        [block, {
          type: method,
          params: [],
          code:
            [func, [return, [
              [symref, guess]
            ]]]
        }],
        [block, {
          type: method,
          params: [],
          code:
            [func, [sqrtiter2, [
              [func, [improve2, [
                [symref, guess],
                [symref, x]
              ]]],
              [symref, x]
            ]]]
        }]
      ]]]
  type: method
  params: [x]
  code:
    [func, [sqrtiter, [
      [literal, 1],
      [symref, x]
    ]]]
testSqrt2:
  type: method
  params: []
  code:
    [func, [send, [
      [func, [sqrt2, [
        [literal, 9]
      ]]],
      [symref, io]
    ]]]
factorial:
  type: method
  params: [n]
  code:
    [func, [if, [
      [func, ["==", [
        [symref, n],
        [literal, 1]
      ]]],
      [block, {
        type: method,
        params: [],
        code:
          [func, [return, [
            [literal, 1]
          ]]]
      }],
      [block, {
         type: method,
         params: [],
         code:
           [func, ["*", [
             [symref, n],
             [func, [factorial, [
               [func, ["-", [
                 [symref, n],
                 [literal, 1]
                ]]]
              ]]]
            ]]]
      }]
    ]]]
testFac:
  type: method
  params: []
  code:
    [func, [send, [
      [func, [factorial, [
        [literal, 1]
      ]]],
      [symref, io]
    ]]]
testFac2:
  type: method
  params: []
  code:
    [func, [send, [
      [func, [factorial, [
        [literal, 6]
      ]]],
      [symref, io]
    ]]]
fib:
  type: method
  params: [n]
  code:
    [func, [if, [
      [func, ["==", [
        [symref, n],
        [literal, 0]
      ]]],
      [block, {
        type: method,
        params: [],
        code:
          [func, [return, [
            [literal, 0]
          ]]]
      }],
      [block, {
        type: method,
        params: [],
        code:
          [func, [if, [
            [func, ["==", [
              [symref, n],
              [literal, 1]
            ]]],
            [block, {
              type: method,
              params: [],
              code:
                [func, [return, [
                  [literal, 1]
                ]]]
            }],
            [block, {
              type: method,
              params: [],
              code:
                [func, ["+", [
                  [func, [fib, [
                    [func, ["-", [
                      [symref, n],
                      [literal, 1]
                    ]]]
                  ]]],
                  [func, [fib, [
                    [func, ["-", [
                      [symref, n],
                      [literal, 2]
                    ]]]
                  ]]]
                ]]]
            }]
          ]]]
      }]
    ]]]
testFib:
  type: method
  params: []
  code:
    [func, [send, [
      [func, [fib, [
        [literal, 0]
      ]]],
      [symref, io]
    ]]]
testFib2:
  type: method
  params: []
  code:
    [func, [send, [
      [func, [fib, [
        [literal, 1]
      ]]],
      [symref, io]
    ]]]
testFib3:
  type: method
  params: []
  code:
    [func, [send, [
      [func, [fib, [
        [literal, 2]
      ]]],
      [symref, io]
    ]]]
testFib4:
  type: method
  params: []
  code:
    [func, [send, [
      [func, [fib, [
        [literal, 3]
      ]]],
      [symref, io]
    ]]]
testFib5:
  type: method
  params: []
  code:
    [func, [send, [
      [func, [fib, [
        [literal, 10]
      ]]],
      [symref, io]
    ]]]
sum:
  type: method
  params: [term, a, next, b]
  code:
    [func, [if, [
      [func, [">", [
        [symref, a],
        [symref, b]
      ]]],
      [block, {
        type: method,
        params: [],
        code:
          [func, [return, [
            [literal, 0]
          ]]]
      }],
      [block, {
        type: method,
        params: [],
        code:
          [func, ["+", [
            [func, [term, [
              [symref, a]
            ]]],
            [func, [sum, [
              [symref, term],
              [func, [next, [
                [symref, a]
              ]]],
              [symref, next],
              [symref, b]
            ]]]
          ]]]
      }]
    ]]]
piterm:
  type: method
  params: [x]
  code:
    [func, ["/", [
      [literal, 1.0],
      [func, ["*", [
        [symref, x],
        [func, ["+", [
          [symref, x],
          [literal, 2]
        ]]]
      ]]]
    ]]]
pinext:
  type: method
  params: [x]
  code:
    [func, ["+", [
      [symref, x],
      [literal, 4]
    ]]]
pisum:
  type: method
  params: [a, b]
  code:
    [func, [sum, [
      [symref, piterm],
      [symref, a],
      [symref, pinext],
      [symref, b]
    ]]]
inc:
  type: method
  params: [x]
  code:
    [func, ["+", [
      [symref, x],
      [literal, 1]
    ]]]
identity:
  type: method
  params: [x]
  code:
    [func, ["+", [
      [symref, x],
      [literal, 0]
    ]]]
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
-}
