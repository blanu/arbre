module Arbre.Test
(
    test
)
where

import Arbre.Program

test = ObjectDef [
  ("testLiteral",
    (BlockExp (Block []
      (CallExp (Call "send"
        [
          Literal $ IntL 486
          Literal $ SymrefL "io"
        ]
      ))
    ))
  )
]

{-
testAdd:
  type: method
  params: []
  code:
    [func, [send, [
      [func, ["+", [
        [literal, 137],
        [literal, 349]
      ]]],
      [symref, io]
    ]]]
testAdd2:
  type: method
  params: []
  code:
    [func, [send, [
      [func, ["==", [
        [func, ["+", [
          [literal, 137],
          [literal, 349]
        ]]],
        [literal, 486]
      ]]],
      [symref, io]
    ]]]
testMult:
  type: method
  params: []
  code:
    [func, [send, [
      [func, ["*", [
        [literal, 137],
        [literal, 349]
      ]]],
      [symref, io]
    ]]]
testMult2:
  type: method
  params: []
  code:
    [func, [send, [
      [func, ["==", [
        [func, ["*", [
          [literal, 137],
          [literal, 349]
        ]]],
        [literal, 47813]
      ]]],
      [symref, io]
    ]]]
testEval:
  type: method
  params: []
  code:
    [func, [eval, [
      [block, {
        type: method,
        params: [size],
        code:
          [func, [send, [
            [symref, size],
            [symref, io]
          ]]]
      }],
      [literal, [[literal, 2]]]
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
testNested:
  type: method
  params: []
  code:
    [func, [send, [
      [func, ["==", [
        [func, ['*', [
          [func, ["+", [
            [literal, 2],
            [func, ['*', [
              [literal, 4],
              [literal, 6]
            ]]]
          ]]],
          [func, ["+", [
            [func, ["+", [
              [literal, 3],
              [literal, 5]
            ]]],
            [literal, 7]
          ]]]
        ]]],
        [literal, 390]
      ]]],
      [symref, io]
    ]]]
square:
  type: method
  params: [x]
  code:
    [func, ['*', [
      [symref, x],
      [symref, x]
    ]]]
testSquare:
  type: method
  params: []
  code:
    [func, [send, [
      [func, ["==", [
        [func, [square, [
          [literal, 21]
        ]]],
        [literal, 441]
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
