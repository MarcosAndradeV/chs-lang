# Project TODOs

## chsc/src/chs_ast/hir/mod.rs
- Line 51: Implement macro call handling
- Line 223-225: Implement several expression types:
  - Cast expressions
  - Index expressions
  - Block expressions
- Line 232: Implement syscall expressions

## chsc/src/chs_ast/mir/mod.rs
- Line 264-268: TODO implementation needed (context unclear)
- Line 272: Add error handling for ExprBuilder

## chsc/src/chs_ast/parser.rs
- Line 618: Add support for [<type>; <size>] array types

## chsc/src/chs_codegen/fasm.rs
- Line 350: Implement other size operators ("do be others")

## chsc/src/chs_codegen/qbe_backend.rs
- Line 113: Implement Unreachable terminator
- Line 136: Implement type conversion (generic TODO for type implementation)
- Line 146-147: Multiple implementation TODOs:
  - Operand conversion
  - Move operation
- Line 177-178: Implement constant conversions:
  - Boolean constants
  - Char constants
- Line 193: Implement UnaryOp
- Line 204-211: Implement various Rvalue operations:
  - Call
  - Cast
  - Syscall
  - Index
  - PointerArithmetic
- Line 226-238: Implement binary operations:
  - Sub
  - Mul
  - Div
  - Rem
  - BitXor
  - BitAnd
  - BitOr
  - Shl
  - Shr
  - Eq
- Line 242: Implement comparison instructions for different types
- Line 244-248: Implement comparison operations:
  - Le (Less than or equal)
  - Ne (Not equal)
  - Ge (Greater than or equal)
  - Gt (Greater than)

## chsc/src/chs_lexer/mod.rs
- Line 173: Fix lexer - `lex_string` should handle escape sequences or RawModule should handle escape sequences

## rere.py
- Line 146: Support binary outputs in test comparison