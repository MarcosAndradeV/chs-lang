# Project TODOs

## ideas
- [ ] // args: [string] = slice(argc, argv);

## High Priority

- [ ] Add a Module trait to RawModule
- [ ] Fix integer overflow and allow literal to have the default type i32


### Compiler Core (chsc/src/chs_ast)
- **HIR Implementation** (mod.rs):
  - [ ] Line 51: Implement macro call handling
  - [ ] Line 223-225: Expression types needed:
    - [ ] Cast expressions
    - [ ] Index expressions
    - [ ] Block expressions
  - [ ] Line 232: Implement syscall expressions

- **MIR Implementation** (mir/mod.rs):
  - [ ] Line 264-268: Review and clarify implementation requirements
  - [ ] Line 272: Add error handling for ExprBuilder

- **Parser Enhancements** (parser.rs):
  - [ ] Line 618: Add support for array types: [<type>; <size>]

### Code Generation
- **QBE Backend** (chsc/src/chs_codegen/qbe_backend.rs):
  - [ ] Line 113: Implement Unreachable terminator
  - [ ] Line 136: Type conversion implementation
  - [ ] Lines 146-147: Core operations:
    - [ ] Operand conversion
    - [ ] Move operation
  - [ ] Lines 177-178: Constant handling:
    - [ ] Boolean constants
    - [ ] Char constants
  - [ ] Line 193: UnaryOp implementation
  - [ ] Lines 204-211: Rvalue operations:
    - [ ] Call
    - [ ] Cast
    - [ ] Syscall
    - [ ] Index
    - [ ] PointerArithmetic
  - [ ] Lines 226-238: Binary operations:
    - [ ] Sub
    - [ ] Mul
    - [ ] Div
    - [ ] Rem
    - [ ] BitXor
    - [ ] BitAnd
    - [ ] BitOr
    - [ ] Shl
    - [ ] Shr
    - [ ] Eq
  - [ ] Line 242: Type-specific comparison instructions
  - [ ] Lines 244-248: Comparison operations:
    - [ ] Le (Less than or equal)
    - [ ] Ne (Not equal)
    - [ ] Ge (Greater than or equal)
    - [ ] Gt (Greater than)

## Medium Priority

### FASM Backend (chsc/src/chs_codegen/fasm.rs)
- [ ] Line 350: Implement remaining size operators

### Lexer Improvements (chsc/src/chs_lexer/mod.rs)
- [ ] Line 173: Enhance string handling:
  - [ ] Implement escape sequence handling in `lex_string`
  - [ ] Alternative: Handle escape sequences in RawModule

## Testing Infrastructure
- [ ] Line 146 (rere.py): Add binary output comparison support for tests

## New Tasks and Improvements
- [ ] Add comprehensive error reporting system
- [ ] Implement debug information generation
- [ ] Add optimization passes in MIR
- [ ] Create documentation for compiler internals
- [ ] Set up CI/CD pipeline for automated testing
- [ ] Add benchmarking infrastructure
