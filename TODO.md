# Todo List

## - Make switchable backend for the compiler
- [ ] Define backend trait/interface
- [ ] Implement at least one backend (e.g., Cranelift)
- [ ] Add mechanism to select backend at runtime or compile-time

## - Create an IR (Intermediate Representation)
- [ ] Design IR data structures
- [ ] Implement translation from AST to IR
- [ ] Add IR validation and debugging tools

## - Update code generation
- [ ] Adapt codegen to work from IR instead of directly from AST
- [ ] Refactor existing codegen logic to fit new architecture
- [ ] Test codegen with different backends using IR