format ELF64 executable
entry _start
segment executable
_start:
	pop rdi
	pop rsi
	call _main
	mov rax, 60
	mov rdi, 0
	syscall
;; function
_print_int:
	mov r9, -3689348814741910323
	sub rsp, 40
	mov BYTE [rsp+31], 10
	lea rcx, [rsp+30]
.L2:
	mov rax, rdi
	lea r8, [rsp+32]
	mul r9
	mov rax, rdi
	sub r8, rcx
	shr rdx, 3
	lea rsi, [rdx+rdx*4]
	add rsi, rsi
	sub rax, rsi
	add eax, 48
	mov BYTE [rcx], al
	mov rax, rdi
	mov rdi, rdx
	mov rdx, rcx
	sub rcx, 1
	cmp rax, 9
	ja  .L2
	lea rax, [rsp+32]
	mov edi, 1
	sub rdx, rax
	xor eax, eax
	lea rsi, [rsp+32+rdx]
	mov rdx, r8
	mov rax, 1
	syscall
	add rsp, 40
	ret
;; end

;; function
_main:
	push rbp
	mov rbp, rsp
	sub rsp, 8
	mov QWORD [rbp-8], 1
	jmp .L1
.L2:
	mov rax, QWORD [rbp-8]
	mov rbx, 3
	div rbx
	mov r12, rdx
	mov rbx, 0
	cmp r12, rbx
	sete al
	mov rdi, rax
	call _print_int
	mov rbx, 1
	add rbx, QWORD [rbp-8]
	mov QWORD [rbp-8], rbx
.L1:
	cmp QWORD [rbp-8], 10
	setg al
	jnz .L2
.L3:
	add rsp, 8
	pop rbp
	ret
;; end

