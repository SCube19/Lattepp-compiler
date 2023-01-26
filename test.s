section .data
section .text
global main
extern printInt
extern printString
extern readInt
extern readString
extern error
extern __concat
extern __equals
extern __notequals
foo:
	enter 88, 0
	push QWORD rbx
	push QWORD r12
	push QWORD r13
	push QWORD r14
	push QWORD r15
	mov QWORD [rbp+-8], rdi
	mov QWORD rax, [rbp+-8]
	mov QWORD [rbp+-64], rax
	mov QWORD rax, [rbp+-8]
	mov QWORD [rbp+-72], rax
	mov QWORD rax, [rbp+-64]
	imul QWORD rax, [rbp+-72]
	mov QWORD [rbp+-72], rax
	mov QWORD rax, [rbp+-72]
	mov QWORD [rbp+-16], rax
	mov QWORD rax, [rbp+-8]
	mov QWORD [rbp+-72], rax
	mov QWORD rax, [rbp+-8]
	mov QWORD [rbp+-64], rax
	mov QWORD rax, [rbp+-72]
	add QWORD rax, [rbp+-64]
	mov QWORD [rbp+-64], rax
	mov QWORD rax, [rbp+-64]
	mov QWORD [rbp+-24], rax
	mov QWORD rax, [rbp+-16]
	mov QWORD [rbp+-64], rax
	mov QWORD rax, [rbp+-24]
	mov QWORD [rbp+-72], rax
	mov QWORD rax, [rbp+-64]
	add QWORD rax, [rbp+-72]
	mov QWORD [rbp+-72], rax
	mov QWORD rax, [rbp+-72]
	mov QWORD [rbp+-32], rax
	mov QWORD rax, [rbp+-24]
	mov QWORD [rbp+-72], rax
	mov QWORD rax, [rbp+-16]
	mov QWORD [rbp+-64], rax
	mov QWORD rax, [rbp+-72]
	cqo
	idiv QWORD [rbp+-64]
	mov QWORD [rbp+-64], rax
	mov QWORD rax, [rbp+-64]
	mov QWORD [rbp+-40], rax
	mov QWORD rax, [rbp+-32]
	mov QWORD [rbp+-64], rax
	mov QWORD rax, [rbp+-40]
	mov QWORD [rbp+-72], rax
	mov QWORD rax, [rbp+-64]
	add QWORD rax, [rbp+-72]
	mov QWORD [rbp+-72], rax
	mov QWORD rax, [rbp+-72]
	mov QWORD [rbp+-48], rax
	mov QWORD rax, [rbp+-40]
	mov QWORD [rbp+-72], rax
	mov QWORD rax, [rbp+-48]
	mov QWORD [rbp+-64], rax
	mov QWORD rax, [rbp+-72]
	imul QWORD rax, [rbp+-64]
	mov QWORD [rbp+-64], rax
	mov QWORD rax, [rbp+-64]
	sub QWORD rax, 1
	mov QWORD [rbp+-64], rax
	mov QWORD rax, [rbp+-64]
	mov QWORD [rbp+-56], rax
	mov QWORD rax, [rbp+-56]
	mov QWORD [rbp+-64], rax
	mov QWORD rdi, [rbp+-64]
	call printInt
	add QWORD rsp, 0
	jmp L1
L0:
	mov QWORD rax, [rbp+-16]
	mov QWORD [rbp+-64], rax
	mov QWORD rax, [rbp+-24]
	mov QWORD [rbp+-72], rax
	mov QWORD rax, [rbp+-64]
	add QWORD rax, [rbp+-72]
	mov QWORD [rbp+-72], rax
	mov QWORD rax, [rbp+-72]
	mov QWORD [rbp+-16], rax
	mov QWORD rax, [rbp+-32]
	mov QWORD [rbp+-72], rax
	mov QWORD rax, [rbp+-40]
	mov QWORD [rbp+-64], rax
	mov QWORD rax, [rbp+-72]
	cqo
	idiv QWORD [rbp+-64]
	mov QWORD [rbp+-64], rax
	mov QWORD rax, [rbp+-64]
	mov QWORD [rbp+-32], rax
	mov QWORD rax, [rbp+-16]
	mov QWORD [rbp+-64], rax
	mov QWORD rax, [rbp+-32]
	mov QWORD [rbp+-72], rax
	mov QWORD rax, [rbp+-64]
	imul QWORD rax, [rbp+-72]
	mov QWORD [rbp+-72], rax
	mov QWORD rax, [rbp+-72]
	mov QWORD [rbp+-48], rax
	dec QWORD [rbp+-56]
L1:
	mov QWORD rax, [rbp+-56]
	mov QWORD [rbp+-72], rax
	mov QWORD rax, 0
	cmp QWORD [rbp+-72], rax
	jg L2
	mov QWORD [rbp+-72], 0
	jmp L3
L2:
	mov QWORD [rbp+-72], -1
L3:
	mov QWORD rax, 0
	cmp QWORD [rbp+-72], rax
	jne L0
	mov QWORD rax, [rbp+-16]
	mov QWORD [rbp+-72], rax
	mov QWORD rax, [rbp+-72]
	pop QWORD r15
	pop QWORD r14
	pop QWORD r13
	pop QWORD r12
	pop QWORD rbx
	leave
	ret
main:
	enter 40, 0
	push QWORD rbx
	push QWORD r12
	push QWORD r13
	push QWORD r14
	push QWORD r15
	call readInt
	add QWORD rsp, 0
	mov QWORD [rbp+-8], rax
	mov QWORD rdi, [rbp+-8]
	call foo
	add QWORD rsp, 0
	mov QWORD [rbp+-16], rax
	mov QWORD rdi, [rbp+-16]
	call printInt
	add QWORD rsp, 0
	mov QWORD rax, 0
	pop QWORD r15
	pop QWORD r14
	pop QWORD r13
	pop QWORD r12
	pop QWORD rbx
	leave
	ret
