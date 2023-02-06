section .data
	L1 dq IntQueue__first,IntQueue__insert,IntQueue__isEmpty,IntQueue__rmFirst,IntQueue__size
	L0 dq Node__getElem,Node__getNext,Node__setElem,Node__setNext
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
extern __heap
IntQueue__first:
	enter 24, 0
	push QWORD rbx
	push QWORD r12
	push QWORD r13
	push QWORD r14
	push QWORD r15
	mov QWORD [rbp+-8], rdi
	mov QWORD rax, [rbp+-8]
	mov QWORD rbx, rax
	mov QWORD rax, [rax+8]
	mov QWORD rbx, rax
	mov QWORD rax, [rax+0]
	mov QWORD rdi, rbx
	call [rax]
	mov QWORD rbx, rax
	pop QWORD r15
	pop QWORD r14
	pop QWORD r13
	pop QWORD r12
	pop QWORD rbx
	leave
	ret
IntQueue__insert:
	enter 40, 0
	push QWORD rbx
	push QWORD r12
	push QWORD r13
	push QWORD r14
	push QWORD r15
	mov QWORD [rbp+-8], rdi
	mov QWORD [rbp+-16], rsi
	mov QWORD rdi, 3
	call __heap
	mov QWORD rbx, rax
	mov QWORD [rax+0], L0
	mov QWORD rax, rbx
	mov QWORD [rbp+-24], rax
	mov QWORD rbx, rax
	mov QWORD rax, [rbp+-16]
	mov QWORD r12, rax
	mov QWORD rax, rbx
	mov QWORD rax, [rax+0]
	add QWORD rax, 16
	mov QWORD rdi, rbx
	mov QWORD rsi, r12
	call [rax]
	mov QWORD rax, [rbp+-8]
	mov QWORD r12, rax
	mov QWORD rax, [rax+0]
	add QWORD rax, 16
	mov QWORD rdi, r12
	call [rax]
	mov QWORD r12, rax
	cmp QWORD r12, 0
	je L4
	mov QWORD rax, [rbp+-24]
	mov QWORD r12, rax
	mov QWORD rax, [rbp+-8]
	mov QWORD rbx, rax
	mov QWORD rcx, rbx
	mov QWORD rax, r12
	mov QWORD [rcx+8], rax
	jmp L5
L4:
	mov QWORD rax, [rbp+-8]
	mov QWORD r12, rax
	mov QWORD rax, [rax+16]
	mov QWORD r12, rax
	mov QWORD rax, [rbp+-24]
	mov QWORD rbx, rax
	mov QWORD rax, r12
	mov QWORD rax, [rax+0]
	add QWORD rax, 24
	mov QWORD rdi, r12
	mov QWORD rsi, rbx
	call [rax]
L5:
	mov QWORD rax, [rbp+-24]
	mov QWORD rbx, rax
	mov QWORD rax, [rbp+-8]
	mov QWORD r12, rax
	mov QWORD rcx, r12
	mov QWORD rax, rbx
	mov QWORD [rcx+16], rax
	pop QWORD r15
	pop QWORD r14
	pop QWORD r13
	pop QWORD r12
	pop QWORD rbx
	leave
	ret
IntQueue__isEmpty:
	enter 24, 0
	push QWORD rbx
	push QWORD r12
	push QWORD r13
	push QWORD r14
	push QWORD r15
	mov QWORD [rbp+-8], rdi
	mov QWORD rax, [rbp+-8]
	mov QWORD rbx, rax
	mov QWORD rax, [rax+8]
	mov QWORD rbx, rax
	cmp QWORD rbx, 0
	je L2
	mov QWORD rbx, 0
	jmp L3
L2:
	mov QWORD rbx, -1
L3:
	mov QWORD rax, rbx
	pop QWORD r15
	pop QWORD r14
	pop QWORD r13
	pop QWORD r12
	pop QWORD rbx
	leave
	ret
IntQueue__rmFirst:
	enter 24, 0
	push QWORD rbx
	push QWORD r12
	push QWORD r13
	push QWORD r14
	push QWORD r15
	mov QWORD [rbp+-8], rdi
	mov QWORD rax, [rbp+-8]
	mov QWORD rbx, rax
	mov QWORD rax, [rax+8]
	mov QWORD rbx, rax
	mov QWORD rax, [rax+0]
	add QWORD rax, 8
	mov QWORD rdi, rbx
	call [rax]
	mov QWORD rbx, rax
	mov QWORD rax, [rbp+-8]
	mov QWORD r12, rax
	mov QWORD rcx, r12
	mov QWORD rax, rbx
	mov QWORD [rcx+8], rax
	pop QWORD r15
	pop QWORD r14
	pop QWORD r13
	pop QWORD r12
	pop QWORD rbx
	leave
	ret
IntQueue__size:
	enter 40, 0
	push QWORD rbx
	push QWORD r12
	push QWORD r13
	push QWORD r14
	push QWORD r15
	mov QWORD [rbp+-8], rdi
	mov QWORD rax, [rbp+-8]
	mov QWORD rbx, rax
	mov QWORD rax, [rax+8]
	mov QWORD rbx, rax
	mov QWORD [rbp+-16], rax
	mov QWORD rax, 0
	mov QWORD [rbp+-24], rax
	jmp L7
L6:
	mov QWORD rax, [rbp+-16]
	mov QWORD rbx, rax
	mov QWORD rax, [rax+0]
	add QWORD rax, 8
	mov QWORD rdi, rbx
	call [rax]
	mov QWORD rbx, rax
	mov QWORD [rbp+-16], rax
	inc QWORD [rbp+-24]
L7:
	mov QWORD rax, [rbp+-16]
	mov QWORD rbx, rax
	cmp QWORD rbx, 0
	jne L8
	mov QWORD rbx, 0
	jmp L9
L8:
	mov QWORD rbx, -1
L9:
	cmp QWORD rbx, 0
	jne L6
	mov QWORD rax, [rbp+-24]
	mov QWORD rbx, rax
	pop QWORD r15
	pop QWORD r14
	pop QWORD r13
	pop QWORD r12
	pop QWORD rbx
	leave
	ret
Node__getElem:
	enter 24, 0
	push QWORD rbx
	push QWORD r12
	push QWORD r13
	push QWORD r14
	push QWORD r15
	mov QWORD [rbp+-8], rdi
	mov QWORD rax, [rbp+-8]
	mov QWORD rbx, rax
	mov QWORD rax, [rax+8]
	mov QWORD rbx, rax
	pop QWORD r15
	pop QWORD r14
	pop QWORD r13
	pop QWORD r12
	pop QWORD rbx
	leave
	ret
Node__getNext:
	enter 24, 0
	push QWORD rbx
	push QWORD r12
	push QWORD r13
	push QWORD r14
	push QWORD r15
	mov QWORD [rbp+-8], rdi
	mov QWORD rax, [rbp+-8]
	mov QWORD rbx, rax
	mov QWORD rax, [rax+16]
	mov QWORD rbx, rax
	pop QWORD r15
	pop QWORD r14
	pop QWORD r13
	pop QWORD r12
	pop QWORD rbx
	leave
	ret
Node__setElem:
	enter 40, 0
	push QWORD rbx
	push QWORD r12
	push QWORD r13
	push QWORD r14
	push QWORD r15
	mov QWORD [rbp+-8], rdi
	mov QWORD [rbp+-16], rsi
	mov QWORD rax, [rbp+-16]
	mov QWORD rbx, rax
	mov QWORD rax, [rbp+-8]
	mov QWORD r12, rax
	mov QWORD rcx, r12
	mov QWORD rax, rbx
	mov QWORD [rcx+8], rax
	pop QWORD r15
	pop QWORD r14
	pop QWORD r13
	pop QWORD r12
	pop QWORD rbx
	leave
	ret
Node__setNext:
	enter 40, 0
	push QWORD rbx
	push QWORD r12
	push QWORD r13
	push QWORD r14
	push QWORD r15
	mov QWORD [rbp+-8], rdi
	mov QWORD [rbp+-16], rsi
	mov QWORD rax, [rbp+-16]
	mov QWORD rbx, rax
	mov QWORD rax, [rbp+-8]
	mov QWORD r12, rax
	mov QWORD rcx, r12
	mov QWORD rax, rbx
	mov QWORD [rcx+16], rax
	pop QWORD r15
	pop QWORD r14
	pop QWORD r13
	pop QWORD r12
	pop QWORD rbx
	leave
	ret
f:
	enter 24, 0
	push QWORD rbx
	push QWORD r12
	push QWORD r13
	push QWORD r14
	push QWORD r15
	mov QWORD [rbp+-8], rdi
	mov QWORD rax, [rbp+-8]
	mov QWORD rbx, rax
	mov QWORD rax, [rbp+-8]
	mov QWORD r12, rax
	mov QWORD rax, rbx
	imul QWORD rax, r12
	mov QWORD r12, rax
	add QWORD rax, 3
	mov QWORD r12, rax
	pop QWORD r15
	pop QWORD r14
	pop QWORD r13
	pop QWORD r12
	pop QWORD rbx
	leave
	ret
main:
	enter 24, 0
	push QWORD rbx
	push QWORD r12
	push QWORD r13
	push QWORD r14
	push QWORD r15
	mov QWORD rdi, 3
	call __heap
	mov QWORD rbx, rax
	mov QWORD [rax+0], L1
	mov QWORD rax, rbx
	mov QWORD [rbp+-8], rax
	mov QWORD rbx, rax
	mov QWORD rdi, 3
	call f
	mov QWORD r12, rax
	mov QWORD rax, rbx
	mov QWORD rax, [rax+0]
	add QWORD rax, 8
	mov QWORD rdi, rbx
	mov QWORD rsi, r12
	call [rax]
	mov QWORD rax, [rbp+-8]
	mov QWORD r12, rax
	mov QWORD rax, [rax+0]
	add QWORD rax, 8
	mov QWORD rdi, r12
	mov QWORD rsi, 5
	call [rax]
	mov QWORD rax, [rbp+-8]
	mov QWORD r12, rax
	mov QWORD rax, [rax+0]
	add QWORD rax, 8
	mov QWORD rdi, r12
	mov QWORD rsi, 7
	call [rax]
	mov QWORD rax, [rbp+-8]
	mov QWORD r12, rax
	mov QWORD rax, [rax+0]
	mov QWORD rdi, r12
	call [rax]
	mov QWORD r12, rax
	mov QWORD rdi, r12
	call printInt
	mov QWORD rax, [rbp+-8]
	mov QWORD r12, rax
	mov QWORD rax, [rax+0]
	add QWORD rax, 24
	mov QWORD rdi, r12
	call [rax]
	mov QWORD rax, [rbp+-8]
	mov QWORD r12, rax
	mov QWORD rax, [rax+0]
	add QWORD rax, 32
	mov QWORD rdi, r12
	call [rax]
	mov QWORD r12, rax
	mov QWORD rdi, r12
	call printInt
	mov QWORD rax, 0
	pop QWORD r15
	pop QWORD r14
	pop QWORD r13
	pop QWORD r12
	pop QWORD rbx
	leave
	ret
