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
fun:
	enter 40, 0
	push QWORD rbx
	push QWORD r12
	push QWORD r13
	push QWORD r14
	push QWORD r15
	mov QWORD [rbp+-8], rdi
	mov QWORD [rbp+-16], rdx
	mov QWORD rax, [rbp+-8]
	mov QWORD rbx, rax
	mov QWORD rax, [rbp+-16]
	mov QWORD r12, rax
	mov QWORD rax, rbx
	add QWORD rax, r12
	mov QWORD r12, rax
	mov QWORD [rbp+-24], rax
	inc QWORD [rbp+-16]
	mov QWORD rax, [rbp+-24]
	mov QWORD r12, rax
	add QWORD rax, 1
	mov QWORD r12, rax
	pop QWORD r15
	pop QWORD r14
	pop QWORD r13
	pop QWORD r12
	pop QWORD rbx
	leave
	ret
main:
	enter 8, 0
	push QWORD rbx
	push QWORD r12
	push QWORD r13
	push QWORD r14
	push QWORD r15
	mov QWORD rdi, 3
	mov QWORD rsi, 2
	mov QWORD rdx, 1
	call fun
	mov QWORD rbx, rax
	mov QWORD rdi, rbx
	call printInt
	mov QWORD rax, 0
	pop QWORD r15
	pop QWORD r14
	pop QWORD r13
	pop QWORD r12
	pop QWORD rbx
	leave
	ret
