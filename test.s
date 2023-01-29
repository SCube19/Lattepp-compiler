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
main:
	enter 40, 0
	push QWORD rbx
	push QWORD r12
	push QWORD r13
	push QWORD r14
	push QWORD r15
	mov QWORD rax, 0
	mov QWORD [rbp+-8], rax
	mov QWORD rax, 56
	mov QWORD [rbp+-16], rax
	mov QWORD rbx, rax
	add QWORD rax, 45
	mov QWORD rbx, rax
	cmp QWORD rbx, 2
	jle L2
	mov QWORD rbx, 0
	jmp L3
L2:
	mov QWORD rbx, -1
L3:
	cmp QWORD rbx, 0
	je L0
	mov QWORD rax, 1
	mov QWORD [rbp+-8], rax
	jmp L1
L0:
	mov QWORD rax, 2
	mov QWORD [rbp+-8], rax
L1:
	mov QWORD rax, [rbp+-8]
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
