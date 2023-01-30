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
extern __heap
main:
	enter 24, 0
	push QWORD rbx
	push QWORD r12
	push QWORD r13
	push QWORD r14
	push QWORD r15
	mov QWORD rdi, 5
	call __heap
	mov QWORD rbx, rax
	mov QWORD [rbp+-8], rax
	mov QWORD rbx, rax
	mov QWORD rcx, rbx
	mov QWORD rax, 112
	mov QWORD [rcx+32], rax
	mov QWORD rax, [rbp+-8]
	mov QWORD rbx, rax
	mov QWORD rax, [rax+32]
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
