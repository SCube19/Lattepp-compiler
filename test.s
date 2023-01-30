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
	mov QWORD rdi, 4
	call __heap
	mov QWORD rbx, rax
	mov QWORD [rbp+-8], rax
	mov QWORD rax, 5
	add QWORD rax, 1
	mov QWORD rbx, rax
	mov QWORD rdi, rbx
	call __heap
	mov QWORD r12, rax
	mov QWORD rcx, r12
	mov QWORD rax, 5
	mov QWORD [rcx+0], rax
	mov QWORD rax, [rbp+-8]
	mov QWORD rbx, rax
	mov QWORD rcx, rbx
	mov QWORD rax, r12
	mov QWORD [rcx+8], rax
	mov QWORD rax, [rbp+-8]
	mov QWORD r12, rax
	mov QWORD rax, [rax+8]
	mov QWORD r12, rax
	mov QWORD rcx, r12
	mov QWORD rax, 123
	mov QWORD rdx, 2
	mov QWORD [rcx+8+rdx*8], rax
	mov QWORD rax, [rbp+-8]
	mov QWORD r12, rax
	mov QWORD rax, [rax+8]
	mov QWORD r12, rax
	mov QWORD rcx, 2
	mov QWORD rax, [rax+8+rcx*8]
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
