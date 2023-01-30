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
	enter 8, 0
	push QWORD rbx
	push QWORD r12
	push QWORD r13
	push QWORD r14
	push QWORD r15
	mov QWORD rax, 1
	add QWORD rax, 1
	mov QWORD rbx, rax
	mov QWORD rdi, rbx
	call __heap
	mov QWORD r12, rax
	mov QWORD rax, [rax+0]
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
