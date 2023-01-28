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
	enter 8, 0
	push QWORD rbx
	push QWORD r12
	push QWORD r13
	push QWORD r14
	push QWORD r15
	mov QWORD rax, 56
	add QWORD rax, 45
	mov QWORD rbx, rax
	mov QWORD rax, 2
	cmp QWORD rbx, rax
	jle L2
	mov QWORD rbx, 0
	jmp L3
L2:
	mov QWORD rbx, -1
L3:
	mov QWORD rax, 0
	cmp QWORD rbx, rax
	je L0
	jmp L1
L0:
L1:
	mov QWORD rdi, 0
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
