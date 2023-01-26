	L26 db `!`, 0
	L0 db `&&`, 0
	L29 db `false`, 0
	L30 db `true`, 0
	L13 db `||`, 0
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
	mov QWORD rbx, L0
	mov QWORD rdi, rbx
	call printString
	mov QWORD rdi, -1
	call test
	mov QWORD rbx, rax
	mov QWORD rax, 0
	cmp QWORD rbx, rax
	je L2
	mov QWORD rdi, 0
	call test
	mov QWORD rbx, rax
	mov QWORD rax, 0
	cmp QWORD rbx, rax
	jne L2
L2:
	jne L1
	mov QWORD rbx, 0
	jmp L3
L1:
	mov QWORD rbx, -1
L3:
	mov QWORD rdi, rbx
	call printBool
	mov QWORD rdi, -2
	call test
	mov QWORD rbx, rax
	mov QWORD rax, 0
	cmp QWORD rbx, rax
	je L5
	mov QWORD rdi, 1
	call test
	mov QWORD rbx, rax
	mov QWORD rax, 0
	cmp QWORD rbx, rax
	jne L5
L5:
	jne L4
	mov QWORD rbx, 0
	jmp L6
L4:
	mov QWORD rbx, -1
L6:
	mov QWORD rdi, rbx
	call printBool
	mov QWORD rdi, 3
	call test
	mov QWORD rbx, rax
	mov QWORD rax, 0
	cmp QWORD rbx, rax
	je L8
	mov QWORD rdi, -5
	call test
	mov QWORD rbx, rax
	mov QWORD rax, 0
	cmp QWORD rbx, rax
	jne L8
L8:
	jne L7
	mov QWORD rbx, 0
	jmp L9
L7:
	mov QWORD rbx, -1
L9:
	mov QWORD rdi, rbx
	call printBool
	mov QWORD rdi, 234234
	call test
	mov QWORD rbx, rax
	mov QWORD rax, 0
	cmp QWORD rbx, rax
	je L11
	mov QWORD rdi, 21321
	call test
	mov QWORD rbx, rax
	mov QWORD rax, 0
	cmp QWORD rbx, rax
	jne L11
L11:
	jne L10
	mov QWORD rbx, 0
	jmp L12
L10:
	mov QWORD rbx, -1
L12:
	mov QWORD rdi, rbx
	call printBool
	mov QWORD rbx, L13
	mov QWORD rdi, rbx
	call printString
	mov QWORD rdi, -1
	call test
	mov QWORD rbx, rax
	mov QWORD rax, 0
	cmp QWORD rbx, rax
	jne L14
	mov QWORD rdi, 0
	call test
	mov QWORD rbx, rax
	mov QWORD rax, 0
	cmp QWORD rbx, rax
L14:
	jne L15
	mov QWORD rbx, 0
	jmp L16
L15:
	mov QWORD rbx, -1
L16:
	mov QWORD rdi, rbx
	call printBool
	mov QWORD rdi, -2
	call test
	mov QWORD rbx, rax
	mov QWORD rax, 0
	cmp QWORD rbx, rax
	jne L17
	mov QWORD rdi, 1
	call test
	mov QWORD rbx, rax
	mov QWORD rax, 0
	cmp QWORD rbx, rax
L17:
	jne L18
	mov QWORD rbx, 0
	jmp L19
L18:
	mov QWORD rbx, -1
L19:
	mov QWORD rdi, rbx
	call printBool
	mov QWORD rdi, 3
	call test
	mov QWORD rbx, rax
	mov QWORD rax, 0
	cmp QWORD rbx, rax
	jne L20
	mov QWORD rdi, -5
	call test
	mov QWORD rbx, rax
	mov QWORD rax, 0
	cmp QWORD rbx, rax
L20:
	jne L21
	mov QWORD rbx, 0
	jmp L22
L21:
	mov QWORD rbx, -1
L22:
	mov QWORD rdi, rbx
	call printBool
	mov QWORD rdi, 234234
	call test
	mov QWORD rbx, rax
	mov QWORD rax, 0
	cmp QWORD rbx, rax
	jne L23
	mov QWORD rdi, 21321
	call test
	mov QWORD rbx, rax
	mov QWORD rax, 0
	cmp QWORD rbx, rax
L23:
	jne L24
	mov QWORD rbx, 0
	jmp L25
L24:
	mov QWORD rbx, -1
L25:
	mov QWORD rdi, rbx
	call printBool
	mov QWORD rbx, L26
	mov QWORD rdi, rbx
	call printString
	mov QWORD rdi, -1
	call printBool
	mov QWORD rdi, 0
	call printBool
	mov QWORD rax, 0
	pop QWORD r15
	pop QWORD r14
	pop QWORD r13
	pop QWORD r12
	pop QWORD rbx
	leave
	ret
printBool:
	enter 24, 0
	push QWORD rbx
	push QWORD r12
	push QWORD r13
	push QWORD r14
	push QWORD r15
	mov QWORD [rbp+-8], rdi
	not QWORD rbx
	mov QWORD rax, 0
	cmp QWORD rbx, rax
	je L27
	mov QWORD rbx, L29
	mov QWORD rdi, rbx
	call printString
	jmp L28
L27:
	mov QWORD rbx, L30
	mov QWORD rdi, rbx
	call printString
L28:
	pop QWORD r15
	pop QWORD r14
	pop QWORD r13
	pop QWORD r12
	pop QWORD rbx
	leave
	ret
	pop QWORD r15
	pop QWORD r14
	pop QWORD r13
	pop QWORD r12
	pop QWORD rbx
	leave
	ret
test:
	enter 24, 0
	push QWORD rbx
	push QWORD r12
	push QWORD r13
	push QWORD r14
	push QWORD r15
	mov QWORD [rbp+-8], rdi
	mov QWORD rax, [rbp+-8]
	mov QWORD rbx, rax
	mov QWORD rdi, rbx
	call printInt
	mov QWORD rax, [rbp+-8]
	mov QWORD rbx, rax
	mov QWORD rax, 0
	cmp QWORD rbx, rax
	jg L31
	mov QWORD rbx, 0
	jmp L32
L31:
	mov QWORD rbx, -1
L32:
	mov QWORD rax, rbx
	pop QWORD r15
	pop QWORD r14
	pop QWORD r13
	pop QWORD r12
	pop QWORD rbx
	leave
	ret
