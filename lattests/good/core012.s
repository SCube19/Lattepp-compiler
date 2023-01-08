section .data
	L5 db ' ', 0
	L6 db 'concatenation', 0
	L10 db 'false', 0
	L4 db 'string', 0
	L9 db 'true', 0
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
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 24
	mov DWORD eax, 56
	mov DWORD [ebp+-4], eax
	mov DWORD [ebp+-12], 23
	neg DWORD [ebp+-12]
	mov DWORD eax, [ebp+-12]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-12], eax
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-12]
	add DWORD eax, [ebp+-16]
	mov DWORD [ebp+-16], eax
	sub DWORD esp, 12
	push DWORD [ebp+-16]
	call printInt
	add DWORD esp, 4
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-12], eax
	mov DWORD eax, [ebp+-16]
	sub DWORD eax, [ebp+-12]
	mov DWORD [ebp+-12], eax
	sub DWORD esp, 12
	push DWORD [ebp+-12]
	call printInt
	add DWORD esp, 4
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-12], eax
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-12]
	imul DWORD eax, [ebp+-16]
	mov DWORD [ebp+-16], eax
	sub DWORD esp, 12
	push DWORD [ebp+-16]
	call printInt
	add DWORD esp, 4
	mov DWORD eax, 45
	cdq
	mov DWORD ebx, 2
	idiv DWORD ebx
	mov DWORD [ebp+-16], eax
	sub DWORD esp, 12
	push DWORD [ebp+-16]
	call printInt
	add DWORD esp, 4
	mov DWORD eax, 78
	cdq
	mov DWORD ebx, 3
	idiv DWORD ebx
	mov DWORD [ebp+-16], edx
	sub DWORD esp, 12
	push DWORD [ebp+-16]
	call printInt
	add DWORD esp, 4
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-12], eax
	mov DWORD eax, [ebp+-16]
	sub DWORD eax, [ebp+-12]
	mov DWORD [ebp+-12], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-20], eax
	mov DWORD eax, [ebp+-16]
	add DWORD eax, [ebp+-20]
	mov DWORD [ebp+-20], eax
	mov DWORD eax, [ebp+-20]
	cmp DWORD [ebp+-12], eax
	jg L0
	mov DWORD [ebp+-20], 0
	jmp L1
L0:
	mov DWORD [ebp+-20], 4294967295
L1:
	sub DWORD esp, 12
	push DWORD [ebp+-20]
	call printBool
	add DWORD esp, 4
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-20], eax
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-12], eax
	mov DWORD eax, [ebp+-20]
	cdq
	idiv DWORD [ebp+-12]
	mov DWORD [ebp+-12], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-20], eax
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-20]
	imul DWORD eax, [ebp+-16]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-16]
	cmp DWORD [ebp+-12], eax
	jle L2
	mov DWORD [ebp+-16], 0
	jmp L3
L2:
	mov DWORD [ebp+-16], 4294967295
L3:
	sub DWORD esp, 12
	push DWORD [ebp+-16]
	call printBool
	add DWORD esp, 4
	mov DWORD [ebp+-16], L4
	mov DWORD [ebp+-12], L5
	sub DWORD esp, 8
	push DWORD [ebp+-12]
	push DWORD [ebp+-16]
	call __concat
	add DWORD esp, 8
	mov DWORD [ebp+-20], eax
	mov DWORD [ebp+-12], L6
	sub DWORD esp, 8
	push DWORD [ebp+-12]
	push DWORD [ebp+-20]
	call __concat
	add DWORD esp, 8
	mov DWORD [ebp+-16], eax
	sub DWORD esp, 12
	push DWORD [ebp+-16]
	call printString
	add DWORD esp, 4
	mov DWORD eax, 0
	leave
	ret
printBool:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	mov DWORD eax, [ebp+8]
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	je L7
	mov DWORD [ebp+-8], L9
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call printString
	add DWORD esp, 4
	leave
	ret
	jmp L8
L7:
	mov DWORD [ebp+-8], L10
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call printString
	add DWORD esp, 4
	leave
	ret
L8:
	leave
	ret
