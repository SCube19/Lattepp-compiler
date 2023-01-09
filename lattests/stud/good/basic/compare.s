section .data
	L3 db `4`, 0
	L10 db `5`, 0
	L17 db `6`, 0
	L24 db `7`, 0
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
	sub DWORD esp, 8
	mov DWORD eax, 1
	mov DWORD ebx, 1
	cmp DWORD ebx, eax
	jle L1
	mov DWORD [ebp+-4], 0
	jmp L2
L1:
	mov DWORD [ebp+-4], 4294967295
L2:
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	je L0
	mov DWORD [ebp+-4], L3
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call printString
	add DWORD esp, 4
L0:
	mov DWORD eax, 1
	mov DWORD ebx, 1
	cmp DWORD ebx, eax
	jge L5
	mov DWORD [ebp+-4], 0
	jmp L6
L5:
	mov DWORD [ebp+-4], 4294967295
L6:
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	je L4
	mov DWORD [ebp+-4], L3
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call printString
	add DWORD esp, 4
L4:
	mov DWORD eax, 1
	mov DWORD ebx, 1
	cmp DWORD ebx, eax
	jg L8
	mov DWORD [ebp+-4], 0
	jmp L9
L8:
	mov DWORD [ebp+-4], 4294967295
L9:
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	je L7
	mov DWORD [ebp+-4], L10
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call printString
	add DWORD esp, 4
L7:
	mov DWORD eax, 1
	mov DWORD ebx, 1
	cmp DWORD ebx, eax
	jl L12
	mov DWORD [ebp+-4], 0
	jmp L13
L12:
	mov DWORD [ebp+-4], 4294967295
L13:
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	je L11
	mov DWORD [ebp+-4], L10
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call printString
	add DWORD esp, 4
L11:
	mov DWORD eax, 2
	mov DWORD ebx, 1
	cmp DWORD ebx, eax
	jl L15
	mov DWORD [ebp+-4], 0
	jmp L16
L15:
	mov DWORD [ebp+-4], 4294967295
L16:
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	je L14
	mov DWORD [ebp+-4], L17
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call printString
	add DWORD esp, 4
L14:
	mov DWORD eax, 1
	mov DWORD ebx, 2
	cmp DWORD ebx, eax
	jg L19
	mov DWORD [ebp+-4], 0
	jmp L20
L19:
	mov DWORD [ebp+-4], 4294967295
L20:
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	je L18
	mov DWORD [ebp+-4], L17
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call printString
	add DWORD esp, 4
L18:
	mov DWORD eax, 2
	mov DWORD ebx, 1
	cmp DWORD ebx, eax
	jg L22
	mov DWORD [ebp+-4], 0
	jmp L23
L22:
	mov DWORD [ebp+-4], 4294967295
L23:
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	je L21
	mov DWORD [ebp+-4], L24
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call printString
	add DWORD esp, 4
L21:
	mov DWORD eax, 1
	mov DWORD ebx, 2
	cmp DWORD ebx, eax
	jl L26
	mov DWORD [ebp+-4], 0
	jmp L27
L26:
	mov DWORD [ebp+-4], 4294967295
L27:
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	je L25
	mov DWORD [ebp+-4], L24
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call printString
	add DWORD esp, 4
L25:
	mov DWORD eax, 0
	leave
	ret
