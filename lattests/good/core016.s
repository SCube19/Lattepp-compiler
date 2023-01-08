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
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	mov DWORD eax, 17
	mov DWORD [ebp+-4], eax
	jmp L1
L0:
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-8]
	sub DWORD eax, 2
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-4], eax
L1:
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	jg L2
	mov DWORD [ebp+-8], 0
	jmp L3
L2:
	mov DWORD [ebp+-8], 4294967295
L3:
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	jne L0
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	jl L6
	mov DWORD [ebp+-8], 0
	jmp L7
L6:
	mov DWORD [ebp+-8], 4294967295
L7:
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	je L4
	sub DWORD esp, 12
	push DWORD 0
	call printInt
	add DWORD esp, 4
	mov DWORD eax, 0
	leave
	ret
	jmp L5
L4:
	sub DWORD esp, 12
	push DWORD 1
	call printInt
	add DWORD esp, 4
	mov DWORD eax, 0
	leave
	ret
L5:
