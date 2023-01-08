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
	mov DWORD eax, 4294967295
	mov DWORD ebx, 4294967295
	cmp DWORD ebx, eax
	je L1
	mov DWORD [ebp+-4], 0
	jmp L2
L1:
	mov DWORD [ebp+-4], 4294967295
L2:
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	je L0
	sub DWORD esp, 12
	push DWORD 42
	call printInt
	add DWORD esp, 4
L0:
	mov DWORD eax, 0
	leave
	ret
