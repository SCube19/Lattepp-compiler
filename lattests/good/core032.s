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
	mov DWORD [ebp+-4], 42
	neg DWORD [ebp+-4]
	mov DWORD [ebp+-8], 1
	neg DWORD [ebp+-8]
	mov DWORD eax, [ebp+-4]
	cdq
	idiv DWORD [ebp+-8]
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call printInt
	add DWORD esp, 4
	mov DWORD eax, 0
	leave
	ret
