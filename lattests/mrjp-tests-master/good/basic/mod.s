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
	mov DWORD eax, 5
	cdq
	mov DWORD ebx, 3
	idiv DWORD ebx
	mov DWORD [ebp+-4], edx
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call printInt
	add DWORD esp, 4
	mov DWORD eax, 5
	mov DWORD [ebp+-4], eax
	neg DWORD [ebp+-4]
	mov DWORD eax, [ebp+-4]
	cdq
	mov DWORD ebx, 3
	idiv DWORD ebx
	mov DWORD [ebp+-4], edx
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call printInt
	add DWORD esp, 4
	mov DWORD eax, 0
	leave
	ret
