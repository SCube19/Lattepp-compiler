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
	mov DWORD eax, 1
	add DWORD eax, 1
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call printInt
	add DWORD esp, 4
	mov DWORD eax, 0
	leave
	ret
