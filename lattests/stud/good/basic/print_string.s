section .data
	L0 db `abc`, 0
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
	mov DWORD [ebp+-4], L0
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call printString
	add DWORD esp, 4
	mov DWORD eax, 0
	leave
	ret
