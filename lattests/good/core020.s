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
	sub DWORD esp, 0
	call p
	add DWORD esp, 0
	sub DWORD esp, 12
	push DWORD 1
	call printInt
	add DWORD esp, 4
	mov DWORD eax, 0
	leave
	ret
p:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	leave
	ret
