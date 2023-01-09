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
	mov DWORD eax, 0
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-8]
	leave
	ret
