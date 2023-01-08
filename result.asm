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
	call x
	add DWORD ebp, 0
	mov DWORD [ebp+-4], 0
	mov DWORD eax, [ebp+-4]
	leave
	ret
x:
	push DWORD ebp
	mov DWORD ebp, esp
	leave
	ret
