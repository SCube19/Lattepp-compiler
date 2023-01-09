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
	jmp L1
L0:
	mov DWORD eax, 0
	leave
	ret
L1:
	mov DWORD eax, 0
	mov DWORD ebx, 4294967295
	cmp DWORD ebx, eax
	jne L0
