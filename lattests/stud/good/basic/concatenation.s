section .data
	L0 db `a`, 0
	L1 db `b`, 0
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
	sub DWORD esp, 24
	mov DWORD [ebp+-4], L0
	mov DWORD [ebp+-8], L1
	sub DWORD esp, 8
	push DWORD [ebp+-8]
	push DWORD [ebp+-4]
	call __concat
	add DWORD esp, 8
	mov DWORD [ebp+-12], eax
	sub DWORD esp, 12
	push DWORD [ebp+-12]
	call printString
	add DWORD esp, 4
	mov DWORD eax, 0
	leave
	ret
