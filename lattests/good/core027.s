section .data
	L0 db 'bad', 0
	L1 db 'good', 0
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
f:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	mov DWORD eax, [ebp+8]
	mov DWORD [ebp+-4], eax
	mov DWORD [ebp+-8], L1
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call printString
	add DWORD esp, 4
	leave
	ret
main:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	mov DWORD [ebp+-4], L0
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call f
	add DWORD esp, 4
	mov DWORD eax, 0
	leave
	ret
