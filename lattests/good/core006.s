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
	mov DWORD eax, 0
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 45
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 36
	mov DWORD [ebp+-12], eax
	neg DWORD [ebp+-12]
	mov DWORD eax, [ebp+-12]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-12], eax
	sub DWORD esp, 12
	push DWORD [ebp+-12]
	call printInt
	add DWORD esp, 4
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-12], eax
	sub DWORD esp, 12
	push DWORD [ebp+-12]
	call printInt
	add DWORD esp, 4
	mov DWORD eax, 0
	leave
	ret
