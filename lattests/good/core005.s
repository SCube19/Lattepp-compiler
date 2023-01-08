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
	mov DWORD eax, 56
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-12], eax
	mov DWORD eax, [ebp+-12]
	add DWORD eax, 45
	mov DWORD [ebp+-12], eax
	mov DWORD eax, 2
	cmp DWORD [ebp+-12], eax
	jle L2
	mov DWORD [ebp+-12], 0
	jmp L3
L2:
	mov DWORD [ebp+-12], 4294967295
L3:
	mov DWORD eax, 0
	cmp DWORD [ebp+-12], eax
	je L0
	mov DWORD eax, 1
	mov DWORD [ebp+-4], eax
	jmp L1
L0:
	mov DWORD eax, 2
	mov DWORD [ebp+-4], eax
L1:
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-12], eax
	sub DWORD esp, 12
	push DWORD [ebp+-12]
	call printInt
	add DWORD esp, 4
	mov DWORD eax, 0
	leave
	ret
