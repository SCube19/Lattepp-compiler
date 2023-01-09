section .data
	L6 db `jeszcze raz`, 0
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
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 0
	call readInt
	add DWORD esp, 0
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 1
	cmp DWORD [ebp+-8], eax
	je L4
	mov DWORD [ebp+-8], 0
	jmp L5
L4:
	mov DWORD [ebp+-8], 4294967295
L5:
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	je L2
	mov DWORD eax, 0
	leave
	ret
	jmp L3
L2:
	mov DWORD [ebp+-8], L6
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call printString
	add DWORD esp, 4
L3:
L1:
	mov DWORD eax, 0
	mov DWORD ebx, 4294967295
	cmp DWORD ebx, eax
	jne L0
