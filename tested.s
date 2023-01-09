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
	mov DWORD eax, 10
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 1
	mov DWORD [ebp+-8], eax
	jmp L1
L0:
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-12], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-12]
	imul DWORD eax, [ebp+-16]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-16]
	mov DWORD [ebp+-8], eax
	dec DWORD [ebp+-4]
L1:
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-16], eax
	jg L2
	mov DWORD [ebp+-16], 0
	jmp L3
L2:
	mov DWORD [ebp+-16], 4294967295
L3:
	mov DWORD eax, 0
	cmp DWORD [ebp+-16], eax
	jne L0
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-16], eax
	sub DWORD esp, 12
	push DWORD [ebp+-16]
	call printInt
	add DWORD esp, 4
	sub DWORD esp, 0
	call error
	add DWORD esp, 0
