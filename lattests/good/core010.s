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
fac:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	mov DWORD eax, [ebp+8]
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 0
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 0
	mov DWORD [ebp+-12], eax
	mov DWORD eax, 1
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-16]
	mov DWORD [ebp+-12], eax
	jmp L1
L0:
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-12]
	mov DWORD [ebp+-20], eax
	mov DWORD eax, [ebp+-16]
	imul DWORD eax, [ebp+-20]
	mov DWORD [ebp+-20], eax
	mov DWORD eax, [ebp+-20]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-12]
	mov DWORD [ebp+-20], eax
	mov DWORD eax, [ebp+-20]
	sub DWORD eax, 1
	mov DWORD [ebp+-20], eax
	mov DWORD eax, [ebp+-20]
	mov DWORD [ebp+-12], eax
L1:
	mov DWORD eax, [ebp+-12]
	mov DWORD [ebp+-20], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-20], eax
	jg L2
	mov DWORD [ebp+-20], 0
	jmp L3
L2:
	mov DWORD [ebp+-20], 4294967295
L3:
	mov DWORD eax, 0
	cmp DWORD [ebp+-20], eax
	jne L0
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-20], eax
	mov DWORD eax, [ebp+-20]
	leave
	ret
main:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	sub DWORD esp, 12
	push DWORD 5
	call fac
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call printInt
	add DWORD esp, 4
	mov DWORD eax, 0
	leave
	ret
