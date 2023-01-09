section .data
	L3 db `ahoj`, 0
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
	sub DWORD esp, 0
	call print
	add DWORD esp, 0
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	je L0
	mov DWORD eax, 0
	mov DWORD ebx, 0
	cmp DWORD ebx, eax
	jne L1
L0:
	mov DWORD [ebp+-4], 0
	jmp L2
L1:
	mov DWORD [ebp+-4], 4294967295
L2:
	mov DWORD eax, 0
	leave
	ret
print:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	mov DWORD [ebp+-4], L3
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call printString
	add DWORD esp, 4
	mov DWORD eax, 4294967295
	leave
	ret
