section .data
	L11 db `Expected a non-negative integer, but got:`, 0
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
fibonacci:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	mov DWORD eax, [ebp+8]
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-24], eax
	mov DWORD eax, 1
	cmp DWORD [ebp+-24], eax
	jle L1
	mov DWORD [ebp+-24], 0
	jmp L2
L1:
	mov DWORD [ebp+-24], 4294967295
L2:
	mov DWORD eax, 0
	cmp DWORD [ebp+-24], eax
	je L0
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-24], eax
	mov DWORD eax, [ebp+-24]
	leave
	ret
L0:
	mov DWORD eax, 0
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 1
	mov DWORD [ebp+-12], eax
	mov DWORD eax, 0
	mov DWORD [ebp+-16], eax
	mov DWORD eax, 2
	mov DWORD [ebp+-20], eax
	jmp L4
L3:
	mov DWORD eax, [ebp+-12]
	mov DWORD [ebp+-24], eax
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-28], eax
	mov DWORD eax, [ebp+-24]
	add DWORD eax, [ebp+-28]
	mov DWORD [ebp+-28], eax
	mov DWORD eax, [ebp+-28]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-12]
	mov DWORD [ebp+-28], eax
	mov DWORD eax, [ebp+-28]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-16]
	mov DWORD [ebp+-28], eax
	mov DWORD eax, [ebp+-28]
	mov DWORD [ebp+-12], eax
	inc DWORD [ebp+-20]
L4:
	mov DWORD eax, [ebp+-20]
	mov DWORD [ebp+-28], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-24], eax
	mov DWORD eax, [ebp+-24]
	cmp DWORD [ebp+-28], eax
	jle L5
	mov DWORD [ebp+-24], 0
	jmp L6
L5:
	mov DWORD [ebp+-24], 4294967295
L6:
	mov DWORD eax, 0
	cmp DWORD [ebp+-24], eax
	jne L3
	mov DWORD eax, [ebp+-12]
	mov DWORD [ebp+-24], eax
	mov DWORD eax, [ebp+-24]
	leave
	ret
main:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	sub DWORD esp, 0
	call readInt
	add DWORD esp, 0
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	jge L9
	mov DWORD [ebp+-8], 0
	jmp L10
L9:
	mov DWORD [ebp+-8], 4294967295
L10:
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	je L7
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call fibonacci
	add DWORD esp, 4
	mov DWORD [ebp+-12], eax
	sub DWORD esp, 12
	push DWORD [ebp+-12]
	call printInt
	add DWORD esp, 4
	mov DWORD eax, 0
	leave
	ret
	jmp L8
L7:
	mov DWORD [ebp+-12], L11
	sub DWORD esp, 12
	push DWORD [ebp+-12]
	call printString
	add DWORD esp, 4
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-12], eax
	sub DWORD esp, 12
	push DWORD [ebp+-12]
	call printInt
	add DWORD esp, 4
	mov DWORD eax, 1
	leave
	ret
L8:
