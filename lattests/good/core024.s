section .data
	L7 db 'NOOO', 0
	L6 db 'yes', 0
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
e:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	mov DWORD [ebp+-4], L7
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call printString
	add DWORD esp, 4
	mov DWORD eax, 0
	leave
	ret
f:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	mov DWORD eax, [ebp+8]
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+12]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-12], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-16]
	cmp DWORD [ebp+-12], eax
	jg L4
	mov DWORD [ebp+-16], 0
	jmp L5
L4:
	mov DWORD [ebp+-16], 4294967295
L5:
	mov DWORD eax, 0
	cmp DWORD [ebp+-16], eax
	jne L2
	sub DWORD esp, 0
	call e
	add DWORD esp, 0
	mov DWORD [ebp+-16], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-16], eax
	jne L2
L1:
	mov DWORD [ebp+-16], 0
	jmp L3
L2:
	mov DWORD [ebp+-16], 4294967295
L3:
	mov DWORD eax, 0
	cmp DWORD [ebp+-16], eax
	je L0
	mov DWORD [ebp+-16], L6
	sub DWORD esp, 12
	push DWORD [ebp+-16]
	call printString
	add DWORD esp, 4
L0:
	leave
	ret
main:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	sub DWORD esp, 8
	push DWORD 2
	push DWORD 1
	call f
	add DWORD esp, 8
	mov DWORD eax, 0
	leave
	ret
