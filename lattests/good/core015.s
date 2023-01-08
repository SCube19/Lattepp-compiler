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
ev:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	mov DWORD eax, [ebp+8]
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	jg L2
	mov DWORD [ebp+-8], 0
	jmp L3
L2:
	mov DWORD [ebp+-8], 4294967295
L3:
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	je L0
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-8]
	sub DWORD eax, 2
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call ev
	add DWORD esp, 4
	mov DWORD [ebp+-12], eax
	mov DWORD eax, [ebp+-12]
	leave
	ret
	jmp L1
L0:
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-12], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-12], eax
	jl L6
	mov DWORD [ebp+-12], 0
	jmp L7
L6:
	mov DWORD [ebp+-12], 4294967295
L7:
	mov DWORD eax, 0
	cmp DWORD [ebp+-12], eax
	je L4
	mov DWORD eax, 0
	leave
	ret
	jmp L5
L4:
	mov DWORD eax, 1
	leave
	ret
L5:
L1:
main:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	sub DWORD esp, 12
	push DWORD 17
	call ev
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call printInt
	add DWORD esp, 4
	mov DWORD eax, 0
	leave
	ret
