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
f:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	mov DWORD eax, [ebp+8]
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+12]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-12], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-12], eax
	jg L8
	mov DWORD [ebp+-12], 0
	jmp L9
L8:
	mov DWORD [ebp+-12], 4294967295
L9:
	mov DWORD eax, 0
	cmp DWORD [ebp+-12], eax
	je L5
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-12], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-12], eax
	jg L10
	mov DWORD [ebp+-12], 0
	jmp L11
L10:
	mov DWORD [ebp+-12], 4294967295
L11:
	mov DWORD eax, 0
	cmp DWORD [ebp+-12], eax
	jne L6
L5:
	mov DWORD [ebp+-12], 0
	jmp L7
L6:
	mov DWORD [ebp+-12], 4294967295
L7:
	mov DWORD eax, 0
	cmp DWORD [ebp+-12], eax
	jne L3
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-12], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-12], eax
	jl L15
	mov DWORD [ebp+-12], 0
	jmp L16
L15:
	mov DWORD [ebp+-12], 4294967295
L16:
	mov DWORD eax, 0
	cmp DWORD [ebp+-12], eax
	je L12
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-12], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-12], eax
	jl L17
	mov DWORD [ebp+-12], 0
	jmp L18
L17:
	mov DWORD [ebp+-12], 4294967295
L18:
	mov DWORD eax, 0
	cmp DWORD [ebp+-12], eax
	jne L13
L12:
	mov DWORD [ebp+-12], 0
	jmp L14
L13:
	mov DWORD [ebp+-12], 4294967295
L14:
	mov DWORD eax, 0
	cmp DWORD [ebp+-12], eax
	jne L3
L2:
	mov DWORD [ebp+-12], 0
	jmp L4
L3:
	mov DWORD [ebp+-12], 4294967295
L4:
	mov DWORD eax, 0
	cmp DWORD [ebp+-12], eax
	je L0
	mov DWORD eax, 7
	leave
	ret
	jmp L1
L0:
	mov DWORD eax, 42
	leave
	ret
L1:
main:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	mov DWORD [ebp+-4], 1
	neg DWORD [ebp+-4]
	sub DWORD esp, 8
	push DWORD [ebp+-4]
	push DWORD 1
	call f
	add DWORD esp, 8
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call printInt
	add DWORD esp, 4
	mov DWORD eax, 0
	leave
	ret
