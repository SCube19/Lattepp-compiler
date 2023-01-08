section .data
	L12 db 'apa', 0
	L36 db 'false', 0
	L35 db 'true', 0
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
dontCallMe:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	mov DWORD eax, [ebp+8]
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call printInt
	add DWORD esp, 4
	mov DWORD eax, 4294967295
	leave
	ret
implies:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	mov DWORD eax, [ebp+8]
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+12]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-12], eax
	not DWORD [ebp+-12]
	mov DWORD eax, 0
	cmp DWORD [ebp+-12], eax
	jne L38
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-12], eax
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-16]
	cmp DWORD [ebp+-12], eax
	je L40
	mov DWORD [ebp+-16], 0
	jmp L41
L40:
	mov DWORD [ebp+-16], 4294967295
L41:
	mov DWORD eax, 0
	cmp DWORD [ebp+-16], eax
	jne L38
L37:
	mov DWORD [ebp+-16], 0
	jmp L39
L38:
	mov DWORD [ebp+-16], 4294967295
L39:
	mov DWORD eax, [ebp+-16]
	leave
	ret
main:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	mov DWORD eax, 4
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-8]
	mov DWORD ebx, 3
	cmp DWORD ebx, eax
	jle L5
	mov DWORD [ebp+-8], 0
	jmp L6
L5:
	mov DWORD [ebp+-8], 4294967295
L6:
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	je L2
	mov DWORD eax, 2
	mov DWORD ebx, 4
	cmp DWORD ebx, eax
	jne L10
	mov DWORD [ebp+-8], 0
	jmp L11
L10:
	mov DWORD [ebp+-8], 4294967295
L11:
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	je L7
	mov DWORD eax, 0
	mov DWORD ebx, 4294967295
	cmp DWORD ebx, eax
	jne L8
L7:
	mov DWORD [ebp+-8], 0
	jmp L9
L8:
	mov DWORD [ebp+-8], 4294967295
L9:
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	jne L3
L2:
	mov DWORD [ebp+-8], 0
	jmp L4
L3:
	mov DWORD [ebp+-8], 4294967295
L4:
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	je L0
	sub DWORD esp, 12
	push DWORD 4294967295
	call printBool
	add DWORD esp, 4
	jmp L1
L0:
	mov DWORD [ebp+-8], L12
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call printString
	add DWORD esp, 4
L1:
	mov DWORD eax, 4294967295
	mov DWORD ebx, 4294967295
	cmp DWORD ebx, eax
	je L16
	mov DWORD [ebp+-8], 0
	jmp L17
L16:
	mov DWORD [ebp+-8], 4294967295
L17:
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	jne L14
	sub DWORD esp, 12
	push DWORD 1
	call dontCallMe
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	jne L14
L13:
	mov DWORD [ebp+-8], 0
	jmp L15
L14:
	mov DWORD [ebp+-8], 4294967295
L15:
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call printBool
	add DWORD esp, 4
	mov DWORD [ebp+-8], 5
	neg DWORD [ebp+-8]
	mov DWORD eax, [ebp+-8]
	mov DWORD ebx, 4
	cmp DWORD ebx, eax
	jl L21
	mov DWORD [ebp+-8], 0
	jmp L22
L21:
	mov DWORD [ebp+-8], 4294967295
L22:
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	je L18
	sub DWORD esp, 12
	push DWORD 2
	call dontCallMe
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	jne L19
L18:
	mov DWORD [ebp+-8], 0
	jmp L20
L19:
	mov DWORD [ebp+-8], 4294967295
L20:
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call printBool
	add DWORD esp, 4
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-8]
	mov DWORD ebx, 4
	cmp DWORD ebx, eax
	je L26
	mov DWORD [ebp+-8], 0
	jmp L27
L26:
	mov DWORD [ebp+-8], 4294967295
L27:
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	je L23
	mov DWORD [ebp+-8], 0
	not DWORD [ebp+-8]
	mov DWORD eax, [ebp+-8]
	mov DWORD ebx, 4294967295
	cmp DWORD ebx, eax
	je L31
	mov DWORD [ebp+-8], 0
	jmp L32
L31:
	mov DWORD [ebp+-8], 4294967295
L32:
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	je L28
	mov DWORD eax, 0
	mov DWORD ebx, 4294967295
	cmp DWORD ebx, eax
	jne L29
L28:
	mov DWORD [ebp+-8], 0
	jmp L30
L29:
	mov DWORD [ebp+-8], 4294967295
L30:
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	jne L24
L23:
	mov DWORD [ebp+-8], 0
	jmp L25
L24:
	mov DWORD [ebp+-8], 4294967295
L25:
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call printBool
	add DWORD esp, 4
	sub DWORD esp, 8
	push DWORD 0
	push DWORD 0
	call implies
	add DWORD esp, 8
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call printBool
	add DWORD esp, 4
	sub DWORD esp, 8
	push DWORD 4294967295
	push DWORD 0
	call implies
	add DWORD esp, 8
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call printBool
	add DWORD esp, 4
	sub DWORD esp, 8
	push DWORD 0
	push DWORD 4294967295
	call implies
	add DWORD esp, 8
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call printBool
	add DWORD esp, 4
	sub DWORD esp, 8
	push DWORD 4294967295
	push DWORD 4294967295
	call implies
	add DWORD esp, 8
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call printBool
	add DWORD esp, 4
	mov DWORD eax, 0
	leave
	ret
printBool:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	mov DWORD eax, [ebp+8]
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	je L33
	mov DWORD [ebp+-8], L35
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call printString
	add DWORD esp, 4
	jmp L34
L33:
	mov DWORD [ebp+-8], L36
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call printString
	add DWORD esp, 4
L34:
	leave
	ret
	leave
	ret
