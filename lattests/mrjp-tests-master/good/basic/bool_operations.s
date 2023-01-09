section .data
	L27 db `false`, 0
	L26 db `true`, 0
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
b:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	mov DWORD eax, [ebp+8]
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	je L24
	mov DWORD [ebp+-8], L26
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call printString
	add DWORD esp, 4
	jmp L25
L24:
	mov DWORD [ebp+-8], L27
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call printString
	add DWORD esp, 4
L25:
	leave
	ret
f:
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
	mov DWORD eax, 0
	leave
	ret
main:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	sub DWORD esp, 12
	push DWORD 1
	call t
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	je L0
	sub DWORD esp, 12
	push DWORD 2
	call f
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	jne L1
L0:
	mov DWORD [ebp+-4], 0
	jmp L2
L1:
	mov DWORD [ebp+-4], 4294967295
L2:
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call b
	add DWORD esp, 4
	sub DWORD esp, 12
	push DWORD 3
	call t
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	je L3
	sub DWORD esp, 12
	push DWORD 4
	call t
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	jne L4
L3:
	mov DWORD [ebp+-4], 0
	jmp L5
L4:
	mov DWORD [ebp+-4], 4294967295
L5:
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call b
	add DWORD esp, 4
	sub DWORD esp, 12
	push DWORD 5
	call t
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	jne L7
	sub DWORD esp, 12
	push DWORD 6
	call t
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	jne L7
L6:
	mov DWORD [ebp+-4], 0
	jmp L8
L7:
	mov DWORD [ebp+-4], 4294967295
L8:
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call b
	add DWORD esp, 4
	sub DWORD esp, 12
	push DWORD 7
	call f
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	je L9
	sub DWORD esp, 12
	push DWORD 8
	call t
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	jne L10
L9:
	mov DWORD [ebp+-4], 0
	jmp L11
L10:
	mov DWORD [ebp+-4], 4294967295
L11:
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call b
	add DWORD esp, 4
	sub DWORD esp, 12
	push DWORD 9
	call t
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	je L12
	sub DWORD esp, 12
	push DWORD 10
	call t
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	je L15
	sub DWORD esp, 12
	push DWORD 11
	call t
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	jne L16
L15:
	mov DWORD [ebp+-4], 0
	jmp L17
L16:
	mov DWORD [ebp+-4], 4294967295
L17:
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	jne L13
L12:
	mov DWORD [ebp+-4], 0
	jmp L14
L13:
	mov DWORD [ebp+-4], 4294967295
L14:
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call b
	add DWORD esp, 4
	sub DWORD esp, 12
	push DWORD 12
	call f
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	jne L19
	sub DWORD esp, 12
	push DWORD 13
	call f
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	je L21
	sub DWORD esp, 12
	push DWORD 14
	call t
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	jne L22
L21:
	mov DWORD [ebp+-4], 0
	jmp L23
L22:
	mov DWORD [ebp+-4], 4294967295
L23:
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	jne L19
L18:
	mov DWORD [ebp+-4], 0
	jmp L20
L19:
	mov DWORD [ebp+-4], 4294967295
L20:
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call b
	add DWORD esp, 4
	mov DWORD eax, 0
	leave
	ret
t:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	mov DWORD eax, [ebp+8]
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call f
	add DWORD esp, 4
	mov DWORD [ebp+-12], eax
	mov DWORD eax, [ebp+-12]
	mov DWORD [ebp+-12], eax
	not DWORD [ebp+-12]
	mov DWORD eax, [ebp+-12]
	leave
	ret
