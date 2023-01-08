section .data
	L26 db '!', 0
	L0 db '&&', 0
	L29 db 'false', 0
	L30 db 'true', 0
	L13 db '||', 0
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
	mov DWORD [ebp+-4], L0
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call printString
	add DWORD esp, 4
	mov DWORD [ebp+-4], 1
	neg DWORD [ebp+-4]
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call test
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	je L1
	sub DWORD esp, 12
	push DWORD 0
	call test
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	jne L2
L1:
	mov DWORD [ebp+-8], 0
	jmp L3
L2:
	mov DWORD [ebp+-8], 4294967295
L3:
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call printBool
	add DWORD esp, 4
	mov DWORD [ebp+-8], 2
	neg DWORD [ebp+-8]
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call test
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	je L4
	sub DWORD esp, 12
	push DWORD 1
	call test
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	jne L5
L4:
	mov DWORD [ebp+-4], 0
	jmp L6
L5:
	mov DWORD [ebp+-4], 4294967295
L6:
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call printBool
	add DWORD esp, 4
	sub DWORD esp, 12
	push DWORD 3
	call test
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	je L7
	mov DWORD [ebp+-4], 5
	neg DWORD [ebp+-4]
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call test
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	jne L8
L7:
	mov DWORD [ebp+-8], 0
	jmp L9
L8:
	mov DWORD [ebp+-8], 4294967295
L9:
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call printBool
	add DWORD esp, 4
	sub DWORD esp, 12
	push DWORD 234234
	call test
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	je L10
	sub DWORD esp, 12
	push DWORD 21321
	call test
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	jne L11
L10:
	mov DWORD [ebp+-8], 0
	jmp L12
L11:
	mov DWORD [ebp+-8], 4294967295
L12:
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call printBool
	add DWORD esp, 4
	mov DWORD [ebp+-8], L13
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call printString
	add DWORD esp, 4
	mov DWORD [ebp+-8], 1
	neg DWORD [ebp+-8]
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call test
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	jne L15
	sub DWORD esp, 12
	push DWORD 0
	call test
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	jne L15
L14:
	mov DWORD [ebp+-4], 0
	jmp L16
L15:
	mov DWORD [ebp+-4], 4294967295
L16:
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call printBool
	add DWORD esp, 4
	mov DWORD [ebp+-4], 2
	neg DWORD [ebp+-4]
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call test
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	jne L18
	sub DWORD esp, 12
	push DWORD 1
	call test
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	jne L18
L17:
	mov DWORD [ebp+-8], 0
	jmp L19
L18:
	mov DWORD [ebp+-8], 4294967295
L19:
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call printBool
	add DWORD esp, 4
	sub DWORD esp, 12
	push DWORD 3
	call test
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	jne L21
	mov DWORD [ebp+-8], 5
	neg DWORD [ebp+-8]
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call test
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	jne L21
L20:
	mov DWORD [ebp+-4], 0
	jmp L22
L21:
	mov DWORD [ebp+-4], 4294967295
L22:
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call printBool
	add DWORD esp, 4
	sub DWORD esp, 12
	push DWORD 234234
	call test
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	jne L24
	sub DWORD esp, 12
	push DWORD 21321
	call test
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-4], eax
	jne L24
L23:
	mov DWORD [ebp+-4], 0
	jmp L25
L24:
	mov DWORD [ebp+-4], 4294967295
L25:
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call printBool
	add DWORD esp, 4
	mov DWORD [ebp+-4], L26
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call printString
	add DWORD esp, 4
	sub DWORD esp, 12
	push DWORD 4294967295
	call printBool
	add DWORD esp, 4
	sub DWORD esp, 12
	push DWORD 0
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
	not DWORD [ebp+-8]
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	je L27
	mov DWORD [ebp+-8], L29
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call printString
	add DWORD esp, 4
	jmp L28
L27:
	mov DWORD [ebp+-8], L30
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call printString
	add DWORD esp, 4
L28:
	leave
	ret
	leave
	ret
test:
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
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	jg L31
	mov DWORD [ebp+-8], 0
	jmp L32
L31:
	mov DWORD [ebp+-8], 4294967295
L32:
	mov DWORD eax, [ebp+-8]
	leave
	ret
