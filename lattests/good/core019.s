section .data
	L8 db 'foo', 0
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
	mov DWORD eax, 78
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 1
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-12], eax
	sub DWORD esp, 12
	push DWORD [ebp+-12]
	call printInt
	add DWORD esp, 4
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-12], eax
	sub DWORD esp, 12
	push DWORD [ebp+-12]
	call printInt
	add DWORD esp, 4
	jmp L1
L0:
	dec DWORD [ebp+-4]
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-12], eax
	sub DWORD esp, 12
	push DWORD [ebp+-12]
	call printInt
	add DWORD esp, 4
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-12], eax
	mov DWORD eax, [ebp+-12]
	add DWORD eax, 7
	mov DWORD [ebp+-12], eax
	mov DWORD eax, [ebp+-12]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-12], eax
	sub DWORD esp, 12
	push DWORD [ebp+-12]
	call printInt
	add DWORD esp, 4
L1:
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-12], eax
	mov DWORD eax, 76
	cmp DWORD [ebp+-12], eax
	jg L2
	mov DWORD [ebp+-12], 0
	jmp L3
L2:
	mov DWORD [ebp+-12], 4294967295
L3:
	mov DWORD eax, 0
	cmp DWORD [ebp+-12], eax
	jne L0
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-12], eax
	sub DWORD esp, 12
	push DWORD [ebp+-12]
	call printInt
	add DWORD esp, 4
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-12], eax
	mov DWORD eax, 4
	cmp DWORD [ebp+-12], eax
	jg L6
	mov DWORD [ebp+-12], 0
	jmp L7
L6:
	mov DWORD [ebp+-12], 4294967295
L7:
	mov DWORD eax, 0
	cmp DWORD [ebp+-12], eax
	je L4
	mov DWORD eax, 4
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-12], eax
	sub DWORD esp, 12
	push DWORD [ebp+-12]
	call printInt
	add DWORD esp, 4
	jmp L5
L4:
	mov DWORD [ebp+-12], L8
	sub DWORD esp, 12
	push DWORD [ebp+-12]
	call printString
	add DWORD esp, 4
L5:
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-12], eax
	sub DWORD esp, 12
	push DWORD [ebp+-12]
	call printInt
	add DWORD esp, 4
	mov DWORD eax, 0
	leave
	ret
