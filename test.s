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
g:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	mov DWORD eax, [ebp+8]
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 10
	add DWORD eax, [ebp+-8]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-8]
	leave
	ret
h:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	mov DWORD eax, [ebp+8]
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 32
	add DWORD eax, [ebp+-8]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-8]
	leave
	ret
main:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	sub DWORD esp, 12
	push DWORD 8
	call h
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+-4]
	imul DWORD eax, 2
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+-4]
	cdq
	mov DWORD ebx, 2
	idiv DWORD ebx
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 12
	add DWORD eax, [ebp+-4]
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD 8
	call h
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-4]
	add DWORD eax, [ebp+-8]
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD 0
	call g
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+-8]
	add DWORD eax, [ebp+-4]
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call printInt
	add DWORD esp, 4
	mov DWORD eax, 0
	leave
	ret
