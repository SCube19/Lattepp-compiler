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
	mov DWORD ebx, eax
	mov DWORD eax, 10
	add DWORD eax, ebx
	mov DWORD ebx, eax
	mov DWORD eax, ebx
	leave
	ret
h:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	mov DWORD eax, [ebp+8]
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD ebx, eax
	mov DWORD eax, 32
	add DWORD eax, ebx
	mov DWORD ebx, eax
	mov DWORD eax, ebx
	leave
	ret
main:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	sub DWORD esp, 12
	push DWORD 8
	push DWORD ecx
	push DWORD edx
	call h
	pop DWORD edx
	pop DWORD ecx
	add DWORD esp, 4
	mov DWORD ebx, eax
	mov DWORD eax, ebx
	imul DWORD eax, 2
	mov DWORD ebx, eax
	mov DWORD eax, ebx
	cdq
	mov DWORD ebx, 2
	idiv DWORD ebx
	mov DWORD ebx, eax
	mov DWORD eax, 12
	add DWORD eax, ebx
	mov DWORD ebx, eax
	sub DWORD esp, 12
	push DWORD 8
	push DWORD ecx
	push DWORD edx
	call h
	pop DWORD edx
	pop DWORD ecx
	add DWORD esp, 4
	mov DWORD ecx, eax
	mov DWORD eax, ebx
	add DWORD eax, ecx
	mov DWORD ecx, eax
	sub DWORD esp, 12
	push DWORD 0
	push DWORD ecx
	push DWORD edx
	call g
	pop DWORD edx
	pop DWORD ecx
	add DWORD esp, 4
	mov DWORD ebx, eax
	mov DWORD eax, ecx
	add DWORD eax, ebx
	mov DWORD ebx, eax
	sub DWORD esp, 12
	push DWORD ebx
	call printInt
	add DWORD esp, 4
	mov DWORD eax, 0
	leave
	ret
