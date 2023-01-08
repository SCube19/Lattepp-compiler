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
d:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	mov DWORD eax, 0
	leave
	ret
main:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	sub DWORD esp, 0
	call d
	add DWORD esp, 0
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call s
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 12
	push DWORD [ebp+-4]
	call printInt
	add DWORD esp, 4
	mov DWORD eax, 0
	leave
	ret
s:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	mov DWORD eax, [ebp+8]
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-8]
	add DWORD eax, 1
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-8]
	leave
	ret
