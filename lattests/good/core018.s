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
main:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 24
	sub DWORD esp, 0
	call readInt
	add DWORD esp, 0
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-16]
	mov DWORD [ebp+-4], eax
	sub DWORD esp, 0
	call readString
	add DWORD esp, 0
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-16]
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 0
	call readString
	add DWORD esp, 0
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-16]
	mov DWORD [ebp+-12], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-16]
	sub DWORD eax, 5
	mov DWORD [ebp+-16], eax
	sub DWORD esp, 12
	push DWORD [ebp+-16]
	call printInt
	add DWORD esp, 4
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-12]
	mov DWORD [ebp+-20], eax
	sub DWORD esp, 8
	push DWORD [ebp+-20]
	push DWORD [ebp+-16]
	call __concat
	add DWORD esp, 8
	mov DWORD [ebp+-24], eax
	sub DWORD esp, 12
	push DWORD [ebp+-24]
	call printString
	add DWORD esp, 4
	mov DWORD eax, 0
	leave
	ret
