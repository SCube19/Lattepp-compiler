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
	mov DWORD eax, 10
	add DWORD eax, 22
	mov DWORD [ebp+-20], eax
	mov DWORD eax, [ebp+-20]
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 78
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 4294967295
	mov DWORD [ebp+-12], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-20], eax
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-24], eax
	mov DWORD eax, [ebp+-20]
	add DWORD eax, [ebp+-24]
	mov DWORD [ebp+-24], eax
	mov DWORD eax, [ebp+-24]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, 0
	leave
	ret
x:
	push DWORD ebp
	mov DWORD ebp, esp
	leave
	ret
