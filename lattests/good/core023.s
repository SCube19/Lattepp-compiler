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
foo:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	mov DWORD eax, [ebp+8]
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+12]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+16]
	mov DWORD [ebp+-12], eax
	mov DWORD eax, [ebp+20]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+24]
	mov DWORD [ebp+-20], eax
	mov DWORD eax, [ebp+28]
	mov DWORD [ebp+-24], eax
	mov DWORD eax, [ebp+32]
	mov DWORD [ebp+-28], eax
	mov DWORD eax, [ebp+36]
	mov DWORD [ebp+-32], eax
	mov DWORD eax, [ebp+40]
	mov DWORD [ebp+-36], eax
	mov DWORD eax, [ebp+44]
	mov DWORD [ebp+-40], eax
	mov DWORD eax, [ebp+48]
	mov DWORD [ebp+-44], eax
	mov DWORD eax, [ebp+52]
	mov DWORD [ebp+-48], eax
	mov DWORD eax, [ebp+56]
	mov DWORD [ebp+-52], eax
	mov DWORD eax, [ebp+60]
	mov DWORD [ebp+-56], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-64], eax
	mov DWORD eax, 2
	imul DWORD eax, [ebp+-64]
	mov DWORD [ebp+-64], eax
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-68], eax
	mov DWORD eax, [ebp+-68]
	cdq
	mov DWORD ebx, 2
	idiv DWORD ebx
	mov DWORD [ebp+-68], eax
	mov DWORD eax, [ebp+-64]
	add DWORD eax, [ebp+-68]
	mov DWORD [ebp+-68], eax
	mov DWORD eax, [ebp+-12]
	mov DWORD [ebp+-64], eax
	mov DWORD eax, [ebp+-68]
	add DWORD eax, [ebp+-64]
	mov DWORD [ebp+-64], eax
	mov DWORD eax, [ebp+-16]
	mov DWORD [ebp+-68], eax
	mov DWORD eax, [ebp+-64]
	add DWORD eax, [ebp+-68]
	mov DWORD [ebp+-68], eax
	mov DWORD eax, [ebp+-20]
	mov DWORD [ebp+-64], eax
	mov DWORD eax, [ebp+-68]
	add DWORD eax, [ebp+-64]
	mov DWORD [ebp+-64], eax
	mov DWORD eax, [ebp+-24]
	mov DWORD [ebp+-68], eax
	mov DWORD eax, [ebp+-64]
	add DWORD eax, [ebp+-68]
	mov DWORD [ebp+-68], eax
	mov DWORD eax, [ebp+-28]
	mov DWORD [ebp+-64], eax
	mov DWORD eax, [ebp+-68]
	add DWORD eax, [ebp+-64]
	mov DWORD [ebp+-64], eax
	mov DWORD eax, [ebp+-32]
	mov DWORD [ebp+-68], eax
	mov DWORD eax, [ebp+-64]
	add DWORD eax, [ebp+-68]
	mov DWORD [ebp+-68], eax
	mov DWORD eax, [ebp+-36]
	mov DWORD [ebp+-64], eax
	mov DWORD eax, [ebp+-68]
	add DWORD eax, [ebp+-64]
	mov DWORD [ebp+-64], eax
	mov DWORD eax, [ebp+-40]
	mov DWORD [ebp+-68], eax
	mov DWORD eax, [ebp+-68]
	cdq
	mov DWORD ebx, 2
	idiv DWORD ebx
	mov DWORD [ebp+-68], eax
	mov DWORD eax, [ebp+-64]
	add DWORD eax, [ebp+-68]
	mov DWORD [ebp+-68], eax
	mov DWORD eax, [ebp+-44]
	mov DWORD [ebp+-64], eax
	mov DWORD eax, [ebp+-68]
	add DWORD eax, [ebp+-64]
	mov DWORD [ebp+-64], eax
	mov DWORD eax, [ebp+-48]
	mov DWORD [ebp+-68], eax
	mov DWORD eax, [ebp+-64]
	add DWORD eax, [ebp+-68]
	mov DWORD [ebp+-68], eax
	mov DWORD eax, [ebp+-52]
	mov DWORD [ebp+-64], eax
	mov DWORD eax, [ebp+-68]
	add DWORD eax, [ebp+-64]
	mov DWORD [ebp+-64], eax
	mov DWORD eax, [ebp+-56]
	mov DWORD [ebp+-68], eax
	mov DWORD eax, [ebp+-64]
	add DWORD eax, [ebp+-68]
	mov DWORD [ebp+-68], eax
	mov DWORD eax, [ebp+-68]
	cdq
	mov DWORD ebx, 10
	idiv DWORD ebx
	mov DWORD [ebp+-68], edx
	mov DWORD eax, [ebp+-68]
	mov DWORD [ebp+-60], eax
	mov DWORD eax, [ebp+-60]
	mov DWORD [ebp+-68], eax
	sub DWORD esp, 12
	push DWORD [ebp+-68]
	call printInt
	add DWORD esp, 4
	mov DWORD eax, [ebp+-60]
	mov DWORD [ebp+-68], eax
	mov DWORD eax, [ebp+-68]
	leave
	ret
main:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 72
	mov DWORD eax, 1
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 2
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 1
	mov DWORD [ebp+-12], eax
	mov DWORD eax, 2
	mov DWORD [ebp+-16], eax
	mov DWORD eax, 1
	mov DWORD [ebp+-20], eax
	mov DWORD eax, 2
	mov DWORD [ebp+-24], eax
	mov DWORD eax, 1
	mov DWORD [ebp+-28], eax
	mov DWORD eax, 2
	mov DWORD [ebp+-32], eax
	mov DWORD eax, 1
	mov DWORD [ebp+-36], eax
	mov DWORD eax, 2
	mov DWORD [ebp+-40], eax
	mov DWORD eax, 1
	mov DWORD [ebp+-44], eax
	mov DWORD eax, 2
	mov DWORD [ebp+-48], eax
	mov DWORD eax, 1
	mov DWORD [ebp+-52], eax
	mov DWORD eax, 2
	mov DWORD [ebp+-56], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-60], eax
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-64], eax
	mov DWORD eax, [ebp+-12]
	mov DWORD [ebp+-68], eax
	mov DWORD eax, [ebp+-16]
	mov DWORD [ebp+-72], eax
	mov DWORD eax, [ebp+-20]
	mov DWORD [ebp+-76], eax
	mov DWORD eax, [ebp+-24]
	mov DWORD [ebp+-80], eax
	mov DWORD eax, [ebp+-28]
	mov DWORD [ebp+-84], eax
	mov DWORD eax, [ebp+-32]
	mov DWORD [ebp+-88], eax
	mov DWORD eax, [ebp+-36]
	mov DWORD [ebp+-92], eax
	mov DWORD eax, [ebp+-40]
	mov DWORD [ebp+-96], eax
	mov DWORD eax, [ebp+-44]
	mov DWORD [ebp+-100], eax
	mov DWORD eax, [ebp+-48]
	mov DWORD [ebp+-104], eax
	mov DWORD eax, [ebp+-52]
	mov DWORD [ebp+-108], eax
	mov DWORD eax, [ebp+-56]
	mov DWORD [ebp+-112], eax
	sub DWORD esp, 8
	push DWORD [ebp+-112]
	push DWORD [ebp+-108]
	push DWORD [ebp+-104]
	push DWORD [ebp+-100]
	push DWORD [ebp+-96]
	push DWORD [ebp+-92]
	push DWORD [ebp+-88]
	push DWORD [ebp+-84]
	push DWORD [ebp+-80]
	push DWORD [ebp+-76]
	push DWORD [ebp+-72]
	push DWORD [ebp+-68]
	push DWORD [ebp+-64]
	push DWORD [ebp+-60]
	call foo
	add DWORD esp, 56
	mov DWORD [ebp+-116], eax
	mov DWORD eax, [ebp+-116]
	leave
	ret
