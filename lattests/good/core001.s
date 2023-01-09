section .data
	L29 db ``, 0
	L6 db `/* world`, 0
	L4 db `=`, 0
	L5 db `hello */`, 0
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
fac:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	mov DWORD eax, [ebp+8]
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 0
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 0
	mov DWORD [ebp+-12], eax
	mov DWORD eax, 1
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-16]
	mov DWORD [ebp+-12], eax
	jmp L8
L7:
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-12]
	mov DWORD [ebp+-20], eax
	mov DWORD eax, [ebp+-16]
	imul DWORD eax, [ebp+-20]
	mov DWORD [ebp+-20], eax
	mov DWORD eax, [ebp+-20]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-12]
	mov DWORD [ebp+-20], eax
	mov DWORD eax, [ebp+-20]
	sub DWORD eax, 1
	mov DWORD [ebp+-20], eax
	mov DWORD eax, [ebp+-20]
	mov DWORD [ebp+-12], eax
L8:
	mov DWORD eax, [ebp+-12]
	mov DWORD [ebp+-20], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-20], eax
	jg L9
	mov DWORD [ebp+-20], 0
	jmp L10
L9:
	mov DWORD [ebp+-20], 4294967295
L10:
	mov DWORD eax, 0
	cmp DWORD [ebp+-20], eax
	jne L7
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-20], eax
	mov DWORD eax, [ebp+-20]
	leave
	ret
ifac:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	mov DWORD eax, [ebp+8]
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 8
	push DWORD [ebp+-8]
	push DWORD 1
	call ifac2f
	add DWORD esp, 8
	mov DWORD [ebp+-12], eax
	mov DWORD eax, [ebp+-12]
	leave
	ret
ifac2f:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 24
	mov DWORD eax, [ebp+8]
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+12]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-20], eax
	mov DWORD eax, [ebp+-20]
	cmp DWORD [ebp+-16], eax
	je L24
	mov DWORD [ebp+-20], 0
	jmp L25
L24:
	mov DWORD [ebp+-20], 4294967295
L25:
	mov DWORD eax, 0
	cmp DWORD [ebp+-20], eax
	je L23
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-20], eax
	mov DWORD eax, [ebp+-20]
	leave
	ret
L23:
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-20], eax
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-16]
	cmp DWORD [ebp+-20], eax
	jg L27
	mov DWORD [ebp+-16], 0
	jmp L28
L27:
	mov DWORD [ebp+-16], 4294967295
L28:
	mov DWORD eax, 0
	cmp DWORD [ebp+-16], eax
	je L26
	mov DWORD eax, 1
	leave
	ret
L26:
	mov DWORD eax, 0
	mov DWORD [ebp+-12], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-20], eax
	mov DWORD eax, [ebp+-16]
	add DWORD eax, [ebp+-20]
	mov DWORD [ebp+-20], eax
	mov DWORD eax, [ebp+-20]
	cdq
	mov DWORD ebx, 2
	idiv DWORD ebx
	mov DWORD [ebp+-20], eax
	mov DWORD eax, [ebp+-20]
	mov DWORD [ebp+-12], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-20], eax
	mov DWORD eax, [ebp+-12]
	mov DWORD [ebp+-16], eax
	sub DWORD esp, 8
	push DWORD [ebp+-16]
	push DWORD [ebp+-20]
	call ifac2f
	add DWORD esp, 8
	mov DWORD [ebp+-24], eax
	mov DWORD eax, [ebp+-12]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-16]
	add DWORD eax, 1
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-20], eax
	sub DWORD esp, 8
	push DWORD [ebp+-20]
	push DWORD [ebp+-16]
	call ifac2f
	add DWORD esp, 8
	mov DWORD [ebp+-28], eax
	mov DWORD eax, [ebp+-24]
	imul DWORD eax, [ebp+-28]
	mov DWORD [ebp+-28], eax
	mov DWORD eax, [ebp+-28]
	leave
	ret
main:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	sub DWORD esp, 12
	push DWORD 10
	call fac
	add DWORD esp, 4
	mov DWORD [ebp+-16], eax
	sub DWORD esp, 12
	push DWORD [ebp+-16]
	call printInt
	add DWORD esp, 4
	sub DWORD esp, 12
	push DWORD 10
	call rfac
	add DWORD esp, 4
	mov DWORD [ebp+-16], eax
	sub DWORD esp, 12
	push DWORD [ebp+-16]
	call printInt
	add DWORD esp, 4
	sub DWORD esp, 12
	push DWORD 10
	call mfac
	add DWORD esp, 4
	mov DWORD [ebp+-16], eax
	sub DWORD esp, 12
	push DWORD [ebp+-16]
	call printInt
	add DWORD esp, 4
	sub DWORD esp, 12
	push DWORD 10
	call ifac
	add DWORD esp, 4
	mov DWORD [ebp+-16], eax
	sub DWORD esp, 12
	push DWORD [ebp+-16]
	call printInt
	add DWORD esp, 4
	mov DWORD eax, 0
	mov DWORD [ebp+-4], eax
	mov DWORD eax, 10
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 1
	mov DWORD [ebp+-12], eax
	jmp L1
L0:
	mov DWORD eax, [ebp+-12]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-20], eax
	mov DWORD eax, [ebp+-16]
	imul DWORD eax, [ebp+-20]
	mov DWORD [ebp+-20], eax
	mov DWORD eax, [ebp+-20]
	mov DWORD [ebp+-12], eax
	dec DWORD [ebp+-8]
L1:
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-20], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-20], eax
	jg L2
	mov DWORD [ebp+-20], 0
	jmp L3
L2:
	mov DWORD [ebp+-20], 4294967295
L3:
	mov DWORD eax, 0
	cmp DWORD [ebp+-20], eax
	jne L0
	mov DWORD eax, [ebp+-12]
	mov DWORD [ebp+-20], eax
	sub DWORD esp, 12
	push DWORD [ebp+-20]
	call printInt
	add DWORD esp, 4
	mov DWORD [ebp+-20], L4
	sub DWORD esp, 8
	push DWORD 60
	push DWORD [ebp+-20]
	call repStr
	add DWORD esp, 8
	mov DWORD [ebp+-16], eax
	sub DWORD esp, 12
	push DWORD [ebp+-16]
	call printString
	add DWORD esp, 4
	mov DWORD [ebp+-16], L5
	sub DWORD esp, 12
	push DWORD [ebp+-16]
	call printString
	add DWORD esp, 4
	mov DWORD [ebp+-16], L6
	sub DWORD esp, 12
	push DWORD [ebp+-16]
	call printString
	add DWORD esp, 4
	mov DWORD eax, 0
	leave
	ret
mfac:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 24
	mov DWORD eax, [ebp+8]
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	je L17
	mov DWORD [ebp+-8], 0
	jmp L18
L17:
	mov DWORD [ebp+-8], 4294967295
L18:
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	je L15
	mov DWORD eax, 1
	leave
	ret
	jmp L16
L15:
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-12], eax
	mov DWORD eax, [ebp+-12]
	sub DWORD eax, 1
	mov DWORD [ebp+-12], eax
	sub DWORD esp, 12
	push DWORD [ebp+-12]
	call nfac
	add DWORD esp, 4
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-8]
	imul DWORD eax, [ebp+-16]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-16]
	leave
	ret
L16:
nfac:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	mov DWORD eax, [ebp+8]
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	jne L21
	mov DWORD [ebp+-8], 0
	jmp L22
L21:
	mov DWORD [ebp+-8], 4294967295
L22:
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	je L19
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-8]
	sub DWORD eax, 1
	mov DWORD [ebp+-8], eax
	sub DWORD esp, 12
	push DWORD [ebp+-8]
	call mfac
	add DWORD esp, 4
	mov DWORD [ebp+-12], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-12]
	imul DWORD eax, [ebp+-8]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-8]
	leave
	ret
	jmp L20
L19:
	mov DWORD eax, 1
	leave
	ret
L20:
repStr:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 24
	mov DWORD eax, [ebp+8]
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+12]
	mov DWORD [ebp+-8], eax
	mov DWORD [ebp+-20], L29
	mov DWORD eax, [ebp+-20]
	mov DWORD [ebp+-12], eax
	mov DWORD eax, 0
	mov DWORD [ebp+-16], eax
	jmp L31
L30:
	mov DWORD eax, [ebp+-12]
	mov DWORD [ebp+-20], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-24], eax
	sub DWORD esp, 8
	push DWORD [ebp+-24]
	push DWORD [ebp+-20]
	call __concat
	add DWORD esp, 8
	mov DWORD [ebp+-28], eax
	mov DWORD eax, [ebp+-28]
	mov DWORD [ebp+-12], eax
	inc DWORD [ebp+-16]
L31:
	mov DWORD eax, [ebp+-16]
	mov DWORD [ebp+-28], eax
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-24], eax
	mov DWORD eax, [ebp+-24]
	cmp DWORD [ebp+-28], eax
	jl L32
	mov DWORD [ebp+-24], 0
	jmp L33
L32:
	mov DWORD [ebp+-24], 4294967295
L33:
	mov DWORD eax, 0
	cmp DWORD [ebp+-24], eax
	jne L30
	mov DWORD eax, [ebp+-12]
	mov DWORD [ebp+-24], eax
	mov DWORD eax, [ebp+-24]
	leave
	ret
rfac:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 24
	mov DWORD eax, [ebp+8]
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	je L13
	mov DWORD [ebp+-8], 0
	jmp L14
L13:
	mov DWORD [ebp+-8], 4294967295
L14:
	mov DWORD eax, 0
	cmp DWORD [ebp+-8], eax
	je L11
	mov DWORD eax, 1
	leave
	ret
	jmp L12
L11:
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-8], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-12], eax
	mov DWORD eax, [ebp+-12]
	sub DWORD eax, 1
	mov DWORD [ebp+-12], eax
	sub DWORD esp, 12
	push DWORD [ebp+-12]
	call rfac
	add DWORD esp, 4
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-8]
	imul DWORD eax, [ebp+-16]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-16]
	leave
	ret
L12:
