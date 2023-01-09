section .data
	L0 db `\"\npop\npowrot:\ngetstatic java/lang/System/out Ljava/io/PrintStream;\nldc \"zle \"\ninvokevirtual java/io/PrintStream/print(Ljava/lang/String;)V\ngoto powrot\nldc \"`, 0
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
f:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	mov DWORD eax, [ebp+8]
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-12], eax
	mov DWORD eax, [ebp+-4]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, 2
	imul DWORD eax, [ebp+-16]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-12]
	add DWORD eax, [ebp+-16]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-16]
	mov DWORD [ebp+-8], eax
	mov DWORD [ebp+-16], L0
	sub DWORD esp, 12
	push DWORD [ebp+-16]
	call printString
	add DWORD esp, 4
	mov DWORD eax, [ebp+-8]
	mov DWORD [ebp+-16], eax
	mov DWORD eax, [ebp+-16]
	leave
	ret
main:
	push DWORD ebp
	mov DWORD ebp, esp
	sub DWORD esp, 8
	sub DWORD esp, 12
	push DWORD 1
	call f
	add DWORD esp, 4
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+-4]
	sub DWORD eax, 3
	mov DWORD [ebp+-4], eax
	mov DWORD eax, [ebp+-4]
	leave
	ret
