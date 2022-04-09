; Structure example(32 - bit)

.386
.model flat, stdcall
.stack 40960
ExitProcess proto, dwExitCode:dword

.data

.code
main proc
	mov eax, 0
	mov ebx, 0
two:
	mov ecx, 0
three:
	push ebx

	; mul r1 r1 r2
	push eax
	push ecx
	mov eax, ebx
	mul ecx
	mov ebx, eax
	pop ecx
	pop eax


	push ecx
	mov ecx, 3

	push eax
	push ecx
	mov eax, ebx
	mov edx, 0
	div ecx
	mov edx, 0
	mov ebx, eax
	pop ecx
	pop eax

	pop ebx
	add eax, ebx
	push ecx
	mov ecx, 1
	add ebx, ecx
	pop ecx
	push ecx
	mov ecx, 10000
	cmp ebx, ecx
	pop ecx
	jb three
	mov ebx, 1
	add ecx, ebx
	mov ebx, 10000
	cmp ecx, ebx
	mov ebx, 0
	jb two

main endp
end main