
extern sum


[section .text]	;代码段

global _start			;开始点
global myprintf			;调用函数

_start:

	call sum
	add	esp, 8		

	mov	ebx, 0
	mov	eax, 1		
	int	0x80		; 系统中断


myprintf:
	mov	edx, [esp + 8]	; len
	mov	ecx, [esp + 4]	; msg
	mov	ebx, 1
	mov	eax, 4		
	int	0x80		; 系统中断
	ret
	
