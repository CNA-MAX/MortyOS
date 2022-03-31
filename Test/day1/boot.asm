	org 07c00h												;起始位07c00h
	mov ax,cs
	mov ds,ax
	mov es,ax
	call DispStr											
	jmp $													;死循环
DispStr:													;字符串调用函数
	mov ax,BootMessage
	mov bp,ax												;串地址
	mov cx,22												;串长度
	mov ax,01301h											;AH = 13,AL = 01h
	mov bx,000ah											;黑底绿字
	mov dl,0
	int 10h													;10h号中断
	ret 
BootMessage:		db "Hi 2022,i am MortyDOS."
times 510 - ($-$$)  db  0							     	;生成一串512字节二进制代码
dw 0xaa55													;结束标志