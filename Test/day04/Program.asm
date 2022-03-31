;********************************************
;BIOS里面的512字节过于小，完全无法完成其他动作
;现在任务是跳出512字节限制，完成更多操作
;********************************************
NUL EQU 0x00                        ;空字符
SETCHAR EQU 0x07                    ;设置文字属性
VIDOMEM EQU 0xb800                  ;显卡控制输出起始地址
STRINGLEN EQU 0xffff                ;字符串长度

;********************************************
;该头部声明个代码段尺寸、段地址、分几个段、段入口点
;偏移地址
;********************************************
section head align=16 vstart=0      ;当不设置vstart参数时，标号的汇编地址是相对程序头的偏移
    Size dd ProgramEnd              ;长度4B，偏移地址0x00

    SegmentAddr:
    CodeSeg dd section.code.start   ;长度4B，偏移地址0x04
    DataSeg dd section.data.start   ;长度4B，偏移地址0x08
    StackSeg dd section.stack.start ;长度4B，偏移地址0x0c

    SegmentNum:
    SegNum db (SegmentNum-SegmentAddr)/4 ; 长度1B，偏移地址0x10
    Entry dw CodeStart              ;长度2B，偏移地址0x11（偏移地址）
            dd section.code.start   ;长度4B，偏移地址0x13（段地址）
    
section code align=16 vstart=0      ;当不设置vstart参数时，标号的汇编地址是相对程序头的偏移
CodeStart:
    mov ax,[DataSeg]
    mov ds,ax
    xor si,si
    call PrintString
    jmp $
PrintString:                        ;打印函数  
    .setup:                         ;初始化部分
    push ax                         ;原有数据入栈保存
    push bx
    push cx
    push dx
    mov ax,VIDOMEM                  
    mov es,ax                       ;通过修改附加段寄存器值，跳转到）0Xb800执行
    xor di,di
    
    mov bh,SETCHAR                  ;文字属性统一为0x07
    mov cx,STRINGLEN                ;过定义字符串长度

    .printchar:                     ;打印字符
    mov bl,[ds:si]                  ;字符串段+偏移地址，BX寄存器低八位存储字符 ASCII值
    or bl,NUL                       
    jz .return                      ;若当前字符是0x00则不写入显存
    
    inc si                          ;SI加一指向下一个字符
    mov [es:di],bl                  ;字符值存入寄存器
    inc di                          ;DI 寄存器加一
    mov [es:di],bh                  ;字符属性存入寄存器
    inc di                          ;DI 寄存器加一
    
    loop .printchar                 ;循环执行
    .return:
    mov bx, di
    pop dx
    pop cx
    pop bx
    pop ax
    ret                             ;程序结束，跳回调用处

section data align=16 vstart=0      ;当不设置vstart参数时，标号的汇编地址是相对程序头的偏移
    Hello db 'Hello,I come from program on section one,lock!'
section stack align=16 vstart=0      ;当不设置vstart参数时，标号的汇编地址是相对程序头的偏移
    times 128 db 0                   ;先为该代码段占用128字节内存空间
section end align=16                 ;当不设置vstart参数时，标号的汇编地址是相对程序头的偏移
    ProgramEnd: