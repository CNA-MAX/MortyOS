
;********************************************
;BIOS里面的512字节过于小，完全无法完成其他动作
;现在任务是跳出512字节限制，完成更多操作
;********************************************
NUL EQU 0x00                        ;空字符
SETCHAR EQU 0x07                    ;设置文字属性
VIDOMEM EQU 0xb800                  ;显卡控制输出起始地址
LinesLEN EQU 0xffff                ;字符串长度

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
    mov ax,[StackSeg]               ;栈寄存器初始化
    mov ss,ax
    mov sp,StackEnd
    xor si,si
    call ClearScreen
    call PrintLines
    jmp $
PrintLines:                        ;打印函数  
    mov cx,HelloEnd-Hello
    xor si,si                      ;初始化字符指针
    .putc:                          ;循环打印函数
    mov al,[si]                    ;将si值给al
    inc si                         ;si加一
    mov ah,0x0e                    ;显存端口号
    int 0x10                       ;调用10号中断，该中断由bios提供用来打印字符
    loop .putc
    ret

    ClearScreen:                    ;清屏函数，重复写2000个黑底白字空格
    mov ax, VIDOMEM
    mov es, ax
    xor di, di
    mov bl, ' '
    mov bh, SETCHAR
    mov cx, 2000
    .putspace:
    mov [es:di], bl
    inc di
    mov [es:di], bh
    inc di
    loop .putspace
    ret

section data align=16 vstart=0      ;当不设置vstart参数时，标号的汇编地址是相对程序头的偏移
    Hello db 'CRLF'
          db  0x0d,0x0a
          db  0x0d,0x0a
          db 'CR'
          db  0x0a
          db 'LF'
          db 0x0d
          db 'NUL'
          db 0x0d, 0x0a
          db 0x00
    HelloEnd:
section stack align=16 vstart=0      ;当不设置vstart参数时，标号的汇编地址是相对程序头的偏移
    times 128 db 0                   ;先为该代码段占用128字节内存空间
    StackEnd:
section end align=16                 ;当不设置vstart参数时，标号的汇编地址是相对程序头的偏移
    ProgramEnd: