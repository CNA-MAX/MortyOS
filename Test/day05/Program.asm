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
    mov ax,[StackSeg]               ;栈寄存器初始化
    mov ss,ax
    mov sp,StackEnd
    xor si,si
    call ClearScreen
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

    
    cmp bl,0x0d                     ;判断当前字符是否为回车
    jz .putCR                       ;跳转到回车处理段

    cmp bl,0x0a                     ;判断当前字符是否为换行
    jz .putLF                       ;跳转至换行处理段

    or bl,NUL                       
    jz .return                      ;若当前字符是0x00则不写入显存
    inc si                          ;SI加一指向下一个字符
    mov [es:di],bl                  ;字符值存入寄存器
    inc di                          ;DI 寄存器加一
    mov [es:di],bh                  ;字符属性存入寄存器
    inc di                          ;DI 寄存器加一
    
    call  SetCursor                 ;每输出一个字符改变一次光标位置
    jmp .loopEnd                    ;循环执行

    .loopEnd:
    loop .printchar                 ;循环执行
  

    .putCR:                         ;处理回车段
    mov bl,160                      
    mov ax,di
    div bl
    shr ax,8                        ;将ah值移近al
    sub di,ax                       ;打印位置到行首
    call SetCursor                  ;更新光标
    inc si
    jmp .loopEnd


    .putLF:                         ;处理换行段
    add di,160                      ;di值加上160便会换行
    call SetCursor                  ;更新光标
    inc si
    jmp .loopEnd

    .return:
    mov bx, di
    pop dx
    pop cx
    pop bx
    pop ax
    ret                             ;程序结束，跳回调用处

    SetCursor:                      ;光标更新
    push ax                         ;需要使用寄存器，先对其进行入栈保存
    push dx
    push bx

    mov ax,di                       ;将di值放在ax寄存器，等等用它来除以2得到光标当前位置

    mov dx,0

    mov bx,2
    div bx
    mov bx,ax

    mov dx,0x3d4                    ;0x3d4向显存发送端口编号
    mov al,0x0e
    out dx,al                       ;发送寄存器编号
    mov dx,0x3d5                    ;0x3d5向显存发送读写数据
    mov al,bh
    out dx,al                       
    mov dx,0x3d4                    ;向显存发送端口编号
    mov al,0x0f
    out dx,al
    mov al,bl
    mov dx,0x3d5
    out dx,al
    pop ax
    pop dx
    pop bx
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