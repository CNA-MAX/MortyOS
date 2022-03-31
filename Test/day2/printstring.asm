NUL EQU 0x00                        ;空字符
SETCHAR EQU 0x07                    ;设置文字属性
VIDOMEM EQU 0xb800                  ;显卡控制输出起始地址
STRINGLEN EQU 0xffff                ;字符串长度

section code align=16 vstart=0x7c00 ;声明每段代码长度16字节，BIOS起始地址0x7c00
    mov si,SayHello                 ;将Sayhello装进源变址寄存器SI
    xor di,di                       ;将DI寄存器清零
    call PrintString                ;跳转到打印函数(PrintString)
    mov si,SayBye                   ;将SayBye装进源变址寄存器SI
    call PrintString                ;跳转到打印函数(PrintString)
    jmp END                         ;死循环     

PrintString:                        ;打印函数  
    .setup:                         ;初始化部分
    mov ax,VIDOMEM                  
    mov es,ax                       ;通过修改附加段寄存器值，跳转到）0Xb800执行

    mov bh,SETCHAR                  ;文字属性统一为0x07
    mov cx,STRINGLEN                ;过定义字符串长度

    .printchar:                     ;打印字符
    mov bl,[ds:si]                  ;字符串段+偏移地址，BX寄存器低八位存储字符 ASCII值
    inc si                          ;SI加一指向下一个字符
    mov [es:di],bl                  ;字符值存入寄存器
    inc di                          ;DI 寄存器加一
    mov [es:di],bh                  ;字符属性存入寄存器
    inc di                          ;DI 寄存器加一
    or bl,NUL                       
    jz .return                      ;如果字符打印完毕跳转到return段
    loop .printchar                 ;循环执行
    .return:
    ret                             ;程序结束，跳回调用处


SayHello db 'Hello!'                ;将字符串转换为ASCII值并，以db格式储存，空白处用0填补
            db 0x00
SayBye   db 'Goodbye.'
            db 0x00
END:jmp END                         ;死循环
times 510-($-$$) db 0
                 db 0x55,0xaa       ;余下字节填充0，最后填写结束标志55aa