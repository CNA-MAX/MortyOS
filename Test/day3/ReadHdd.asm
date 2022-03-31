;********************************************
;BIOS里面的512字节过于小，完全无法完成其他动作
;现在任务是跳出512字节限制，完成更多操作
;********************************************
NUL EQU 0x00                        ;空字符
SETCHAR EQU 0x07                    ;设置文字属性
VIDOMEM EQU 0xb800                  ;显卡控制输出起始地址
STRINGLEN EQU 0xffff                ;字符串长度

HDDPORT EQU 0x1f0
;********************************************
;LBA28逻辑块存储，是将硬盘分为2的28次方个扇区，
;每个扇区512字节，既可寻址128G存储空间，在本项目
;中完全够用.
;0x1f0是LBA28模式下16为数据读、写端口
;********************************************

section code align=16 vstart=0x7c00 ;声明每段代码长度16字节，BIOS起始地址0x7c00

    mov si,[READSTART]              ;扇区号为28位，需要两个16位寄存器存储
    mov cx,[READSTART+0x02]         ;READSTART低位在SI，高位在CX
    mov al,[SECTORNUM]              ;将读取扇区数放在ax寄存器低位
    push ax                         ;为防止数据混乱，将ax先入栈保存
    ;物理地址转换为逻辑地址
    mov ax,[DESTMEN]
    mov dx,[DESTMEN+0x02]           ;先把起始位置用dx、ax寄存器保存
    mov bx,16                       
    div bx                          ;将DESTMEN除以16商存储在ax寄存器
    mov ds,ax                       ;作为段地址

    xor di,di                       ;DI寄存器置0
    pop ax                          ;ax数据出栈

    call ReadHdd                    ;调用读取硬盘函数
    xor si,si
    call PrintString                ;调用打印字符函数
    jmp End


    ReadHdd:                        ;读硬盘函数，需要声明起始扇区号，扇区数以及保存地址
        push ax
        push bx
        push cx
        push dx
        ;防止数据混乱，将寄存器数据入栈保存
        mov dx,HDDPORT+2            ;该端口声明读扇区数
        out dx,al                   ;向该端口写入SECTORNUM

        mov dx,HDDPORT+3            ;扇区起始位置声明
        mov ax,si
        out dx,al                   ;数据端口只有16位，需要逐字写入

        mov dx,HDDPORT+4
        mov al,ah 
        out dx,al

        mov dx,HDDPORT+5
        mov ax,cx
        out dx,al

        mov dx,HDDPORT+6            ;声明硬盘读取方式
        mov al,ah
        mov ah,0xe0
        or al,ah                    ;或运算将0x1f6高4位定义为1110，表示主硬盘LBA模式
        out dx,al                   ;进入寄存器
        
        mov dx,HDDPORT+7            ;读硬盘模式
        mov al,0x20
        out dx,al

        .waits:                     ;等待硬盘参数
        in al,dx                    ;读取0x1f7端口查询状态
        and al,0x88                 ;仅对3、7位感兴趣
        cmp al,0x08                 ;通过比较al与0x08值判断硬盘是否准备好数据
        jnz .waits

        mov dx,HDDPORT              ;读取0x1f0数据               
        mov cx,256                  ;一个扇区512字节，需要读取256次     512/2=256,因为每次仅能读取两字节

        .readword:
        in ax,dx                    ;将数据读取出来 
        mov [ds:di],ax              ;保存在ds：di处
        add di,2
        or ah,0x00                  ;当无数据时跳出循环
        jnz .readword

        .return:                     ;将原本数据出栈
        pop dx
        pop cx
        pop bx
        pop ax
        
        ret

PrintString:                        ;打印函数  
    .setup:                         ;初始化部分
    push ax                         ;原有数据入栈保存
    push bx
    push cx
    push dx
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
    mov bx, di
    pop dx
    pop cx
    pop bx
    pop ax
    ret                             ;程序结束，跳回调用处


READSTART dd 10                     ;开始读取位置为10号逻辑扇区      
SECTORNUM db 1                      ;读取1个扇区
DESTMEN   dd 0x10000                ;数据保存位置，在0x10000往后保存

End: jmp End
times 510-($-$$) db 0
                 db 0x55, 0xaa