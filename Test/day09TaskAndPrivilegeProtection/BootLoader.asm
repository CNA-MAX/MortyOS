;*****************************************
;硬盘主引导扇区，位于磁盘0柱0道1扇区
;内存空间映射
;0000_7E20:|--栈段    基地址：0x7c00 界限：FFFFE 32bit，4k--| 
;0000_7E18:|--数据段  基地址：0x7c00 界限：001FE 32bit，B --|
;0000—7E10:|--代码段  基地址：0x7c00 界限：001FE 32bit，B --|
;0000—7E08:|--数据段  基地址：0x0000 界限：FFFFF 32bit，4K--|
;0000-7E00:|--          空描述符                         --|
;0000-7C00:|--          主引导程序                       --|
;0000-6C00:|--          4KB栈段                          --|
;内核加载后GDT布局
;表内偏移                       描述符索引
;+38            内核代码段       0x38
;+30            内核数据段       0x30
;+28            公用例程段       0x28
;+20            文本模式显存     0x20
;+18            初始栈段         0x18
;+10            初始代码段       0x10
;+08            0~4G数据段       0x08
;+00            空描述符         0x00

;功能：从BIOS接管计算机控制权建立GDT，加载内核，初始化执行环境
;*****************************************

KernelBaseAddress   equ 0x0004_0000              ;定义内核加载起始内存地址
KernelStartSector   equ 0x0000_0001              ;定义内核起始逻辑扇区号
;16位实模式
mov ax,cs
mov ss,ax
mov sp,0x7c00                                   ;寄存器对齐，在0x7c00开始运行

;建立GDT
;技术GDT所在逻辑段地址
mov eax,[cs:pgdt+0x7c00+0x02]                   ;GDT32位物理地址
xor edx,edx 
mov ebx,16
div ebx                                         ;将32位地址转化为16位逻辑，高16位在dx，低16位在ax

mov ds,eax                                      ;DS寄存器指向该段，准备执行
mov ebx,edx                                     ;加载段内起始偏移地址
;创建描述符表
;0#描述符，空描述符
mov dword [ebx+0x00],0x0000_0000
mov dword [ebx+0x04],0x0000_0000
;1#数据段描述符，对应0~4GB线性地址空间
mov dword [ebx+0x08],0x0000_ffff                ;基地址为0，段界限为0xffff
mov dword [ebx+0x0c],0x00cf_9200                ;粒度为4KB，寄存器段描述符
;2#代码段描述符，保护模式下初始化代码段描述符
mov dword [ebx+0x10],0x7c00_01ff                ;基地址为0x0000_7c00,段界限0x1ff
mov dword [ebx+0x14],0x0040_9800                ;粒度为1字节
;3#栈段描述符，保护模式下堆栈段描述符              ;堆栈向下延申
mov dword [ebx+0x18],0x7c00_fffe                ;基地址0x7c00，段界限0xfffe
mov dword [ebx+0x1c],0x00cf_9600                ;粒度4KB
;4#显示缓冲区，保护模式下显示缓冲区描述符
mov dword [ebx+0x20],0x8000_7fff                ;基地址0xb800，段界限0x7fff
mov dword [ebx+0x24],0x0040_920b                ;粒度为1字节
;初始化描述符表寄存器GDTR
mov word [cs:pgdt+0x7c00],39                    ;描述符界限
lgdt [cs:pgdt+0x7c00]                           ;加载GDT寄存器

in al,0x92                                      ;开启南桥芯片内端口
or al,0000_0010B
out 0x92,al                                     ;打开A20地址线
;该步骤的意义是0xFFFFF+1后等于0x00000，因为保护模式下有24根地址线，如果不设置A20会产生进位，系统不需要进位，进而将A20恒设定为0
cli                                             ;屏蔽软件触发中断

mov eax,CR0             
or eax,1
mov CR0,eax                                     ;进入保护模式关键是设置CR0寄存器PE位，将其设置为1
;PE位是CR0的第一位（位0）
;进入保护模式
jmp dword 0x0010:flush                          ;清空流水线
    [bits 32]                                       ;进入保护模式后所有代码都需要按照32位格式编译
flush:
        mov eax,0x0008                                  ;加载数据段（0~4GB）选择子
        mov ds,eax

        mov eax,0x0018                                  ;加载栈段描述符
        mov ss,eax
        xor esp,esp                                     ;初始化栈指针
        ;加载系统核心
        mov edi,KernelBaseAddress                       ;DI寄存器指向系统起始地址
        mov eax,KernelStartSector                       ;AX寄存器装载系统起始逻辑扇区号
        mov ebx,edi                                     ;起始地址
        call ReadHardDisk                               ;读取引导程序（一扇区）

        mov eax,[edi]                                   ;内核尺寸
        xor edx,edx
        mov ecx,512                                     ;占多少个扇区
        div ecx

        or edx,edx
        jnz ReadDisk_1                                  ;未除尽扇区加一才能将数据完整读完
        dec eax                                         ;读一个扇区，扇区总数就减一
    ReadDisk_1:
        or eax,eax                                      ;考虑实际长度≤512个字节的情况 
        jz setup                                        ;eax=0
        
        ;读取剩余扇区
        mov ecx,eax                                     ;32位模式下循环计数使用ecx
        mov eax,KernelStartSector
        inc eax                                         ;由下个扇区接着读
    ReadDisk_2:
        call ReadHardDisk
        inc eax
        loop ReadDisk_2                                 ;循环读取，直到内核读完
setup:
    mov esi,[0x7c00+pgdt+0x02]                      ;保护模式下代码段不可以更改，但可以通过4GB段间接访问
    ;公用例程描述符
    mov eax,[edi+0x04]                              ;公用例程起始汇编地址
    mov ebx,[edi+0x08]                              ;内核数据段汇编地址
    sub ebx,eax
    dec ebx                                         ;公用例程段界限
    add eax,edi                                     ;公用例程段基地址
    mov ecx,0x0040_9800                             ;字节粒度代码段描述符
    call MakeGDTDescriptor
    mov [esi+0x28],eax
    mov [esi+0x2c],edx

    ;内核数据段描述符
    mov eax,[edi+0x08]                              ;内核数据段起始汇编地址
    mov ebx,[edi+0x0c]                              ;内核代码段汇编地址
    sub ebx,eax
    dec ebx                                         ;内核数据段界限
    add eax,edi                                     ;内核数据段基地址
    mov ecx,0x0040_9200                             ;字节粒度数据段描述符
    call MakeGDTDescriptor
    mov [esi+0x30],eax
    mov [esi+0x34],edx

    ;内核代码段描述符
    mov eax,[edi+0x0c]                              ;内核代码段起始汇编地址
    mov ebx,[edi+0x00]                              ;程序总长
    sub ebx,eax
    dec ebx                                         ;内核代码段界限
    add eax,edi                                     ;内核代码段基地址
    mov ecx,0x0040_9800                             ;字节粒度代码段描述符
    call MakeGDTDescriptor
    mov [esi+0x38],eax
    mov [esi+0x3c],edx

    mov word [0x7c00+pgdt],63                       ;描述符表界限。有8个描述符，每个描述符偏移8位，所以有64位，界限63

    lgdt [0x7c00+pgdt]

    jmp far [edi+0x10]                              ;跳转到内核处执行，由于是跨程序跳转，需要使用far关键字
;函数功能:读磁盘
;传入参数:起始扇区号
;         DS:EBX:目标缓冲区地址
;返回：EBX=EBX+512
ReadHardDisk:
        push eax
        push ecx
        push edx
        push eax
        mov dx,0x1f2                                     ;向磁盘发送读取的磁盘号
        mov al,1                                         
        out dx,al                                        ;发送

        inc dx                                           ;0x1f3
        pop eax
        out dx,al                                        ;读LBA地址0~7
        ;硬盘数据串行输出，一次输出8位
        inc dx                                           ;0x1f4
        mov cl,8
        shr eax,cl                                       ;eax右移8位
        out dx,al                                        ;LBA地址8~15

        inc dx                                           ;0x1f5
        shr eax,cl                                       ;eax右移8位
        out dx,al                                        ;LBA地址16~23

        inc dx                                           ;0x1f6
        shr eax,cl                                       ;eax右移8位
        or  al,0xe0                                      ;只需要3位数据，其他的已经读过了
        out dx,al                                        ;LBA地址24~27

        inc dx                                           ;0x1f7
        mov al,0x20                                      ;读命令，写命令0x30
        out dx,al
    .waits:
        in al,dx
        and al,0x88
        cmp al,0x08                                      ;判断硬盘状态
                                                         ;磁盘空闲，且已准备好数据
        jnz .waits                                       ;磁盘忙，未就绪

        mov ecx,256                                      ;读取字节数
        mov dx,0x1f0                                     ;硬盘数据端口
    .readw:
        in ax,dx                                         ;将数据读出来
        mov [ebx],ax                            
        add ebx,2
        loop .readw

        pop edx
        pop ecx
        pop eax
        ret
;函数功能：构造描述符
;入参:eax:线性基地址
;     ebx:段界限
;     ecx:段属性
;返回：edx:eax:完整描述符
MakeGDTDescriptor:
    mov edx,eax                 
    shl eax,16
    or ax,bx                    ;描述符钱32位（eax）构造完毕

    and eax,0xffff_0000         ;清除基地址中无关位
    rol edx,8                   ;循环左移8位
    bswap edx                   ;按位取反，装配基地址31~24和23~16

    xor bx,bx
    or edx,ebx                  ;装配段界限高四位

    or edx,ecx                  ;装配属性
    ret

pgdt    dw 0
        dd 0x0000_7e00          ;GDT物理地址

        times 510-($-$$) db 0
                         db 0x55,0xaa