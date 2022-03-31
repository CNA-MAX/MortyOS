;*****************************************
;硬盘主引导扇区，位于磁盘0柱0道1扇区
;功能：从BIOS接管计算机控制权建立GDT，加载内核，初始化执行环境
;*****************************************

KernelBaseAddress   equ 0x0004_0000              ;定义内核加载起始内存地址
KernelStartSector   equ 0x0000_0001              ;定义内核起始逻辑扇区号、

SECTION MBR vstart=0x0000_7c00
    ;16位实模式
    mov ax,cs
    mov ss,ax
    mov sp,0x7c00                                   ;寄存器对齐，在0x7c00开始运行

    ;建立GDT
    ;技术GDT所在逻辑段地址
    mov eax,[cs:pgdt+0x02]                         ;GDT32位物理地址
    xor edx,edx 
    mov ebx,16
    div ebx                                         ;将32位地址转化为16位逻辑，高16位在dx，低16位在ax

    mov ds,eax                                      ;DS寄存器指向该段，准备执行
    mov ebx,edx                                     ;加载段内起始偏移地址
    ;创建描述符表
    ;0#描述符，空描述符
    mov dword [ebx+0x00],0x0000_0000
    mov dword [ebx+0x04],0x0000_0000
    ;1#代码段描述符，保护模式
    mov dword [ebx+0x08],0x0000_ffff                ;基地址为0，段界限为0xffff，DPL:00
    mov dword [ebx+0x0c],0x00cf_9800                ;粒度为4KB，代码段描述符，向上扩展
    ;2#数据段和堆栈段描述符，保护模式
    mov dword [ebx+0x10],0x0000_ffff                ;基地址为0，段界限为0xffff，DPL:00
    mov dword [ebx+0x14],0x00cf_9200                ;粒度为4KB，数据段描述符，向上扩展
    ;初始化描述符表寄存器GDTR
    mov word [cs:pgdt],23                    ;描述符界限
    lgdt [cs:pgdt]                                  ;加载GDT寄存器

    in al,0x92                                      ;开启南桥芯片内端口
    or al,0x02
    out 0x92,al                                     ;打开A20地址线
    ;该步骤的意义是0xFFFFF+1后等于0x00000，因为保护模式下有24根地址线，如果不设置A20会产生进位，系统不需要进位，进而将A20恒设定为0
    cli                                             ;屏蔽软件触发中断

    mov eax,CR0             
    or eax,1
    mov cr0,eax                                     ;进入保护模式关键是设置CR0寄存器PE位，将其设置为1
    ;PE位是CR0的第一位（位0）
    ;进入保护模式
    jmp dword 0x0008:flush                          ;清空流水线
        [bits 32]                                       ;进入保护模式后所有代码都需要按照32位格式编译
flush:
            mov eax,0x00010                                  ;加载数据段（0~4GB）选择子
            mov ds,eax

            mov es,eax                                      
            mov fs,eax
            mov gs,eax
            mov ss,eax
            mov esp,0x7000                                  ;栈指针
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
            jnz .ReadDisk_1                                  ;未除尽扇区加一才能将数据完整读完
            dec eax                                         ;读一个扇区，扇区总数就减一
        .ReadDisk_1:
            or eax,eax                                      ;考虑实际长度≤512个字节的情况 
            jz Page                                         ;eax=0
            
            ;读取剩余扇区
            mov ecx,eax                                     ;32位模式下循环计数使用ecx
            mov eax,KernelStartSector
            inc eax                                         ;由下个扇区接着读
        .ReadDisk_2:
            call ReadHardDisk
            inc eax
            loop .ReadDisk_2                                 ;循环读取，直到内核读完
Page:
    ;准备打开分页机制，创建系统内核页目录表（PDT）
            mov ebx,0x0002_0000                                 ;页目录表PDT物理地址
            mov dword [ebx+4092],0x0002_0003                ;再页目录中创建一个指向自己的表项

            mov edx,0x0002_1003
            mov [ebx+0x000],edx                             ;创建0x00000000对应的目录项
            mov [ebx+0x800],edx                             ;写入目录项
            ;初始化新创建目录项对应页表，初始化页表项
            mov ebx,0x0002_1000                             ;页表物理地址
            xor eax,eax                                     ;起始页物理地址
            xor esi,esi
        .Part1:
            mov edx,eax
            or edx,0x0000_0003
            mov [ebx+esi*4],edx                         ;登记物理地址
            add eax,0x1000                              ;下一个页物理地址
            inc esi
            cmp esi,256                                 
            jl .Part1
            ;令CR3指向页目录开启分页
            mov eax,0x0002_0000                         ;PCD=PWT=0
            mov cr3,eax
            ;将GDT线性地址映射到0x8000_0000开始的相同位置
            sgdt [pgdt]
            mov ebx,[pgdt+2]
            add dword [pgdt+2],0x8000_0000              
            lgdt [pgdt]
            mov eax,CR0
            or eax,0x8000_0000
            mov CR0,eax                                 ;开启分页

            ;将堆栈映射到高端，预防内核数据被覆盖，用户程序可以通过虚拟内存将该段数据覆盖
            add esp,0x8000_0000

            jmp [0x8004_0004]
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

    pgdt    dw 0
            dd 0x0000_8000          ;GDT物理地址

            times 510-($-$$) db 0
                            db 0x55,0xaa