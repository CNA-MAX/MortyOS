;*******************************************************************
;系统内核
;功能：分配内存，读取加载用户程序，控制用户程序执行
;*******************************************************************
            KernelCodeSelector  equ 0x38    ;内核代码段选择子
            KernelDataSelector  equ 0x30    ;内核数据段选择子
            SysDemoSeletor      equ 0x28    ;系统例程段选择子
            TextRamSeletor      equ 0x20    ;文本显示缓冲区段选择子
            KernelStackSelector equ 0x18    ;内存堆栈段选择子
            MemorySelector      equ 0x08    ;0~4GB内存段选择子

            UserProgameLocation equ 50   ;用户代码存在扇区

;内核头部，用于加载内核程序
            KernelLength    dd  KernelEnd                       ;内核总长
            SysDemoSeg      dd  section.SysDemoSel.start        ;section.段名称.start为Nasm提供获取段汽车汇编地址语法
                                                                ;公用例程
            KernelDataSel   dd  section.KernelData.start        ;内核数据段
            KernelCodeSel   dd  section.KernelCode.start        ;内核代码段
            KernelEntry     dd  start                           ;内核代码段入口
                            dw  KernelCodeSelector
[bits 32]
SECTION SysDemoSel  vstart=0        ;公共例程
;字符串显示例程
;显示0终止的字符串并移动光标
;输入:DS:EBX 串地址
PutString:
        push ecx
    .getc:
        mov cl,[ebx]
        or cl,cl
        jz .exit
        call PutChar
        inc ebx
        jmp .getc
    .exit:
        pop ecx
        retf                        ;段间返回
;函数功能：在当前光标处显示一个字符，并推进光标。
;注意:仅用于段内调用
;输入：CL 字符ASCII码
PutChar:
        pushad                          ;32位寄存器入栈，顺序EAX,ECX,EDX,EBX,ESP,EBP,ESI,EDI 
        ;获取光标当前位置
        mov dx,0x3d4                    ;显存接收命令寄存器
        mov al,0x0e
        out dx,al
        inc  dx                         ;显存读写数据寄存器0x3d5
        in al,dx                        ;读8bit高字
        mov ah,al


        dec dx                          ;0x3d4
        mov al,0x0f                     ;读8bit低字
        out dx,al
        inc dx
        in al,dx
        mov bx,ax                       ;Bx储存代表光标位置18位数

        cmp cl,0x0d             ;判断回车
        jz .PutCR
        cmp cl,0x0a             ;判断换行
        jz .PutLF

        push es                 ;正常显示字符
        mov eax,TextRamSeletor  ;0xb800选择子
        mov es,eax
        shl bx,1
        mov [es:bx],cl
        pop es
        call .CursorNext
        jmp .RollScreen

    .CursorNext:                        ;光标推进到下一个位置
        shr bx,1
        inc bx
        ret

    .PutLF:                             ;换行
        add bx,80
        jmp .RollScreen 
    .PutCR:                             ;回车
        mov ax,bx
        mov bl,80
        div bl
        mul bl
        mov bx,ax
        jmp .SetCursor
    
    .RollScreen:
        cmp bx,2000                     ;光标超出屏幕
        jl .SetCursor                   ;清屏

        push ds
        push es
        mov eax,TextRamSeletor
        mov ds,eax
        mov es,eax
        cld
        mov esi,0xa0                    ;32位模式下movsb/w/d需要使用esi/edi/ecx
        mov edi,0x00
        mov ecx,1920
        rep movsd
        mov bx,3840                     ;清除屏幕最低一行
        mov ecx,80
     .cls:              
        mov word[es:bx],0x720
        add bx,2
        loop .cls

        pop es
        pop ds
        mov bx,1920
     .SetCursor:                         ;设置光标
        mov dx,0x3d4
        mov al,0x0e
        out dx,al

        inc dx
        mov al,bh
        out dx,al

        dec dx
        mov al,0x0f
        out dx,al

        inc dx
        mov al,bl
        out dx,al
        
        popad                           ;32位寄存器出栈，pushad逆过程
        ret
          

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
        retf                                            ;段间返回
;函数功能:在当前光标处显示一个双字(Hex)并推进光标
;输入：EDX：要转换并显示的数字
;输出；无
PutHexDword:
        pushad
        
        push ds
        mov ax,KernelDataSelector                       ;切换到内核数据段
        mov ds,ax

        mov ebx,BinHex                                 ;基址寄存器指向核心数据段内转换表
        mov ecx,8

     .xlt:
        rol edx,4
        mov eax,edx
        and eax,0x0000_0000f
        xlat                                            ;开始查表

        push ecx
        mov cl,al
        call PutChar                                    ;打印字符
        pop ecx

        loop .xlt

        pop ds

        popad
        retf                                            ;段内调用返回
;函数功能:动态内存分配
;输入:ECX 需要空间
;返回:ECX 起始线性地址
;该函数实现过程，初始化0x0010_0000，该位置位于1M内存空间外，用ECX传入需要内存空间后，用
;0x0010_0000加上需要的大小并传入ECX作为下一个起始地址。缺点是前面已经使用完的内存不能再次被调用
;由于现在计算机内存大多1G+，所以算法的不足被计算机硬件弥补
AllocateMemory:
        push ds
        push eax
        push ebx

        mov eax,KernelDataSelector
        mov ds,eax

        mov eax,[RamAlloc]
        add eax,ecx                     ;下次分配起始地址

        mov ecx,[RamAlloc]              ;返回分配的起始地址

        mov ebx,eax
        and ebx,0xffff_fffc
        add ebx,4                       ;强制对齐
        test eax,0x0000_0003            ;下次分配起始地址4字节对齐
        cmovnz eax,ebx                  ;若没对齐，强制对齐
        mov [RamAlloc],eax

        pop ebx
        pop eax
        pop ds
        retf

;函数功能:在GDT内安装用户程序描述符
;输入: EDX:EAX 新描述符
;输出: CX 描述符选择子
SetUpGDTDescriptor:
        push eax
        push ebx
        push edx
        push ds
        push es

        mov ebx,KernelDataSelector  ;切换到内核数据段,准备开始处理GDT
        mov ds,ebx

        sgdt [pgdt]                 ;获取GDT大小与基地址

        mov ebx,MemorySelector      
        mov es,ebx                  ;ES指向4GB数据段操作GDT

        movzx ebx,word [pgdt]       ;GDT界限
        inc bx                      ;GDT总字节数，下一个描述符偏移量
        add ebx,[pgdt+2]            ;GDT线性地址+偏移量得到用于安装新描述符线性地址

        mov [es:ebx],eax
        mov [es:ebx+4],edx          ;将edx：eax描述符写入edi寄存器指向偏移处

        add word [pgdt],8           ;开拓一个描述符空间
        lgdt [pgdt]                 ;GDT更改生效

        mov ax,[pgdt]               ;获取GDT新界限值生成段选择子
        xor dx,dx
        mov bx,8
        div bx
        mov cx,ax
        shl cx,3                    ;将索引号移到正确位置

        pop es
        pop ds

        pop edx
        pop ebx
        pop eax
        retf
;函数功能：构造存储器与系统的描述符
;输入:  EAX 线性基地址
;       EBX 段界限
;       ECX 属性，无关位清零
;返回:  EAX:EAX 描述符
MakeSegDescriptor:                        
         mov edx,eax
         shl eax,16
         or ax,bx                           ;构造描述符前32位

         and edx,0xffff_0000                ;清除基地址中无关位
         rol edx,8                          ;循环左移8位
         bswap edx                          ;按位取反，装配基地址31~24和23~16

         xor bx,bx
         or edx,ebx                         ;装配段界限高四位

         or edx,ecx                         ;装配属性

         retf

SECTION KernelData  vstart=0        ;内核数据
    pgdt    dw  0
            dd  0                   ;设置修改GDT
    RamAlloc    dd  0x0010_0000     ;分配内存起始地址
    ;符号地址检索，可拓展，其作用是提供例程给用户程序调用
         salt:
         salt1           db  '@Kelnel_PrintString'
                     times 256-($-salt1) db 0
                          dd  PutString
                          dw  SysDemoSeletor      

         salt2           db  '@Kelnel_ReadDiskData'
                     times 256-($-salt2) db 0
                          dd  ReadHardDisk
                          dw  SysDemoSeletor      

         salt3           db  '@PrintDwordAsHexString'
                     times 256-($-salt3) db 0
                          dd  PutHexDword
                          dw  SysDemoSeletor      

         salt4           db  '@Kelnel_TerminateProgram'
                     times 256-($-salt4) db 0
                          dd  ReturnPoint
                          dw  KernelCodeSelector

         SaltItemLen   equ $-salt4
         SaltItems      equ ($-salt)/SaltItemLen

         message1        db  '  If you seen this message,that means we '
                          db  'are now in protect mode,and the system '
                          db  'core is loaded,and the video display '
                          db  'routine works perfectly.',0x0d,0x0a,0

         message5        db  '  Loading user program...',0
         
         DoStatus        db  'Done.',0x0d,0x0a,0
         
         message6        db  0x0d,0x0a,0x0d,0x0a,0x0d,0x0a
                          db  '  User program terminated,control returned.',0

         BinHex          db '0123456789ABCDEF'     ;PutHexDword子过程用的查找表

         KernelBuf  times 2048  db  0   ;内核数据缓冲区
         EspPointer     dd      0       ;内核临时保存数据栈指针

         CPU_Brand0 db  0x0d,0x0a,'  ',0        ;输出cpu品牌信息
         CPU_Brand  times 52 db 0
         CPU_Brand1 db  0x0d,0x0a,0x0d,0x0a,0


SECTION KernelCode  vstart=0        ;内核代码
;函数功能:重定位用户程序
;输入:ESI 起始逻辑扇区号
;返回:AX 指向用户程序头部的选择子
LoadRelocateProgram:                    
        push ebx
        push ecx
        push edx
        push esi
        push edi 

        push ds
        push es

        mov eax,KernelDataSelector
        mov ds,eax                              ;DS切换到内核数据段

        mov eax,esi                             ;读取程序头部数据
        mov ebx,KernelBuf
        call SysDemoSeletor:ReadHardDisk        ;加载选择子

        ;计算程序大小
        mov eax,[KernelBuf]                     ;程序尺寸
        mov ebx,eax
        and ebx,0xffff_fe00                     ;512字节对齐
        add ebx,512
        test eax,0x0000_01ff                    ;程序大小是512倍数？
        cmovnz eax,ebx                          ;不够512补零

        mov ecx,eax                             ;实际需要内存大小
        call SysDemoSeletor:AllocateMemory
        mov ebx,ecx                             ;EAX 保存申请到的内存首地址
        push ebx
        xor edx,edx
        mov ecx,512
        div ecx
        mov ecx,eax                             ;总扇区数

        mov eax,MemorySelector                  ;DS指向4GB段
        mov ds,eax
        mov eax,esi                             ;起始扇区号
    .Part1:                                     ;建立GDT
        call SysDemoSeletor:ReadHardDisk        
        inc eax
        loop .Part1                             ;循环读取完整用户程序
        ;建立头部描述符
        pop edi                                 ;恢复程序装载首地址
        mov eax,edi                             ;程序头起始线性地址
        mov ebx,[edi+0x04]                      ;段长度
        dec ebx                                 ;段界限
        mov ecx,0x0040_9200                     ;设置段描述符字节粒度
        call SysDemoSeletor:MakeSegDescriptor
        call SysDemoSeletor:SetUpGDTDescriptor
        mov [edi+0x04],cx                        

        ;建立程序代码描述符
         mov eax,edi
         add eax,[edi+0x14]                 ;代码起始线性地址
         mov ebx,[edi+0x18]                 ;段长度
         dec ebx                            ;段界限
         mov ecx,0x0040_9800                 ;字节粒度的代码段描述符
         call SysDemoSeletor:MakeSegDescriptor
         call SysDemoSeletor:SetUpGDTDescriptor
         mov [edi+0x14],cx
         ;建立程序数据描述符
         mov eax,edi
         add eax,[edi+0x1c]                 ;数据段起始线性地址
         mov ebx,[edi+0x20]                 ;段长度
         dec ebx                            ;段界限
         mov ecx,0x0040_9200                 ;字节粒度的代码段描述符
         call SysDemoSeletor:MakeSegDescriptor
         call SysDemoSeletor:SetUpGDTDescriptor
         mov [edi+0x1c],cx
         ;建立程序堆栈段描述符
         mov ecx,[edi+0x0c]                     ;4KB倍率
         mov ebx,0x000f_ffff
         sub ebx,ecx                            ;段界限
         mov eax,4096
         mul dword [edi+0x0c]
         mov ecx,eax                            ;为堆栈段分配内存
         call SysDemoSeletor:AllocateMemory
         add eax,ecx                            ;取得堆栈高位物理地址
         mov ecx,0x00c0_9600                    ;4KB堆栈段描述符
         call SysDemoSeletor:MakeSegDescriptor
         call SysDemoSeletor:SetUpGDTDescriptor
         mov [edi+0x08],cx
         ;SALT(符号地址对照表) 
         ;允许用户程序通过定义功能名称调用函数
         ;重定位SALT            
         mov eax,[edi+0x04]
         mov es,eax                             ;ES 指向用户程序头部
         mov eax,KernelDataSelector      
         mov ds,eax

         cld

         mov ecx,[es:0x24]                      ;用户程序SALT条目数
         mov edi,0x28                           ;用户程序SALT位于头部0x2c处
  .Part2: 
         push ecx
         push edi
         mov ecx,SaltItems
         mov esi,salt
  .Part3:
         push edi
         push esi
         push ecx

         mov ecx,64                         ;对比检索表条目次数
         repe cmpsd                         ;单次对比4字节
         jnz .Part4
         mov eax,[esi]                      ;若匹配，ESI将刚好指向气候地址数据
         mov [es:edi-256],eax               ;将字符串改写为偏移地址
         mov ax,[esi+4]
         mov [es:edi-252],ax                ;以及段选择子
  .Part4:
      
        pop ecx
        pop esi
        add esi,SaltItemLen
        pop edi                             ;从头比较
        loop .Part3
      
        pop edi
        add edi,256
        pop ecx
        loop .Part2

        mov ax,[es:0x04]
        pop es                                  
        pop ds

        pop edi
        pop esi
        pop edx
        pop ecx
        pop ebx
        ret
      
start:                                          ;内核由这里开始执行
        mov ecx ,KernelDataSelector         
        mov ds,ecx                              ;使DI寄存器指向内核数据段

        mov ebx,message1
        call SysDemoSeletor:PutString

        ;显示处理器品牌信息
        mov eax,0x8000_0002
        cpuid
        mov [CPU_Brand + 0x00],eax
        mov [CPU_Brand + 0x04],ebx
        mov [CPU_Brand + 0x08],ecx
        mov [CPU_Brand + 0x0c],edx

        mov eax,0x8000_0003
        cpuid
        mov [CPU_Brand + 0x10],eax
        mov [CPU_Brand + 0x14],ebx
        mov [CPU_Brand + 0x18],ecx
        mov [CPU_Brand + 0x1c],edx

        mov eax,0x8000_0004
        cpuid
        mov [CPU_Brand + 0x20],eax
        mov [CPU_Brand + 0x24],ebx
        mov [CPU_Brand + 0x28],ecx
        mov [CPU_Brand + 0x2c],edx

        mov ebx,CPU_Brand0
        call SysDemoSeletor:PutString
        mov ebx,CPU_Brand
        call SysDemoSeletor:PutString
        mov ebx,CPU_Brand1
        call SysDemoSeletor:PutString

        mov ebx,message5
        call SysDemoSeletor:PutString
        mov esi,UserProgameLocation            ;用户程序位于逻辑扇区UserProgameLocation
        call LoadRelocateProgram

        mov ebx,DoStatus
        call SysDemoSeletor:PutString

        mov [EspPointer],esp            ;临时保存堆栈指针
        mov ds,ax
        jmp far [0x10]                  ;控制权交接
;用户程序返回点
ReturnPoint:                                
    mov eax,KernelDataSelector          
    mov ds,eax                          ;使DS指向内核数据段

    mov eax,KernelStackSelector         ;切换回内核堆栈
    mov ss,eax
    mov esp,[EspPointer]

    mov ebx,message6
    call SysDemoSeletor:PutString
    ;这里可以放置清除用户程序各种描述符的指令
    ;也可以加载并启动其它程序
       
    hlt

SECTION KernelTail
KernelEnd:                              ;该标号返回内核位汇编地址