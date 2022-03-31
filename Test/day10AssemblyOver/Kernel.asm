;*******************************************************************
;系统内核
;功能：分配内存，读取加载用户程序，控制用户程序执行
;*******************************************************************
    ;常量定义宏
    Flat4GBCodeSegSel   equ 0x0008      ;内存平坦模型下的4G代码段选择子
    Flat4GBDataSegSel   equ 0x0018      ;数据段选择子
    IdtLinearAddress    equ 0x8001_f000 ;中断向量描述符表基地址

    ;程序定义宏
    %macro AllocKernelLinear    0       ;在内核空间中分配虚拟内存
            mov ebx,[KernelTCB+0x06] 
            add dword [KernelTCB+0x06],0x1000 
            call Flat4GBCodeSegSel:AllocInstPage
    %endmacro
    %macro  AllocUserLiner      0       ;在任务空间中分配虚拟内存
            mov ebx,[esi+0x06]
            add dword [esi+0x06],0x1000
            call Flat4GBCodeSegSel:AllocInstPage
    %endmacro

SECTION  Kernel  vstart=0x80040000        ;内核
;内核头部，用于加载内核程序
            KernelLength    dd  KernelEnd                       ;内核总长
            KernelEntry     dd  start                           ;内核代码段入口
[bits 32]

;字符串显示例程
;显示0终止的字符串并移动光标
;输入:DS:EBX 串地址
PutString:
        push ebx
        push ecx

        cli                         ;硬件在操作，关中断
    .getc:
        mov cl,[ebx]
        or cl,cl                    ;检测结束标志 0
        jz .exit                    ;显示完毕，返回
        call PutChar
        inc ebx
        jmp .getc
    .exit:
        sti                         ;恢复中断
        pop ecx
        pop ebx
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
        and ebx,0x0000_ffff             ;准备使用32位寻址方式访问显存
        
        cmp cl,0x0d                     ;判断回车
        jz .PutCR
        cmp cl,0x0a                     ;判断换行
        jz .PutLF

        shl bx,1                        ;正常显示字符
        mov [0x800b_8000+ebx],cl        ;在光标处显示字符

        call .CursorNext
        jmp .RollScreen

    .CursorNext:                         ;光标推进到下一个位置
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

        cld
        
        mov esi,0x800b_80a0
        mov edi,0x800b_8000                    ;32位模式下movsb/w/d需要使用esi/edi/ecx
        mov ecx,1920
        rep movsd
        mov bx,3840                     ;清除屏幕最低一行
        mov ecx,80
     .cls:              
        mov word[0x800b_8000+ebx],0x720
        add bx,2
        loop .cls

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
        cli
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
        sti
        retf                                            ;段间返回
;函数功能:在当前光标处显示一个双字(Hex)并推进光标
;输入：EDX：要转换并显示的数字
;输出；无
PutHexDword:
        pushad
        
        mov ebx,BinHex                                 ;基址寄存器指向核心数据段内转换表
        mov ecx,8

     .xlt:
        rol edx,4
        mov eax,edx
        and eax,0x0000_000f
        xlat                                            ;开始查表

        push ecx
        mov cl,al
        call PutChar                                    ;打印字符
        pop ecx

        loop .xlt

        popad
        retf                                            ;段内调用返回
;函数功能:在GDT内安装用户程序描述符
;输入: EDX:EAX 新描述符
;输出: CX 描述符选择子
SetUpGDTDescriptor:
        push eax
        push ebx
        push edx
       
        sgdt [pgdt]                 ;获取GDT大小与基地址

        movzx ebx,word [pgdt]       ;GDT界限
        inc bx                      ;GDT总字节数，下一个描述符偏移量
        add ebx,[pgdt+2]            ;GDT线性地址+偏移量得到用于安装新描述符线性地址

        mov [ebx],eax
        mov [ebx+4],edx             ;将edx：eax描述符写入edi寄存器指向偏移处

        add word [pgdt],8           ;开拓一个描述符空间
        lgdt [pgdt]                 ;GDT更改生效

        mov ax,[pgdt]               ;获取GDT新界限值生成段选择子
        xor dx,dx
        mov bx,8
        div bx
        mov cx,ax
        shl cx,3                    ;将索引号移到正确位置

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
;函数功能:构造描述符、调用门
;输入:EAX   门代码段内偏移地址
;      BX   门代码所在段选择子
;      CX   段类型及属性
;返回:EDX:EAX 完整描述符
MakeGateDescriptor:
    push ebx
    push ecx
    mov edx,eax
    and edx,0xffff_0000             ;偏移地址搞16位
    or dx,cx                        ;组装属性部分到edx

    and eax,0x0000_ffff             ;偏移地址低16位
    shl ebx,16
    or eax,ebx                      ;组装段选择子部分

    pop ecx
    pop ebx
    retf
;函数功能:分配一个4KB页
;输入:无
;输出:EAX 页物理地址
Allocate4KPage:
        push ebx
        push ecx
        push edx

        xor eax,eax
    .Part1:
        bts [PageBitMap],eax
        jnc .Part2
        inc eax 
        cmp eax,PageMapLen*8
        jl .Part1

        mov ebx,message3
        call Flat4GBCodeSegSel:PutString
        hlt             ;若没有可以分配页，停机
    .Part2:
        shl eax,12      ;乘以4096
        pop edx
        pop ecx
        pop ebx

        ret
;函数功能:分配页并安装当前活动的层级分页结构
;输入:EBX 页线性地址
;输出:无
AllocInstPage:
        push    eax
        push    ebx
        push    esi
        ;检查该线性地址对应页表是否存在
        mov esi,ebx
        and esi,0xffc0_0000
        shr esi,20              ;得到页目录索引，乘4
        or  esi,0xffff_f000     ;页目录自身线性地址+表内偏移

        test dword [esi],0x0000_0001    ;P=1? 检查该线性地址是否已有对应页表
        jnz .Part1
        ;创建该线性地址所对应的页表
        call Allocate4KPage     ;分配一个页作为页表
        or eax,0x0000_0007
        mov [esi],eax           ;在页目录中登记该页表
    .Part1:                     ;开始访问该线性地址所对应页表
        mov esi,ebx
        shr esi,10
        and esi,0x003f_f000    
        or esi,0xffc0_0000      ;得到该页表线性地址
        ;得到该线性地址的页表对应条目
        and ebx,0x003f_f000
        shr ebx,10              ;右移12位再乘以4
        or  esi,ebx             ;页表项线性地址
        call Allocate4KPage     ;分配一个页
        or  eax,0x0000_0007
        mov [esi],eax

        pop esi
        pop ebx
        pop eax

        retf

;函数功能:创建新页目录并复制当前页目录内容
;输入:无
;输出:EAX 新页目录物理地址
CreateCopyCurPdir:
        push esi
        push edi
        push ebx
        push ecx

        call Allocate4KPage     ;创建新页
        mov ebx,eax
        or ebx,0x0000_0007
        mov [0xffff_fff8],ebx

        invlpg [0xffff_fff8]

        mov esi,0xffff_f000     ;ESI -> 当前页线性地址
        mov edi,0xffff_e000     ;EDI -> 新页目录线性地址
        mov ecx,1024            ;ECX=要复制目录项数
        cld
        repe movsd

        pop ecx
        pop ebx
        pop edi
        pop esi

        retf
;函数功能:通用中断处理过程
;输入:无
;输出:无
GeneralInterruptHandler:
        push eax

        mov al,0x20         ;中断结束命令EOI
        out 0xa0,al         ;向8259A从片发送
        out 0x20,al         ;向8259A主片发送

        pop eax
        ;未针对中断事件编写对应处理流程

        iretd 
;函数功能:通用异常处理过程
;输入:无
;输出:无
GeneralExceprionHandler:
        mov ebx,ExcepMsg
        call Flat4GBCodeSegSel:PutString
        hlt ;停机
;函数功能:通实时时钟中断处理过程
;输入:无
;输出:无
RTM0x70InterruptHadle:
        pushad
        mov al,0x20         ;中断结束命令EOI
        out 0xa0,al         ;向8259A从片发送
        out 0x20,al         ;向8259A主片发送
        
        mov al,0x0c         ;寄存器C的索引，开放NMI
        out 0x70,al
        in al,0x71          ;读一下RTC的寄存器C，否则只发生一次中断此处不考虑闹钟和周期性中断的情况
        mov eax,TcbChain    ;找当前任务（状态为忙的任务）在链表中的位置
    .Part0:
        mov ebx,[eax]       ;EAX=链表头 EBX=下一个TCB线性地址
        or  ebx,ebx
        jz  .Irtn           ;若链表空或到链尾，从中断返回
        cmp word [ebx+0x04],0xffff  ;当前任务忙？
        je .Part1
        mov eax,ebx         ;下一个TCB线性地址
        jmp .Part0
    .Part1:                 ;将当前忙任务移到链尾
        mov ecx,[ebx]       ;后续TCB线性地址
        mov [eax],ecx       ;将当前任务从链表中删除
    .Part2:
        mov edx,[eax]       ;EBX=当前任务线性地址
        or  edx,edx         ;链尾？
        jz .Part3
        mov eax,edx
        jmp .Part2
    .Part3:
        mov [eax],ebx       ;将忙任务TCB放在链尾
        mov dword [ebx],0x0000_0000    ;将忙任务TCB设置为链尾
        mov eax,TcbChain    ;从链首搜索第一个任务
    .Part4:
        mov eax,[eax]
        or eax,eax          ;到链尾都没有发现空闲任务
        jz .Irtn            ;从中断返回
        cmp word [eax+0x04],0x0000 ;发现空闲任务
        jnz .Part4
        ;将空闲任务和当前任务状态取反
        not word [eax+0x04] ;空闲任务忙
        not word [ebx+0x04] ;忙任务空闲
        jmp far [eax+0x14]  ;任务切换
    .Irtn:
        popad
        iretd
;函数功能:终止当前任务
TerminateCurrentTask:
        mov eax,TcbChain            ;当前任务在链表中位置
    .Part0:
        mov ebx,[eax]
        cmp word [ebx+0x04],0xffff  ;当前任务忙？
        je .Part1
        mov eax,ebx                 ;下一个TCB线性地址
        jmp .Part0
    .Part1:
        mov word [ebx+0x04],0x3333  ;修改当前任务状态为退出
    .Part2:
        hlt                         ;停机，等待程序管理器恢复运行时，将其回收 
        jmp .Part2


;**************************************************************************;
    ;内核数据
        pgdt    dw  0
                dd  0                   ;设置修改GDT
        pidt    dw  0
                dd  0
        TcbChain    dd  0               ;任务控制链
        KernelTCB   times 32 db  0      ;内核(程序控制器)TCB
        PageBitMap  db  0xff,0xff,0xff,0xff,0xff,0xff,0x55,0x55
                    db  0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
                    db  0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
                    db  0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff
                    db  0x55,0x55,0x55,0x55,0x55,0x55,0x55,0x55
                    db  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
                    db  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
                    db  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
        PageMapLen  equ $-PageBitMap
    ;符号地址检索，可拓展，其作用是提供例程给用户程序调用
        Salt:
        Salt1           db  '@Kernel_PrintString'
                     times 256-($-Salt1) db 0
                          dd  PutString
                          dw  Flat4GBCodeSegSel      

        Salt2           db  '@Kernel_ReadDiskData'
                     times 256-($-Salt2) db 0
                          dd  ReadHardDisk
                          dw  Flat4GBCodeSegSel      

        Salt3           db  '@Kernel_PrintDwordAsHexString'
                     times 256-($-Salt3) db 0
                          dd  PutHexDword
                          dw  Flat4GBCodeSegSel      

        Salt4           db  '@Kernel_TerminateProgram'
                     times 256-($-Salt4) db 0
                          dd  TerminateCurrentTask
                          dw  Flat4GBCodeSegSel

        SaltItemLen   equ $-Salt4
        SaltItems     equ ($-Salt)/SaltItemLen
        UserProgameLocation1 equ 50       ;用户代码所在扇区
        UserProgameLocation2 equ 100      ;用户代码所在扇区
        ExcepMsg    db  '********Exception encounted********',0
        message0    db  '  Working in system Kernel with protection '
                    db  'and paging are all enabled.System core is mapped '
                    db  'to address 0x80000000.',0x0d,0x0a,0
                    
        message1    db  '  System wide CALL-GATE mounted.',0x0d,0x0a,0
        ;message2    db  '  System wide CALL-GATE mounted.',0x0d,0x0a,0
                                                                        
        message3    db  '********No more pages********',0


        KernelMsg0  db  '  System Kernel task running!',0x0d,0x0a,0

        BinHex      db '0123456789ABCDEF'     ;PutHexDword子过程用的查找表

        KernelBuf  times 512  db  0   ;内核数据缓冲区

        CPU_Brand0 db  0x0d,0x0a,'  ',0        ;输出cpu品牌信息
        CPU_Brand  times 52 db 0
        CPU_Brand1 db  0x0d,0x0a,0x0d,0x0a,0
;**************************************************************************;
;为LDT安装新描述符
;输入：EDX：EAX 描述符
;     EBX       TCB基地址
;输出：CX       描述符选择子
FillDescriptorInLDT:
        push eax
        push edx
        push edi
     
        mov edi,[ebx+0x0c]                  ;获取LDT基地址
        xor ecx,ecx
        mov cx,[ebx+0x0a]                   ;获取LDT界限
        inc cx                              ;计算LDT中字节数，既描述符偏移地址

        mov [edi+ecx+0x00],eax
        mov [edi+ecx+0x04],edx              ;安装描述符
        add cx,8
        dec cx                              ;获取新LDT界限

        mov [ebx+0x0a],cx                   ;更新LDT界限到TCB

        mov ax,cx
        xor dx,dx
        mov cx,8
        div cx

        mov cx,ax
        shl cx,3
        or cx,0x0004                          ;0000_0000_0000_0100,TI位置1指向LDT，RPL=00

        pop edi
        pop edx
        pop eax
        ret

;函数功能:重定位用户程序
;输入:逻辑扇区号
;     任务控制块基地址
;输出:无
LoadRelocateProgram:                    
        pushad


        mov ebp,esp                             ;为访问通过堆栈传输参数做准备

        mov ebx,0xffff_f000                     ;清空当前页目录前半部分(低2G局部地址空间)
        xor esi,esi
    .Part1:
        mov dword [ebx+esi*4],0x0000_0000
        inc esi
        cmp esi,512
        jl .Part1
        mov eax,cr3
        mov cr3,eax                              ;刷新TLB
        ;开始分配内存并加载用户程序
        mov eax,[ebp+40]                        ;从堆栈中取出用户程序起始扇区号
        mov ebx,KernelBuf                       ;读取程序头
        call Flat4GBCodeSegSel:ReadHardDisk

        ;计算程序大小
        mov eax,[KernelBuf]                     ;程序尺寸
        mov ebx,eax
        and ebx,0xffff_f000                     ;4K对齐
        add ebx,0x1000
        test eax,0x0000_0fff                    ;程序大小是4K倍数？
        cmovnz eax,ebx                          ;不够4K补零

        mov ecx,eax                             ;EAX 保存申请到的内存首地址
        shr ecx,12                              ;程序占用页数

        mov eax,[ebp+40]                        ;起始扇区号
        mov esi,[ebp+36]                        ;从堆栈获取TCB基地址
    .Part2:
        AllocUserLiner                          ;宏:用户程序内分配内存
        push ecx
        mov ecx,8
    .Part3:
        call Flat4GBCodeSegSel:ReadHardDisk
        inc eax
        loop .Part3
        pop ecx
        loop .Part2
        ;内核空间地址空间内创建用户任务TSS
        AllocKernelLinear                       ;宏:在内核地址空间上分配内存,用户任务TSS必须在全局空间上分配

        mov [esi+0x14],ebx                      ;在TCB中填写TSS线性地址
        mov word [esi+0x12],103                 ;在TCB填写TSS界限值
        ;用户任务局部地址空间内创建LDT
        AllocUserLiner                          ;宏:在用户任务地址空间上分配内存
        mov [esi+0x0c],ebx                      ;填写LDT线性地址到TCB中
        ;建立程序代码描述符
         mov eax,0x0000_0000
         mov ebx,0x000f_ffff                
         mov ecx,0x00c0_f800                ;4K粒度的代码段描述符,特权级3
         call Flat4GBCodeSegSel:MakeSegDescriptor
         mov ebx,esi                        ;TCB基地址
         call FillDescriptorInLDT
         or cx,0x0003                       ;设置选择子特权级3
         mov ebx,[esi+0x14]                 ;TCB中获取TSS线性地址
         mov [ebx+76],cx                    ;填写TSS的CS段
         ;建立程序数据描述符
         mov eax,0x0000_0000
         mov ebx,0x000f_ffff                
         mov ecx,0x00c0_f200                ;4K粒度的代码段描述符,特权级3
         call Flat4GBCodeSegSel:MakeSegDescriptor
         mov ebx,esi                        ;TCB基地址
         call FillDescriptorInLDT
         or cx,0x0003                       ;设置选择子特权级3
         mov ebx,[esi+0x14]                 ;TCB中获取TSS线性地址
         mov [ebx+84],cx                    ;填写TSS的DS段
         mov [ebx+72],cx                    ;填写TSS的ES段
         mov [ebx+88],cx                    ;填写TSS的FS段
         mov [ebx+92],cx                    ;填写TSS的GS段
         ;将数据段作为用户任务3特权级固有堆栈
         AllocUserLiner                         ;宏:在用户任务地址空间上分配内存

         mov ebx,[esi+0x14]                     ;从TCB中获取TSS线性地址
         mov [ebx+80],cx                        ;填写TSS的SS段
         mov edx,[esi+0x06]                     ;填写高端线性地址
         mov [ebx+56],edx                       ;填写TSS的ESP段
        ;用户程序局部空间内创建0特权级堆栈
        AllocUserLiner                         ;宏:在用户任务地址空间上分配内存

        mov eax,0x0000_0000
        mov ebx,0x000f_ffff                
        mov ecx,0x00c0_9200                ;4K粒度的代码段描述符,特权级3
        call Flat4GBCodeSegSel:MakeSegDescriptor
        mov ebx,esi                        ;TCB基地址
        call FillDescriptorInLDT
        or cx,0x0000                       ;设置选择子特权级0
        mov ebx,[esi+0x14]                 ;TCB中获取TSS线性地址
        mov [ebx+8],cx                     ;填写TSS的SS0段
        mov edx,[esi+0x06]                 ;填写高端线性地址
        mov [ebx+4],edx                    ;填写TSS的ESP0段

        ;用户程序局部空间内创建1特权级堆栈
        AllocUserLiner                         ;宏:在用户任务地址空间上分配内存

        mov eax,0x0000_0000
        mov ebx,0x000f_ffff                
        mov ecx,0x00c0_b200                ;4K粒度的代码段描述符,特权级1
        call Flat4GBCodeSegSel:MakeSegDescriptor
        mov ebx,esi                        ;TCB基地址
        call FillDescriptorInLDT
        or cx,0x0001                       ;设置选择子特权级1
        mov ebx,[esi+0x14]                 ;TCB中获取TSS线性地址
        mov [ebx+16],cx                    ;填写TSS的SS1段
        mov edx,[esi+0x06]                 ;填写高端线性地址
        mov [ebx+12],edx                   ;填写TSS的ESP1段
        ;创建2特权级堆栈
        AllocUserLiner                         ;宏:在用户任务地址空间上分配内存

        mov eax,0x0000_0000
        mov ebx,0x000f_ffff                
        mov ecx,0x00c0_d200                ;4K粒度的代码段描述符,特权级2
        call Flat4GBCodeSegSel:MakeSegDescriptor
        mov ebx,esi                        ;TCB基地址
        call FillDescriptorInLDT
        or cx,0x0002                       ;设置选择子特权级2
        mov ebx,[esi+0x14]                 ;TCB中获取TSS线性地址
        mov [ebx+24],cx                    ;填写TSS的SS2段
        mov edx,[esi+0x06]                 ;填写高端线性地址
        mov [ebx+20],edx                   ;填写TSS的ESP2段
        ;重定位U-Salt
        cld
        mov ecx,[0x0c]              ;U-Salt项数
        mov edi,[0x08]              ;U-Salt在4GB空间内偏移

    .Part4:
        push ecx
        push edi
        mov ecx,SaltItems
        mov esi,Salt
    .Part5:
        push edi
        push esi
        push ecx

        mov ecx,64                  ;检索表中，条目比较次数
        repe cmpsd                  ;每次比较4Bit
        jnz .Part6
        mov eax,[esi]               ;若匹配，则esi恰好指向其后地址
        mov [edi-256],eax           ;将字符串改为偏移地址形式
        mov ax,[esi+4]
        or ax,0x0003                ;用户程序调用门，RPL=3
        mov [edi-252],ax            ;回填调用门选择子
    .Part6:
        pop ecx
        pop esi
        add esi,SaltItemLen
        pop edi                     ;从头比较
        loop .Part5

        pop edi
        add edi,256
        pop ecx
        loop .Part4
        ;GDT创建LDT描述符
        mov esi,[ebp+36]            ;从堆栈中获取TCB基地址
        mov eax,[esi+0x0c]          ;LDT起始线性地址
        movzx ebx,word [esi+0x0a]   ;LDT段界限
        mov ecx,0x0040_8200         ;LDT描述符，特权级0
        call Flat4GBCodeSegSel:MakeSegDescriptor
        call Flat4GBCodeSegSel:SetUpGDTDescriptor
        mov [esi+0x10],cx        ;登记LDT选择子到TCB
        mov ebx,[esi+0x14]       ;从TCB中获取TSS线性地址
        mov [ebx+96],cx          ;填写TSS的LDT段
        mov word [ebx+0],0       ;反向链=0
        mov dx,[esi+0x12]        ;段界限
        mov [ebx+102],dx         ;填写TSS的I/O位图偏移域
        mov word [ebx+100],0     ;T=0
        mov eax,[0x04]           ;从任务4GB地址空间获取入口点
        mov [ebx+32],eax         ;填写TSS的EIP段
        
        pushfd
        pop edx
        mov [ebx+36],edx         ;填写TSS EFLAGS段

        ;在GDT中登记Tss描述符
        mov eax,[esi+0x14]          ;Tss起始线性地址
        movzx ebx,word [esi+0x12]   ;段界限
        mov ecx,0x0040_8900         ;Tss描述符，特权级0
        call Flat4GBCodeSegSel:MakeSegDescriptor
        call Flat4GBCodeSegSel:SetUpGDTDescriptor
        mov [esi+0x18],cx           ;登记TSS选择子到TCB
        ;创建用户任务页目录
        call Flat4GBCodeSegSel:CreateCopyCurPdir
        mov ebx,[esi+0x14]          ;从TCB中获取TSS的线性地址
        mov dword [ebx+28],eax      ;填写TSS的CR3段
        popad
        ret 8                       ;丢弃调用本函数前入栈参数
;函数功能:在TCB链上追加任务控制块
;输入:ECX TCB线性基地址
AppendToTCBLink:
        cli
        push eax
        push ebx

        mov eax,TcbChain
    .Part1:                         ;EAX 链表头或当前TCB线性地址
        mov ebx,[eax]               ;EBX 下一个TCB线性地址
        or  ebx,ebx
        jz .Part2                   ;链表空或已是链尾          
        mov eax,ebx                 ;定位下一个TCB线性地址
        jmp .Part1
    .Part2:
        mov [eax],ecx
        mov dword [ecx],0x0000_0000;TCB最后一个指针清零
        pop ebx
        pop eax

        sti
        ret
;***********************************************************************************
start:                                          ;内核入口，由这里开始执行
       ;创建中断描述符表IDT，在此之前，禁止调用put_string过程，以及任何含有sti指令的过程。
       ;前20各中断向量用于处理异常
       mov eax,GeneralExceprionHandler          ;门代码所在段内偏移
       mov bx,Flat4GBCodeSegSel                 ;门代码所在段选择子
       mov cx,0x8e00                            ;32位中断门，0特权级
       call Flat4GBCodeSegSel:MakeGateDescriptor
       mov ebx,IdtLinearAddress                 ;中断描述符表线性地址
       xor esi,esi
    .Idt0:
        mov [ebx+esi*8],eax
        mov [ebx+esi*8+4],edx
        inc esi
        cmp esi,19                               ;安装前20个中断处理过程
        jle .Idt0
        ;剩下的给硬件使用或保留
        mov eax,GeneralInterruptHandler          ;门代码所在段内偏移
        mov bx,Flat4GBCodeSegSel                 ;门代码所在段选择子
        mov cx,0x8e00                            ;32位中断门，0特权级
        call Flat4GBCodeSegSel:MakeGateDescriptor
        mov ebx,IdtLinearAddress                 ;中断描述符表线性地址
    .Idt1:
        mov [ebx+esi*8],eax
        mov [ebx+esi*8+4],edx
        inc esi
        cmp esi,255                              ;安装普通中断处理过程
        jle .Idt1
        ;设置实时时钟中断处理过程
        mov eax,RTM0x70InterruptHadle            ;门代码所在段内偏移
        mov bx,Flat4GBCodeSegSel                 ;门代码所在段选择子
        mov cx,0x8e00                            ;32位中断门，0特权级
        call Flat4GBCodeSegSel:MakeGateDescriptor
        mov ebx,IdtLinearAddress                 ;中断描述符表线性地址
        mov [ebx+0x70*8],eax
        mov [ebx+0x70*8+4],edx
        ;准备开放中断
        mov word [pidt],256*8-1                 ;IDT界限
        mov dword [pidt+2],IdtLinearAddress
        lidt [pidt]                             ;加载中断描述符表寄存器IDTR
        ;设置8259A中断控制器
        mov al,0x11
        out 0x20,al                             ;ICW1:边缘触发/级联方式
        mov al,0x20
        out 0x21,al                             ;ICW2:起始中断向量表
        mov al,0x04
        out 0x21,al                             ;ICW3:从片级联IR2
        mov al,0x01
        out 0x21,al                             ;ICW4:非总线缓冲，全嵌套，正常EOI

        mov al,0x11
        out 0xa0,al                             ;ICW1:边缘触发/级联方式
        mov al,0x70
        out 0xa1,al                             ;ICW2:起始中断向量表
        mov al,0x04
        out 0xa1,al                             ;ICW3:从片级联IR2
        mov al,0x01
        out 0xa1,al                             ;ICW4:非总线缓冲，全嵌套，正常EOI
        ;设置定时器中断相关硬件
        mov al,0x0b             ;RTC寄存器B
        or al,0x80              ;阻断NMI
        out 0x70,al             
        mov al,0x12             ;设置寄存器B，禁止周期性中断，开放更新结束后中断，BCD码24小时制
        out 0x71,al

        in al,0xa1              ;读8259A从片IMR寄存器
        and al,0xfe             ;清除位0
        out 0xa1,al             ;写回寄存器

        mov al,0x0c
        out 0x70,al
        in al,0x71              ;读RTC寄存器C，复位中断状态
        ;中断设置工作完成，开放硬件中断
        sti
    
        mov ebx,message0
        call Flat4GBCodeSegSel:PutString

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
        call Flat4GBCodeSegSel:PutString
        mov ebx,CPU_Brand
        call Flat4GBCodeSegSel:PutString
        mov ebx,CPU_Brand1
        call Flat4GBCodeSegSel:PutString
        ;安装服务整个系统调用门
        mov edi,Salt                        ;C-Salt表起始位置
        mov ecx,SaltItems                   ;C-Salt表条目数量
    .Part10:
        push ecx
        mov eax,[edi+256]          ;该条目入口点32位偏移地址
        mov bx,[edi+260]           ;该条目入口点段选择子
        mov cx,0xec00              ;特权级3的调用门（3以上特权级允许访问），0个参数（寄存器而非栈传参）

        call Flat4GBCodeSegSel:MakeGateDescriptor
        call Flat4GBCodeSegSel:SetUpGDTDescriptor
        mov [edi+260],cx           ;将返回的内描述符选择子回填
        add edi,SaltItemLen        ;指向下一个C-Salt条目
        pop ecx
        loop .Part10
        ;对门测试
        mov ebx,message1
        call far [Salt1+256]                    ;通过调用门显示信息
        mov word [KernelTCB+0x04],0xffff        ;任务状态忙
        mov dword [KernelTCB+0x06],0x8010_0000  ;内核虚拟空间分配从这里开始

        mov word [KernelTCB+0x0a],0xffff        ;登记LDT初始界限到TCB
        mov ecx,KernelTCB
        call AppendToTCBLink                    ;将TCB续在链上
        AllocKernelLinear                       ;宏:在内核虚地址中分配内存
        ;在程序管理器TSS设置必要项目              
        mov word [ebx+0],0                      ;反向链=0
        mov eax,cr3
        mov dword [ebx+28],eax                  ;登记CR3 PDBR
        mov word  [ebx+96],0                    ;处理器允许没有LDT任务
        mov word  [ebx+100],0                   ;T=0
        mov word  [ebx+102],103                 ;没有I\O位图
        ;创建程序管理器TSS描述符，安装到GDT
        mov eax,ebx                     ;TSS起始线性地址
        mov ebx,103                     ;段长度
        mov ecx,0x0040_8900             ;TSS描述符，0特权级
        call Flat4GBCodeSegSel:MakeSegDescriptor
        call Flat4GBCodeSegSel:SetUpGDTDescriptor
        mov [KernelTCB+0x18],cx         ;登记内核任务TSS选择子到TCB
        ;任务寄存器TR中的内容是任务存在的标志，该内容也决定了当前任务是谁。
        ;下面的指令为当前正在执行的0特权级任务“程序管理器”后补手续（TSS）。
        ltr cx
        ;现在可认为“程序管理器”任务正在执行中
        ;创建用户任务的任务控制块
        AllocKernelLinear                ;宏:在内核虚地址中分配内存
        mov word [ebx+0x04],0                   ;任务状态空闲
        mov dword [ebx+0x06],0                  ;用户任务局部空间分配从0开始
        mov word  [ebx+0x0a],0xffff             ;登记LDT初始界限到TCB
        push dword UserProgameLocation1          ;用户程序所在逻辑扇区
        push ebx                                ;压入任务起始块控制块起始线性地址
        call LoadRelocateProgram
        mov ecx,ebx
        call AppendToTCBLink                    ;TCB 链接到 TCB链
        ;创建用户程序任务控制块
        AllocKernelLinear                       ;宏:在内核虚地址中分配内存
        mov word [ebx+0x04],0                   ;任务状态空闲
        mov dword [ebx+0x06],0                  ;用户任务局部空间分配从0开始
        mov word  [ebx+0x0a],0xffff             ;登记LDT初始界限到TCB
        push dword UserProgameLocation2         ;用户程序所在逻辑扇区
        push ebx                                ;压入任务起始块控制块起始线性地址
        call LoadRelocateProgram
        mov ecx,ebx
        call AppendToTCBLink                    ;TCB 链接到 TCB链
        
    .Kernel:
        mov ebx,KernelMsg0
        call Flat4GBCodeSegSel:PutString

        ;添加回收终止任务内存

        jmp .Kernel
KernelCodeEnd:
SECTION KernelTail
KernelEnd:
;该标号返回内核汇编地址