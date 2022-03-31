;****************************************************
;   初始化代码
;****************************************************
section Initial align=16 vstart=0x9000
EnterProtectMode:                                       ;至此，正式进入保护模式
    DisableInterrupt:                                   ;禁用中断
        cli                                             ;禁用中断汇编命令Clear Interupt；另Set Interupt(STI)为恢复中断
    SetupGDTDesc:                                       ;GDT设置
        mov ax,GDTEnd                                   
        sub ax,GDTStart
        sub ax,1                                        ;GDT结束位置-(GDT开始位置+1)=GDT范围
        mov [GDTStart],ax

        mov ax,GDTStart
        mov [GDTStart+2],ax                             ;GDT Base?
    LoadGDT:                                            ;加载临时GDT
        lgdt [GDTStart]                                
    %define SEC_DEFAULT_CR0 0x40000023
    %define SEC_DEFAULT_CR4 0x640                       ;设置CR寄存器各位

    EnterProtectBit:
        mov eax,SEC_DEFAULT_CR0
        mov CR0,eax
        jmp dword   CodeDescriptor:ProtectModeLand       ;跳转保护模式

BITS 32
ProtectModeLand:
    mov eax,SEC_DEFAULT_CR0
    mov CR4,eax
ResetSegmentRegister:                                     ;重置段寄存器
    mov ax,DataDescriptor
    mov ds,ax
    mov fs,ax
    mov gs,ax
    mov ss,ax
    mov ax,VideoDescriptor
    mov es,ax
NowUnderProtected:                                         ;进入保护模式
    mov esi,TipProtecteMode
    mov dl,0
    call PrintString
    mov edi,0x00008000

IsLongModeSupported:
    mov eax,0x80000000
    cpuid                                                  ;Inter获取cpuID命令
    cmp eax,0x80000000
    jbe NoLongMode
    mov eax,0x80000001
    cpuid
    bt edx,29
    jnc NoLongMode                                          ;不支持长模式
YesLongMode:                                                ;支持长模式，开启分页，进入64位
    mov esi,TipYesLongMode
    mov dl,0
    call PrintString
;在开启64位之前，需要定义页表
;这个参考EDKII的代码
  

  
  jmp InitialEnd
SeekTheKernelElf:                                            ;寻找内核文件
    cmp dword   [edi],'INIT'
    jne nextFile
    cmp dword   [edi+4],'TAL'
    jne nextFile
    cmp dword   [edi+8],'BIN'
    jne nextFile
    jmp KernelElfFound
    nextFile:
        cmp edi,0x9000
        ja NoKernelElf
        add edi,32
        jmp SeekTheKernelElf
KernelElfFound:                                               ;内核处理
    mov esi,TipKernelFound
    mov dl,0
    call PrintString
    mov ax,[di+0x1c]                                          ;获取内核文件长度
    mov dx,[di+0x1e]

    mov cx,512                                                ;文件长度是字节单位，除以512后得到扇区号
    div cx
    cmp dx,0                                                  ;若余数不为零则扇区号需要加一，因为扇区号是离散的
    je NoRemainder
    inc ax                                                    ;ax为待读取扇区数
    mov [BlockCount],ax
    NoRemainder:
        mov ax,[edi+0x1a]                                     ;文件起始簇号，也是转为扇区号，*8
        sub ax,2
        mov cx,8
        mul cx
    ;现在文件起始扇区号存在dx:ax，直接保存到ebx，这个起始是相对于DataBase 0x32,72
    ;所以待会计算真正的起始扇区号还需要加上DataBase
        and eax,0x0000ffff     
        add ebx,eax
        mov ax,dx
        shl eax,16
        add ebx,eax
        mov [BlockLow],ebx
        mov word [BufferOffset],0x9000
        mov di,0x9000

        mov si,TipGotoKernel
        mov dl,0
        call PrintString
NoKernelElf:
    mov esi,TipNoKernelElf
    mov dl,0
    call PrintString
    hlt
NoLongMode:
    mov esi,TipNoLongMode
    mov dl,0
    call PrintString
    jmp InitialEnd
InitialEnd:
    hlt

PrintCPUIDResult:                                   ;打印CPUID结果
    push ECX
    call SaveCPUResult
    mov esi,CPUIDResult
    mov ecx,52
    call PrintByteAscii
    pop ecx
    ret
PrintCPUIDResultHex:
    push ecx
    call SaveCPUResult
    mov esi,CPUIDResult
    mov ecx,4
    StartPrintCPUIDHex:
        call PrintOneRegister
        add esi,13
        loop StartPrintCPUIDHex
    pop ecx
    ret


PrintOneRegister:
    push ecx
    push esi 
    mov ecx,7
    call PrintNBytesAscii
    add esi,10
    mov ecx,4
    call PrintNBYtesHexTop
    add esi,1
    mov ecx,2
    call PrintNBytesAscii
    pop esi
    pop ecx
SaveCPUResult:                                      ;保存CPUIDE指令查询结果
    mov [reax],eax
    mov [rebx],ebx
    mov [recx],ecx
    mov [redx],edx
    ret
PrintNBYtesHexTop:                                  ;打印N个字节16进制表示，从高到低
    push ax
    push esi    
    StartPrintByteHexTop:
        mov al,[esi]
        call PrintByteHex
        dec esi
        loop StartPrintByteHexTop
    PrintNbytesHexEnd:
        pop esi
        pop ax
        ret
PrintByteHex:
    push bx
    mov bl,al
    shr al,4
    call PrintHalfByteHex
    mov al,bl
    and al,0x0f
    call PrintHalfByteHex
    mov al,bl
    pop bx
    ret

PrintHalfByteHex:                               ;打印半字节Hex,入参al
    push ax
    cmp al,0x09                                 ;比较当前值如果大于表示再A~F范围
    ja PrintAF                                  ;打印一个值ASCII
    PrintNum:                                   ;打印0~9数字
        add al,48
        call PrintByteAscii
        jmp PrintNbytesHexEnd
    PrintAF:                                    ;打印A~F字母
        add al,55
        call PrintByteAscii
        jmp PrintHexEnd
    PrintHexEnd:
        pop ax
        ret 
;打印普通字符串
;dl:结束符字节
PrintString:
  push ax
  push cx
  push dx
  push esi
  mov cx, 65535
  StartPrintString:
    mov al, [esi]
    cmp al, dl
    je PrintStringEnd
    call PrintByteAscii
    inc esi
    loop StartPrintString
  PrintStringEnd: 
  pop esi
  pop dx
  pop cx
  pop ax
  ret

;打印N个普通字符
;cx:打印的字符长度，也即N
;esi:需要打印的内存起始地址
PrintNBytesAscii:
    push    ax
    push    cx
    push    esi
    StartPrintByteAscii:
        mov al,[esi]
        call PrintByteAscii
        inc esi
    loop StartPrintByteAscii
    PrintNBytesAsciiEnd:
    pop esi
    pop cx
    pop ax
    ret
;打印普通字符，入参al
PrintByteAscii:
    push ax
    push dx
    push di
    push esi
    call GetCursor
    cmp al,0x0d             ;判断回车
    jz PrintCR
    cmp al,0x0a             ;判断换行
    jz PrintLF
    PrintNormal:
        mov [es:di],al
        inc esi
        add di,2
        call ShouldScreenRoll
        call SetCursor
        jmp PrintByteAsciiEnd
PrintCR:
    mov dl, 160
    mov ax, di
    div dl
    shr ax, 8
    sub di, ax
    call SetCursor
    inc esi
    jmp PrintByteAsciiEnd
PrintLF:
    add di, 160
    call ShouldScreenRoll
    call SetCursor
    inc esi
    jmp PrintByteAsciiEnd
PrintByteAsciiEnd:
    pop esi
    pop di
    pop dx
    pop ax
    ret
;滚屏判断
ShouldScreenRoll:
    cmp di,4000
    jb NoScreenroll             ;若超出显示范围则滚屏
    RollScreen:
        push ax
        push cx
        push ds
        push si
        cld
        mov ax,es
        mov ds,ax
        mov si,0xa0
        mov di,0x00
        mov cx,1920
        rep movsw
        mov di,3840
        call ClearOneLine
        pop si
        pop ds
        pop cx
        pop ax
    NoScreenroll:                ;没超出返回
        ret
    ClearOneLine:                ;滚动屏幕
        push di
        mov cx,80
        PrintBlackSpace:
            mov word [es:di],0x0720
            add di,2
            loop PrintBlackSpace
        sub di,160
        pop di
        ret
;获取光标位置
GetCursor:
  push ax
  push dx
  mov dx,0x3d4
  mov al,0x0e
  out dx,al
  mov dx,0x3d5
  in al,dx                        ;高8位 
  mov ah,al

  mov dx,0x3d4
  mov al,0x0f
  out dx,al
  mov dx,0x3d5
  in al,dx                        ;低8位 
  add ax, ax

  mov di, ax
  pop dx
  pop ax
  ret
;设置光标位置
SetCursor:
  push dx
  push bx
  push ax
  
  mov ax, di
  mov dx, 0
  mov bx, 2
  div bx

  mov bx, ax
  mov dx, 0x3d4
  mov al, 0x0e
  out dx, al
  mov dx, 0x3d5
  mov al, bh
  out dx, al
  mov dx, 0x3d4
  mov al, 0x0f
  out dx, al
  mov al, bl
  mov dx, 0x3d5
  out dx, al
  pop ax
  pop bx
  pop dx
  ret

;GDT表宏
%define PRESENT_FLAG(p) (p  <<  7)
%define DPL(dpl)        (dpl<<  5)
%define SYSTEM_FLAG(s)  (s  <<  4)
%define DESC_TYPE(t)    (t)

%define DATA32_TYPE 3    ;数据，可扩展，可写，可访问
%define CODE32_TYPE 0xb  ;代码，可执行，可读，可扩展，可访问
%define CODE64_TYPE 0xb  ;代码，可执行，可读，可扩展，可访问

;GDT定义宏
%define GRANULARITY_FLAG(g) (g  <<  7)
%define DEFAULT_SIZE32(d)   (d  <<  6)
%define CODE64_FLAG(l)      (l  <<  5)
%define UPPER_LIMIT(l)      (l)
GDTStart:                                             
  ;空描述符，将GDT描述48位拉过来                        
    NullDescriptor equ $-GDTStart                       ;这样算式保证每个段都是相对于GDT头的偏移
        dw 0                                            ;段界限，范围 00
        dw 0                                            ;15~0 02
        db 0                                            ;23~16 04
        db 0                                            ;sys flag,dpl,type 05
        db 0                                            ;范围19：16，flage 05
        db 0                                            ;31：24
    DataDescriptor equ $-GDTStart                       ;数据段描述符，用于字符模式下写入信息
        dw 0xffff                                       ;段界限，范围 00
        dw 0                                            ;15~0   02
        db 0                                            ;23~16  04
        db PRESENT_FLAG(1)|DPL(0)|SYSTEM_FLAG(1)|DESC_TYPE(DATA32_TYPE)
        db GRANULARITY_FLAG(1)|DEFAULT_SIZE32(1)|CODE64_FLAG(0)|UPPER_LIMIT(0xf)
        db 0                                            ;31~24
    VideoDescriptor equ $-GDTStart                      
        dw 0xffff                                       ;段界限，范围 00
        dw 0x800                                        ;15~0   02
        db 0x0b                                         ;23~16  04
        db PRESENT_FLAG(1)|DPL(0)|SYSTEM_FLAG(1)|DESC_TYPE(DATA32_TYPE)
        db GRANULARITY_FLAG(1)|DEFAULT_SIZE32(1)|CODE64_FLAG(0)|UPPER_LIMIT(0xf)
        db 0     
    CodeDescriptor equ $-GDTStart                       ;32位代码段描述符
        dw 0xffff                                       ;段界限，范围 00
        dw 0                                            ;15~0   02
        db 0                                            ;23~16  04
        db PRESENT_FLAG(1)|DPL(0)|SYSTEM_FLAG(1)|DESC_TYPE(CODE32_TYPE)
        db GRANULARITY_FLAG(1)|DEFAULT_SIZE32(1)|CODE64_FLAG(0)|UPPER_LIMIT(0xf)
        db 0     
    LongDescriptor equ $-GDTStart                       ;长模式代码段描述符
        dw 0xffff                                       ;段界限，范围 00
        dw 0                                            ;15~0   02
        db 0                                            ;23~16  04
        db PRESENT_FLAG(1)|DPL(0)|SYSTEM_FLAG(1)|DESC_TYPE(CODE64_TYPE)
        db GRANULARITY_FLAG(1)|DEFAULT_SIZE32(0)|CODE64_FLAG(1)|UPPER_LIMIT(0xf)
        db 0
GDTEnd:                                                 ;GDT结束地址
;提示信息
TipProtecteMode db 'Now you are under 32 bits Protected Mode!'          ;进入保护模式
                db 0x0d,0x0a,0
TipNoLongMode   db 'No,Long Mode is not supported on this computer.'    ;不支持长模式
                db 0x0d,0x0a,0
TipYesLongMode  db 'Yes! Long Mode is supported on this computer.'      ;进入长模式
                db 0x0d,0x0a,0
TipKernelFound  db 'Yes! Kernel.elf is not found,good for you!'         ;没有内核文件
                db 0x0d,0x0a,0
TipNoKernelElf    db 'Kernel.elf is not found on the disk,sad.'           ;内核文件不在硬盘
                db 0x0d,0x0a,0
TipGotoKernel   db 'Now we are going to jump to Kernel.elf,yes!'        ;跳转到内核文件
                db 0x0d,0x0a,0
DiskAddressPacker:
    PackSize        db  0x10     ;包大小，目前恒为16/0x10，0x00
    Reserved        db  0        ;保留字节
    BlockCount      dw  0        ;读取数据块个数 
    BufferOffset    dw  0        ;目标地址内存偏移
    BufferSegment   dw  0        ;目标地址段
    BlockLow        dd  0        ;磁盘起始绝对地址，单位：扇区 ，低字节部分
    BlockHigh       dd  0        ;高字节

FlagsTip:
    flagtip     db 'EFLAGS: '       ;标志位提示信息
    cf  db  'cf'
        db  'CF'
    pf  db  'pf'
        db  'PF'
    af  db  'af'   
        db  'AF'
    zf  db  'zf'
        db  'ZF'
    sf  db  'sf'
        db  'SF'
    tf  db  'tf'
        db  'TF'
    if  db  'if'
        db  'IF'
    df  db  'df'
        db  'DF'
    of  db  'of'
        db  'OF'
CPUIDResult:        ;计算结果
    teax    db  'EAX: 0x'
    reax    dd  0
            db  '   '
    tebx    db  'EBX: 0x'
    rebx    dd  0
            db  '   '
    tecx    db  'ECX: 0x'
    recx    dd  0
            db  '   '
    tedx    db  'EAX: 0x'
    redx    dd  0
            db  0x0d,0x0a