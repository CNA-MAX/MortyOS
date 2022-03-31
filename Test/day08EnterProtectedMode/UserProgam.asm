;用户程序
;在50号扇区
;将100号扇区Hello.txt读出来
        HelloLocation  equ    100                     ;逻辑扇区号

SECTION Header vstart=0                              ;程序头记录程序相关信息供段选择子提取
        ProgramLength      dd   ProgramEnd           ;程序总长

        HeadLen     dd   HeadEnd                     ;头长
        StackSeg    dd   0                           ;接收堆栈段选择子 0x08
        StackLen    dd   1                           ;程序堆栈大小，粒度4KB

        ProgramEntry    dd  start                    ;程序入口地址 0x10
        CodeSeg         dd  section.code.start       ;代码段位置   0x14
        CodeLen         dd  CodeEnd                  ;代码段长度   0x18
        DataSeg         dd  section.data.start       ;代码段位置   0x1C
        DetaLen         dd  DataEnd                  ;代码段长度   0x20
        ;SALT 
        SaltItems   dd  (HeadEnd-salt)/256         ;初始化SALT中有多少项，每项长度256   0x24

        salt:                                        ;0x28
        PrintString db  '@Kelnel_PrintString'
                    times 256-($-PrintString) db 0         ;不足补零
        TerminateProgram    db  '@Kelnel_TerminateProgram'
                     times 256-($-TerminateProgram) db 0   ;不足补零
        ReadDiskData        db  '@Kelnel_ReadDiskData'
                     times 256-($-ReadDiskData) db 0       ;不足补零

HeadEnd:
SECTION data    vstart=0
        buffer  times   1024    db  0                   ;缓冲区
        message1    db  0x0d,0x0a,0x0d,0x0a
                    db  '**********User program is runing**********'
                    db  0x0d,0x0a,0
        message2    db  'Disk data',0x0d,0x0a,0
DataEnd:
[bits 32]
SECTION code    vstart=0
start:  
        mov eax,ds
        mov fs,eax

        mov eax,[StackSeg]
        mov ss,eax
        mov esp,0

        mov eax,[DataSeg]
        mov ds,eax

        mov ebx,message1
        call far [fs:PrintString]

        mov eax,HelloLocation           ;逻辑扇区号100
        mov ebx,buffer                  ;缓冲区偏移地址
        call far [fs:ReadDiskData]      ;段间调用

        mov ebx,message2    
        call far [fs:PrintString]

        mov ebx,buffer
        call far [fs:PrintString]

        jmp far [fs:TerminateProgram]   ;控制权交回系统
CodeEnd:
SECTION CodeTail
ProgramEnd: