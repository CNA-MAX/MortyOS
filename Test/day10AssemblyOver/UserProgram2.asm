;用户程序
;在100号扇区

        ProgramLength  dd  ProgramEnd                   ;程序长度
        EntryPoint     dd  start                        ;程序入口
        SaltPosition   dd  SaltBegin                    ;SALT表起始偏移地址
        SaltItems      dd  (SaltEnd-SaltBegin)/256      ;Salt表条目数
        ;调用内核函数
        SaltBegin:

        PrintString         db  '@Kernel_PrintString'
                    times 256-($-PrintString) db 0                  ;不足补零
        TerminateProgram    db  '@Kernel_TerminateProgram'
                     times 256-($-TerminateProgram) db 0            ;不足补零
        ReadDiskData        db  '@Kernel_ReadDiskData'
                     times 256-($-ReadDiskData) db 0                ;不足补零
        PrintDwordAsHexString db '@Kernel_PrintDwordAsHexString'
                     times 256-($-PrintDwordAsHexString) db 0       ;不足补零
        SaltEnd:
        message0        db      'User task B->..................'
                        db      0x0d,0x0a,0

[bits 32]
start:  
        mov ebx,message0
        call far [PrintString]
 
        jmp start
        call far [TerminateProgram]   ;控制权交回系统

ProgramEnd: