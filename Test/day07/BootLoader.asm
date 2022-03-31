;****************************************************
;             BootLoader实现逻辑
;程序基础设置------------------不支持扩展0x13号中断报错
;读取FAT32核心逻辑以及工具函数
;   寻找活动分区---------------无活动分区报错
;   计算数据区
;读取所需数据结构
;   读取根目录 ----------------根目录读取失败报错   
;   寻找Initial---------------找不到报错
;   计算文件长度
;   读取并运行-----------------不能正常读取报错
;****************************************************
section Initial vstart=0x7c00                       ;检查硬件设备
InitialTheSegmentRegister:                          ;将段寄存器初始化为0x0000，如此保证后续所有程序地址均为相对0x0000的偏移
    xor ax,ax
    mov ds,ax
    mov es,ax
    mov ss,ax
SetupTheStackPointer:                               ;定义栈空间，栈底为0x7c00
    mov sp,0x7c00
Start:                                              ;程序开始
    mov si,BootLoaderStart                          ;提示开始
    call PrintString
CheckInt13:                                         ;检查硬件是否支持扩展13号中断
    mov ah,0x41
    mov bx,0x55aa
    mov dl,0x80
    int 0x13
    cmp bx,0xaa55
    mov byte [ShitHappens + 0x06],0x31
    jnz BootLoaderEnd
SeekTheActivePartition:                             ;寻找MBR分区表中活动分区，依据是看分区项是否为0x80，最多有4个分区
    ;分区表位于0x7c00+446=0x7c00+0x01be=0x7dbe位置，使用di作为基地址寄存器
    mov di,0x7dbe
    mov cx,4                                        ;循环4次遍历分区
    isActivePartition:
        mov bl,[di]                                 ;将分区头数据存储到dl寄存器
        cmp bl,0x80                                 ;对比值
        je  ActivePartitionFound                    ;找到跳转
        add di,16                                   ;找不到循环
        loop isActivePartition
        ActivePartitionNotFound:
            mov byte [ShitHappens+0x06],0x32        ;没有活动分区，返回错误码
            jmp BootLoaderEnd
        ActivePartitionFound:                       ;找到活动分区后，di就是活动分区项首地址
            mov si,PartitionFound                   ;报告发现分区
            call PrintString                        
            mov ebx,[di+8]                          ;ebx保存活动分区项首地址
            mov dword [BlockLow],ebx                ;目标内存起始地址
            mov word [BufferOffset],0x7e00          ;目标地址偏移
            mov byte [BlockCount],1                 ;读取第一个扇区
            call ReadDisk
GatFirstFat:                                        ;获取文件分配表头数据
    mov di,0x7e00
    xor ebx,ebx                                     ;ebx保存扇区数，先清零
    mov bx,[di+0x0e]
    mov eax,[di+0x1c]                               
    add ebx,eax                                     ;起始扇区号=隐藏扇区+保留扇区
GetDataAreaBase:                                    ;获取数据区起始扇区号
    mov eax,[di+0x24]
    xor cx,cx
    mov cl,[di+0x10]            
    AddFatSize:                                     ;遍历文件分配表
        add ebx,eax                                 
        loop AddFatSize
ReadRootDirectory:                                  ;以簇为单位读取数据区
    mov [BlockLow],ebx      
    mov word [BufferOffset],0x8000
    mov di,0x8000
    mov byte [BlockCount],8
    call ReadDisk
    mov byte [ShitHappens+0x06],0x34
SeekTheInitialBin:                                  ;通过文件名检索Initail.bin文件，在磁盘中，保存名是大写
    cmp dword [di],'INIT'
    jne nxetFile
    cmp dword [di+4],'TAL'
    jne nxetFile
    cmp dword [di+8],'BIN'
    jne nxetFile
    jmp InitialBinFound
    nxetFile:
        cmp di,0x9000
        ja BootLoaderEnd
        add di,32
        jmp SeekTheInitialBin

InitialBinFound:                                    ;获取该文件相关数据
    mov si,InitialFound
    call PrintString
    mov ax,[di+0x1c]                                ;获取文件长度
    mov dx,[di+0x1e]
    mov cx,512                                      ;文件长度除以512得到扇区数
    div cx
    cmp dx,0                                        ;若余数不为0需多读一个扇区数据才能完整
    je NoRemainder                              
    inc ax
    mov [BlockLow],ax
    NoRemainder:                                    ;文件起始簇号*8转为扇区号
        mov ax,[di+0x1a]                            
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
    call ReadDisk                                   ;跳转到initial.bin
    mov si ,GotoInitial
    call PrintString
    jmp di

BootLoaderEnd:
    mov si,ShitHappens
    call PrintString
    hlt                                             ;当出错将打印错误提示并暂停程序执行

DiskAddressPacket:                                  ;扩展int 13h号中断硬盘读取结构体
    PackSize db 0x10                                ;定义包大小16
    Reserved db 0                                   ;保留字节数0
    BlockCount dw 0                                 ;要读取数据块个数
    BufferOffset dw 0                               ;目标地址偏移
    BufferSegment dw 0                              ;目标内存地址的段
    BlockLow dd 0                                   ;磁盘起始绝对位置，单位：扇区，这是低字节部分0x08
    BlockHigh dd 0                                  ;高字节部分

ReadDisk:
    mov ah,0x42                                     ;当ah为0x42时为拓展磁盘读取
    mov al,0x80
    mov si,DiskAddressPacket
    int 0x13
    test ah,ah
    mov byte [ShitHappens+0x06],0x33
    jnz BootLoaderEnd
    ret     


PrintString:                        ;打印以0x0a结尾的字符串
    push ax
    push cx
    push si
    mov cx,512
        PrintChar:
            mov al,[si]
            mov ah,0x0e
            int 0x10
            cmp byte [si],0x0a
            je Return
            inc si
            loop PrintChar
        Return:
            pop si
            pop cx
            pop ax
            ret
    

ImportantTips:
    BootLoaderStart db  'Start Booting!'
                    db 0x0d,0x0a
    PartitionFound  db  'Get Partition!'
                    db 0x0d,0x0a
    InitialFound    db  'Get Initial'
                    db 0x0d,0x0a
    GotoInitial     db  'Go to Initial'                
                    db 0x0d,0x0a 
    ShitHappens     db  'Error 0,check your code!'                
                    db 0x0d,0x0a                 

    times 446-($-$$) db 0