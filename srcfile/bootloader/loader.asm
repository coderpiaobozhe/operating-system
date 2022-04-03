org 0x1000

dw 0xAA55;used to check whether the file is broken or not.
;----- display string
mov si, LoadMsg
call Func_Print
;----- detect memory
xor ebx, ebx
; set the address of ards buffer
mov ax, 0
mov es, ax
mov edi, ards_buffer
mov edx, 0x534D4150;ASCII code for string 'SWAP'
Loop_DetMem:
    mov ecx, 20; size of ards structure
    mov eax, 0xE820
    int 0x15
    ;if CF != 0 then show error on screen
    jc Error
    add di, cx; the next ards
    inc word [ards_count]
    cmp ebx, 0
    ;if ebx !=0 then continue detecting memory
    jnz Loop_DetMem
    ;finish detecting
    mov si, DetectMsg
    call Func_Print

Load_Protect_Mode:
    ; open address A20
    push ax
    in al, 92h
    or al, 0b10
    out 92h, al
    pop ax

    cli; close interrupt program
    
    lgdt [gdt32_ptr]; load gdt

    ; start protect mode
    mov eax, cr0; cr0 register has more than 1 bits, we just need to set the zeroth bit.
    or eax, 1
    mov cr0, eax
    jmp dword code32_selector:Protect_Mode
Func_Print:
    push ax
    mov ah, 0xE
    .next:
        mov al, [si]
        cmp al, 0
        jz .return
        int 10h
        inc si
        jmp .next
    .return:
        pop ax
        ret
LoadMsg: db "Begin to load", 10, 13, 0
DetectMsg: db "Detecting Memory completed", 10, 13, 0
Error:
    mov si, .errmsg
    call Func_Print
    hlt
    .errmsg db "Error : Failed to load", 10, 13, 0
[bits 32]
Protect_Mode:
    mov ax, 0x10
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax
    mov esp, 0x10000; change the stack top
    ; get kernel
    mov edi, 0x100000;target memory address
    mov ecx, 10;tenth sector
    mov bl, 200;amount of sectors
    call Func_Read
    call Func_Check_Sup
    test eax, eax
    jz Not_Sup
    mov dword [0x90000], 0x91007
    mov dword [0x90800], 0x91007
    mov dword [0x91000], 0x92007
    mov dword [0x92000], 0x000083
    mov dword [0x92008], 0x200083
    mov dword [0x92010], 0x400083
    mov dword [0x92018], 0x600083
    mov dword [0x92020], 0x800083
    mov dword [0x92028], 0xa00083
    db 0x66
    lgdt [gdt64_ptr]
    mov ax, 0x10
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax
    mov esp, 0x10000; change the stack top
    ; open PAE(physical address extension)
    mov eax, cr4
    bts eax, 5
    mov cr4, eax
    ; load cr3
    mov eax, 0x90000
    mov cr3, eax
    ; enable long mode
    mov ecx, 0C0000080h
    rdmsr
    bts eax, 8
    wrmsr
    ;open PE and paging
    mov eax, cr0
    bts eax, 0
    bts eax, 31
    mov cr0, eax
    jmp dword code64_selector:0x100000
; LBA Mode
Func_Read:
    push ecx
    push dx
    push ax
    ;set the amount of sectors
    mov dx, 0x1F2
    mov al, bl
    out dx, al 
    ; 0x1F3
    inc dx
    mov al, cl
    out dx, al
    ; 0x1F4
    inc dx
    shr ecx, 8
    mov al, cl
    out dx, al
    ; 0x1F5
    inc dx
    shr ecx, 8
    mov al, cl
    out dx, al
    ; 0x1F6
    inc dx 
    shr ecx, 8 
    mov al, 0b1110_0000 
    and cl, 0b0000_1111 
    or al, cl 
    out dx, al 
    ; 0x1F7(read data from disk)
    inc dx 
    mov al, 0x20 
    out dx, al 

    xor ecx, ecx
    mov cl, bl
    .read:
        ;check if the disk is ready
        mov dx, 0x1F7
        .ready:
            in al, dx
            jmp $+2
            jmp $+2
            jmp $+2
            and al, 0b1000_1000
            cmp al, 0b0000_1000
            jnz .ready
        ;read data from the disk
        mov dx, 0X1F0
        push cx
        mov cx, 256 ; one sector contains 256 words
        .reading:
            in ax, dx
            jmp $+2
            jmp $+2
            jmp $+2
            mov [edi], ax
            add edi, 2
            loop .reading
        pop cx
        loop .read
    pop ax
    pop dx
    pop ecx
    ret
Func_Check_Sup:
    mov eax, 0x80000000
    cpuid
    cmp eax, 0x80000001
    setnb al
    jb Func_Check_Done
    mov eax, 0x80000001
    cpuid
    bt edx, 29
    setc al
Func_Check_Done:
    movzx eax, al
    ret
Not_Sup:
    mov byte[0xb8000], 'N'
    jmp $


code32_selector equ (1 << 3)
data32_selector equ (2 << 3)
; memory_base equ 0; memory base address
; memory_limit equ ((1024*1024*1024*4)/(1024*4))-1; memory size
gdt32_ptr:
    dw (gdt32_end - gdt32_base) - 1
    dd gdt32_base
gdt32_base:
    dd 0, 0; NULL discriptor, 8 bytes.
gdt32_code: dd 0x0000FFFF, 0x00C09A00
    ; dw memory_limit & 0xFFFF; memory limit, 0-15 bit
    ; dw memory_base & 0xFFFF; base address, 0-15 bit
    ; db (memory_base & 0xFF0000) >> 16; base address, 16-23 bit
    ; db 0b_1_00_1_1_0_1_0; on memory, dpl = 0, code or data, code, not dependent, can be read, not been visited by cpu yet
    ; db 0b_1_1_0_0_0000 | (memory_base >> 16) & 0xF; unit=4k, 32 bit, not 64bit, memory limit(16-19 bit)
    ; db (memory_base >> 24) & 0xFF; base address, 24-31 bit
gdt32_data: dd 0x0000FFFF, 0x00C09200
    ; dw memory_limit & 0xFFFF; memory limit, 0-15 bit
    ; dw memory_base & 0xFFFF; base address, 0-15 bit
    ; db (memory_base & 0xFF0000) >> 16; base address, 16-23 bit
    ; db 0b_1_00_1_0_0_1_0; on memory, dpl = 0, code or data, data, up extension, can be written, not been visited by cpu yet
    ; db 0b_1_1_0_0_0000 | (memory_base >> 16) & 0xF; unit=4k, 32 bit, not 64bit, memory limit(16-19 bit)
    ; db (memory_base >> 24) & 0xFF; base address, 24-31 bit  
gdt32_end:

code64_selector equ gdt64_code - gdt64_base
data64_selector equ gdt64_data - gdt64_base
gdt64_ptr:
    dw (gdt64_end - gdt64_base) - 1
    dd gdt64_base
gdt64_base:
    dq 0; NULL discriptor, 8 bytes.
gdt64_code: dq 0x00209A0000000000
gdt64_data: dq 0x0020920000000000
gdt64_end:

ards_count:
    dw 0
ards_buffer:
