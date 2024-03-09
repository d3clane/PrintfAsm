section .text

global MyPrintf

;------------------------------------------------
; Trampoline for _MyPrintf to push args on stack 
; Entry: stdcall
; Exit : None
;------------------------------------------------
MyPrintf:
            ; emulating cdecl call - delete ret adr, push args
            mov [rel MyPrintfTrampolineSaveR12], r12    ; saving r12
            mov  r12, [rsp]                             ; saving ret adr
            
            mov [rsp], r9   ; arg instead of ret adr
            push r8
            push rcx
            push rdx
            push rsi
    
            call _MyPrintf

            ; returning everything as it was
            add rsp, 8 * 4  ; returning stack  as it was
            mov [rsp], r12  ; returing ret adr as it was
            mov r12, [rel MyPrintfTrampolineSaveR12]    ; return r12
            ret
  
;------------------------------------------------
; Printf 
; Entry: rdi - string, other args in Cdecl format
; Exit : None
;------------------------------------------------
_MyPrintf:  push rbp
            mov  rbp, rsp

            push r13    ; will be used for saving rdi
            push r12    ; will be used for saving args adr

            mov  r12, rbp
            add  r12, 0x8   ; pointing on ret adr
MyPrintfLoop1:
            cmp byte [rdi], 0x0    ; '\0' char
            je MyPrintfLoop1End

            mov rsi, '%'
            call PrintUntilChar
            ; prints and moves rdi (buffer ptr)

            cmp byte [rdi], '%'
            jne MyPrintfLoop1
            
            inc rdi     ; moving from % to the next one

            cmp byte [rdi], '%'
            jne MyPrintfSwitch1

            mov r13, rdi
            call PutCharFromBuffer
            mov rdi, r13
            inc rdi

            jmp MyPrintfLoop1

MyPrintfSwitch1:
            mov r13, rdi        ; saving rdi at the moment

            xor rax, rax
            mov al, [rdi]
            cmp al, 'b'
            jb MyPrintfSwitch1_end
            cmp al, 'x'
            ja MyPrintfSwitch1_end

            sub al, 'b'

            add r12, 0x8        ; moving to the next one arg
            mov rdi, [r12]      ; argument for print functions

            lea rdx, [rel MyPrintfSwitch1Table]
            add rdx, [rdx + 8 * rax]    ; calculating jump with switch table
            jmp rdx

MyPrintfSwitch1_b:
            call PrintBinaryInt
            jmp MyPrintfSwitch1_end
MyPrintfSwitch1_c:
            call PrintChar
            jmp MyPrintfSwitch1_end
MyPrintfSwitch1_d:
            call PrintDecimalInt
            jmp MyPrintfSwitch1_end        
MyPrintfSwitch1_o:
            call PrintOctalInt
            jmp MyPrintfSwitch1_end  
MyPrintfSwitch1_s:
            call PrintString
            jmp MyPrintfSwitch1_end  
MyPrintfSwitch1_x:
            call PrintHexInt
            jmp MyPrintfSwitch1_end  
MyPrintfSwitch1_default:
            jmp MyPrintfSwitch1_end

MyPrintfSwitch1_end:
            mov rdi, r13        ; saved rdi back to register
            inc rdi             ; next char

            jmp MyPrintfLoop1

MyPrintfLoop1End:
            pop r12
            pop r13

            mov rsp, rbp
            pop rbp
            ret

;------------------------------------------------
; Puts char from buffer to the stdout
; Entry: (char* buffer)
; Exit : char ASCII or -1 in case of error
;------------------------------------------------
PutCharFromBuffer:  
            mov rax, 0x01                           ; preparing for write syscall
            mov rsi, rdi                            ; char buffer saving
            mov rdi, 1                              ; stdout descriptor
            mov rdx, 1                              ; size of buffer
            syscall

            ret


;------------------------------------------------
; Prints string until rsi or '\0' char
; Entry: (char** buffer)
; Exit : rax - number of chars printed,
;        buffer ptr moved to the end of printing
;------------------------------------------------
PrintUntilChar:
            mov rdx, 0

PrintUntilCharLoop:
            cmp byte [rdi], sil
            je PrintUntilCharLoopEnd
            cmp byte [rdi], 0x0     ; '\0' char
            je PrintUntilCharLoopEnd
            inc rdi
            inc rdx
            jmp PrintUntilCharLoop

PrintUntilCharLoopEnd:
            ;calling write syscall 
            mov rax, 0x01
            push rdi

            sub rdi, rdx
            mov rsi, rdi
            mov rdi, 1      ; stdout descriptor 

            push rdx
            sub rsp, 0x8    ; 16 byte aligning
            syscall     
            add rsp, 0x8    
            pop rdx
            pop rdi

            mov rax, rdx
            ret

;------------------------------------------------
; Prints int in binary format
; Entry: (int val)
; Exit : None
;------------------------------------------------
PrintBinaryInt:
            push rbp
            mov  rbp, rsp
            
PrintBinaryIntArrSize equ 0x30

            sub rsp,  PrintBinaryIntArrSize     ; saving 32 bytes for number + 0b prefix
            mov rax,  -1                        ; counter for array

PrintBinaryIntLoop:
            cmp rdi, 0x0    ; stop pushing bits in case val is 0
            je PrintBinaryIntLoopEnd

            mov rcx, rdi
            and rcx, 0x1    ; saving only least bit
            add rcx, '0'    ; creating ASCII

            mov [rbp + rax], cl    ; pushing least bit in buf
            dec rax
            shr rdi, 1
            jmp PrintBinaryIntLoop

PrintBinaryIntLoopEnd:

            ;'0b' prefix
            mov byte [rbp + rax], 'b'
            dec rax
            mov byte [rbp + rax], '0'
            dec rax

            ;preparing for syscall write 
            mov rdx, rax
            not rdx
            ; rdx = -rax = ~rax + 1. No inc because actual size is rdx - 1

            lea rsi, [rbp + rax + 1]  ; string

            mov rax, 0x1        ; preparing for syscall write
            mov rdi, 1          ; stdout file descriptor

            syscall

            mov rsp, rbp
            pop rbp
            ret

;------------------------------------------------
; Prints char
; Entry: (char ch)
; Exit : None
;------------------------------------------------
PrintChar:
            push rdi
            ; preparing for write syscall
            mov rax, 0x1    ; write syscall
            mov rdi, 0x1    ; stdout
            mov rsi, rsp    ; char ptr
            mov rdx, 1      ; length
            syscall

            pop rdi
            ret

;------------------------------------------------
; Prints int in decimal format
; Entry: (int val)
; Exit : None
;------------------------------------------------
PrintDecimalInt:
            push rbp
            mov  rbp, rsp
            
PrintDecimalIntArrSize equ 0x10

            sub rsp,  PrintDecimalIntArrSize    ; saving 10 bytes for number
            mov rax, rdi                        ; saving value in rax for dividing
            mov rdi,  -1                        ; counter for array
            mov r8,  10d                        ; saving for dividing
PrintDecimalIntLoop:
            cmp rax, 0x0    ; stop pushing bits in case val is 0
            je PrintDecimalIntLoopEnd

            xor rdx, rdx
            div r8
            
            add rdx, '0'    ; creating ASCII

            mov [rbp + rdi], dl    ; pushing ASCII
            dec rdi
            jmp PrintDecimalIntLoop

PrintDecimalIntLoopEnd:
            ;preparing for syscall write 
            mov rdx, rdi
            not rdx
            ; rdx = -rax = ~rax + 1. No inc because actual size is rdx - 1

            lea rsi, [rbp + rdi + 1]  ; string

            mov rax, 0x1        ; preparing for syscall write
            mov rdi, 1          ; stdout file descriptor

            syscall

            mov rsp, rbp
            pop rbp
            ret

;------------------------------------------------
; Prints int in octal format
; Entry: (int val)
; Exit : None
;------------------------------------------------
PrintOctalInt:
            push rbp
            mov  rbp, rsp
            
PrintOctalIntArrSize equ 0x10

            sub rsp,  PrintOctalIntArrSize      ; saving bytes for val + prefix '0'
            mov rax,  -1                        ; counter for array

PrintOctalIntLoop:
            cmp rdi, 0x0    ; stop pushing bits in case val is 0
            je PrintOctalIntLoopEnd

            mov rcx, rdi
            and rcx, 0x7    ; saving only 3 least bits
            add rcx, '0'    ; creating ASCII

            mov [rbp + rax], cl    ; pushing least bits in buf
            dec rax
            shr rdi, 3
            jmp PrintOctalIntLoop

PrintOctalIntLoopEnd:

            ;'0' prefix
            mov byte [rbp + rax], '0'
            dec rax

            ;preparing for syscall write 
            mov rdx, rax
            not rdx
            ; rdx = -rax = ~rax + 1. No inc because actual size is rdx - 1

            lea rsi, [rbp + rax + 1]  ; string

            mov rax, 0x1        ; preparing for syscall write
            mov rdi, 1          ; stdout file descriptor

            syscall

            mov rsp, rbp
            pop rbp
            ret


;------------------------------------------------
; Prints string
; Entry: (char* str)
; Exit : None
;------------------------------------------------
PrintString:
            mov rsi, rdi    ; saving my string ptr
            call StrLen     

            ; preparing for write syscall
            mov rdi, 0x1    ; stdout
            mov rdx, rax    ; length
            mov rax, 0x1    ; call number
            ; rsi is already saved as a string
            syscall

            ret

;------------------------------------------------
; Prints int in hex format
; Entry: (int val)
; Exit : None
;------------------------------------------------
PrintHexInt:
            push rbp
            mov  rbp, rsp
            
PrintHexIntArrSize equ 0x10

            sub rsp,  PrintHexIntArrSize      ; saving bytes for val + prefix '0x'
            mov rax,  -1                        ; counter for array

PrintHexIntLoop:
            cmp rdi, 0x0    ; stop pushing bits in case val is 0
            je PrintHexIntLoopEnd

            mov rcx, rdi
            and rcx, 0xf    ; saving only 3 least bits

            cmp rcx, 0xa 
            jb PrintHexIntDigitIsNumber
            add rcx, 'A' - 0xa    ; creating ASCII
            jmp PrintHexIntPushDigitASCII
PrintHexIntDigitIsNumber:
            add rcx, '0'    ; creating ASCII
PrintHexIntPushDigitASCII:
            mov [rbp + rax], cl    ; pushing least bits in buf
            dec rax
            shr rdi, 4
            jmp PrintHexIntLoop

PrintHexIntLoopEnd:

            ;'0x' prefix
            mov byte [rbp + rax], 'x'
            dec rax
            mov byte [rbp + rax], '0'
            dec rax

            ;preparing for syscall write 
            mov rdx, rax
            not rdx
            ; rdx = -rax = ~rax + 1. No inc because actual size is rdx - 1

            lea rsi, [rbp + rax + 1]  ; string

            mov rax, 0x1        ; preparing for syscall write
            mov rdi, 1          ; stdout file descriptor

            syscall

            mov rsp, rbp
            pop rbp
            ret


;------------------------------------------------
; Returns strlen of null terminated string
; Entry: (char* str)
; Exit : rax - length
;------------------------------------------------
StrLen:
            cld 

            xor rcx, rcx
            dec rcx
            mov al, 0x0     ; null terminating 
            repne scasb
            not rcx
            dec rcx

            mov rax, rcx
            ret


section .data

MyPrintfTrampolineSaveR12 dq 0

section .rodata
 
MyPrintfSwitch1Table:
            dq MyPrintfSwitch1_b       - MyPrintfSwitch1Table
            dq MyPrintfSwitch1_c       - MyPrintfSwitch1Table
            dq MyPrintfSwitch1_d       - MyPrintfSwitch1Table

            dq MyPrintfSwitch1_default - MyPrintfSwitch1Table
            dq MyPrintfSwitch1_default - MyPrintfSwitch1Table
            dq MyPrintfSwitch1_default - MyPrintfSwitch1Table
            dq MyPrintfSwitch1_default - MyPrintfSwitch1Table
            dq MyPrintfSwitch1_default - MyPrintfSwitch1Table
            dq MyPrintfSwitch1_default - MyPrintfSwitch1Table
            dq MyPrintfSwitch1_default - MyPrintfSwitch1Table
            dq MyPrintfSwitch1_default - MyPrintfSwitch1Table
            dq MyPrintfSwitch1_default - MyPrintfSwitch1Table
            dq MyPrintfSwitch1_default - MyPrintfSwitch1Table

            dq MyPrintfSwitch1_o       - MyPrintfSwitch1Table

            dq MyPrintfSwitch1_default - MyPrintfSwitch1Table
            dq MyPrintfSwitch1_default - MyPrintfSwitch1Table
            dq MyPrintfSwitch1_default - MyPrintfSwitch1Table

            dq MyPrintfSwitch1_s       - MyPrintfSwitch1Table

            dq MyPrintfSwitch1_default - MyPrintfSwitch1Table
            dq MyPrintfSwitch1_default - MyPrintfSwitch1Table
            dq MyPrintfSwitch1_default - MyPrintfSwitch1Table
            dq MyPrintfSwitch1_default - MyPrintfSwitch1Table

            dq MyPrintfSwitch1_x       - MyPrintfSwitch1Table
