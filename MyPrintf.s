section .text

global MyPrintf

NumberIsNegative equ 1

;------------------------------------------------
; Trampoline for _MyPrintf to push args on stack 
; Entry: stdcall
; Exit : None
;------------------------------------------------
MyPrintf:
            ; emulating cdecl call - delete ret adr, push args
            mov rax, [rsp]  ; saving ret adr
            mov [rsp], r9   ; arg instead of ret adr
            push r8
            push rcx
            push rdx
            push rsi
            push rax        ; pushing ret adr

            jmp _MyPrintf   ; no calling -> no extra ret adr

            ; returning everything as it was
MyPrintfReturn:
            mov rdi, [rsp]  ; saving ret adr
            add rsp, 8 * 5  ; returning stack  as it was
            mov [rsp], rdi  ; returing ret adr as it was

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
            mov dil, '%'
            call PrintChar
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
            add rdx, [rdx + 8 * rax]    ; calculating jump with jump table
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

            ; syscall write for buffer
            mov rax, 0x1
            mov rdi, 0x1
            lea rsi, [rel PrintfBuffer]
            mov rdx, [rel PrintfBufferSize]
            syscall
            
            mov rsp, rbp
            pop rbp
            
            jmp MyPrintfReturn  ; instead of ret

;------------------------------------------------
; Prints string until rsi or '\0' char
; Entry: (char** buffer)
; Exit : rax - number of chars printed,
;        buffer ptr moved to the end of printing
;------------------------------------------------
PrintUntilChar:
            xor rdx, rdx

PrintUntilCharLoop:
            cmp byte [rdi], sil
            je PrintUntilCharLoopEnd
            cmp byte [rdi], 0x0     ; '\0' char
            je PrintUntilCharLoopEnd
            
            push rdi
            push rdx
            push rsi
            mov  dil, [rdi]
            call PrintToBuffer
            pop rsi
            pop rdx
            pop rdi

            inc rdi
            inc rdx
            jmp PrintUntilCharLoop

PrintUntilCharLoopEnd:
            ret

;------------------------------------------------
; Prints int in binary format
; Entry: (int val)
; Exit : number of chars printed
;------------------------------------------------
PrintBinaryInt:
            push rbp
            mov  rbp, rsp
            
PrintBinaryIntArrSize equ 0x30

            sub rsp,  PrintBinaryIntArrSize     ; saving 32 bytes for number + 0b prefix
            mov rax,  -1                        ; counter for array

            xor r8, r8
            test edi, edi
            jns PrintBinaryIntLoop
            neg edi
            mov r8, NumberIsNegative     ; setting that it was negative
PrintBinaryIntLoop:
            mov rcx, rdi
            and rcx, 0x1    ; saving only least bit
            add rcx, '0'    ; creating ASCII

            mov [rbp + rax], cl    ; pushing least bit in buf
            dec rax
            shr rdi, 1

            cmp rdi, 0x0    ; stop pushing bits in case val is 0
            je PrintBinaryIntLoopEnd
            jmp PrintBinaryIntLoop

PrintBinaryIntLoopEnd:
            sub rsp, 0x8    ; aligning
            push rax        ; saving len


            ;'0b' prefix
            mov byte [rbp + rax], 'b'
            dec rax
            mov byte [rbp + rax], '0'
            dec rax

            cmp r8, NumberIsNegative
            jne PrintBinaryIntToBufferLoop

            mov byte [rbp + rax], '-'   ; pushing '-' char
            dec rax     
            
PrintBinaryIntToBufferLoop:
            inc rax
            cmp rax, 0
            jge PrintBinaryIntToBufferLoopEnd
            
            push rax
            mov  rdi, [rbp + rax]
            call PrintToBuffer
            pop rax

            jmp PrintBinaryIntToBufferLoop

PrintBinaryIntToBufferLoopEnd:
            mov rsp, rbp
            pop rbp
            ret 

;------------------------------------------------
; Prints char
; Entry: (char ch)
;------------------------------------------------
PrintChar:  call PrintToBuffer
            ret

;------------------------------------------------
; Prints int in decimal format
; Entry: (int val)
;------------------------------------------------
PrintDecimalInt:
            push rbp
            mov  rbp, rsp
            
PrintDecimalIntArrSize equ 0x10

            sub rsp,  PrintDecimalIntArrSize    ; saving 10 bytes for number
            mov rax, rdi                        ; saving value in rax for dividing
            mov rdi,  -1                        ; counter for array
            mov r8,  10d                        ; saving for dividing

            xor r9, r9
            test eax, eax
            jns PrintDecimalIntLoop
            neg eax
            mov r9, NumberIsNegative     ; setting that it was negative
PrintDecimalIntLoop:
            xor rdx, rdx
            div r8
            
            add rdx, '0'    ; creating ASCII

            mov [rbp + rdi], dl    ; pushing ASCII
            dec rdi

            cmp rax, 0x0    ; stop pushing bits in case val is 0
            je PrintDecimalIntLoopEnd
            jmp PrintDecimalIntLoop

PrintDecimalIntLoopEnd:
            cmp r9, NumberIsNegative
            jne PrintDecimalIntToBufferLoop
            mov byte [rbp + rdi], '-'
            dec rdi

PrintDecimalIntToBufferLoop:
            inc rdi
            cmp rdi, 0
            jge PrintDecimalIntToBufferLoopEnd
            
            push rdi
            mov  rdi, [rbp + rdi]
            call PrintToBuffer
            pop rdi

            jmp PrintDecimalIntToBufferLoop

PrintDecimalIntToBufferLoopEnd:
            mov rsp, rbp
            pop rbp
            ret 

;------------------------------------------------
; Prints int in octal format
; Entry: (int val)
;------------------------------------------------
PrintOctalInt:
            push rbp
            mov  rbp, rsp
            
PrintOctalIntArrSize equ 0x10

            sub rsp,  PrintOctalIntArrSize      ; saving bytes for val + prefix '0'
            mov rax,  -1                        ; counter for array
            
            xor r8, r8
            test edi, edi
            jns PrintOctalIntLoop
            neg edi
            mov r8, NumberIsNegative     ; setting that it was negative
PrintOctalIntLoop:
            mov rcx, rdi
            and rcx, 0x7    ; saving only 3 least bits
            add rcx, '0'    ; creating ASCII

            mov [rbp + rax], cl    ; pushing least bits in buf
            dec rax
            shr rdi, 3

            cmp rdi, 0x0    ; stop pushing bits in case val is 0
            je PrintOctalIntToBufferLoop
            jmp PrintOctalIntLoop

PrintOctalIntToBufferLoop:
            inc rax
            cmp rax, 0
            jge PrintOctalIntToBufferLoopEnd
            
            push rax
            mov  rdi, [rbp + rax]
            call PrintToBuffer
            pop rax

            jmp PrintOctalIntToBufferLoop

PrintOctalIntToBufferLoopEnd:
            mov rsp, rbp
            pop rbp
            ret 

;------------------------------------------------
; Prints string
; Entry: (char* str)
; Exit : number of chars printed
;------------------------------------------------
PrintString: 
PrintStringLoop:
            cmp byte [rdi], 0x0
            je PrintStringLoopEnd

            push rdi
            mov dil, [rdi]
            call PrintToBuffer
            pop rdi
            inc rdi
            jmp PrintStringLoop
PrintStringLoopEnd:
            ret

;------------------------------------------------
; Prints int in hex format
; Entry: (int val)
;------------------------------------------------
PrintHexInt:
            push rbp
            mov  rbp, rsp
            
PrintHexIntArrSize equ 0x10

            sub rsp,  PrintHexIntArrSize      ; saving bytes for val + prefix '0x'
            mov rax,  -1                        ; counter for array

            xor r8, r8
            test edi, edi
            jns PrintHexIntLoop
            neg edi
            mov r8, NumberIsNegative     ; setting that it was negative
PrintHexIntLoop:
            mov rcx, rdi
            and rcx, 0xf    ; saving only 4 least bits
            
            lea r9, [rel HexASCII]
            add r9, rcx
            mov rcx, [r9]

            mov [rbp + rax], cl    ; pushing least bits in buf
            dec rax
            shr rdi, 4

            cmp rdi, 0x0    ; stop pushing bits in case val is 0
            je PrintHexIntLoopEnd
            jmp PrintHexIntLoop

PrintHexIntLoopEnd:

            ;'0x' prefix
            mov byte [rbp + rax], 'x'
            dec rax
            mov byte [rbp + rax], '0'
            dec rax

            cmp r8, NumberIsNegative
            jne PrintHexIntToBufferLoop

            mov byte [rbp + rax], '-'   ; pushing '-' char
            dec rax     

PrintHexIntToBufferLoop:
            inc rax
            cmp rax, 0
            jge PrintHexIntToBufferLoopEnd
            
            push rax
            mov  rdi, [rbp + rax]
            call PrintToBuffer
            pop rax

            jmp PrintHexIntToBufferLoop

PrintHexIntToBufferLoopEnd:
            mov rsp, rbp
            pop rbp
            ret 
            
;------------------------------------------------
;
;------------------------------------------------
PrintToBuffer:
            mov rax, [rel PrintfBufferSize]
            lea rcx, [rel PrintfBuffer]
            cmp rax, [rel PrintfBufferCapacity]
            jl PrintToBufferPushChar 
            
            push rcx
            push rdi
            ; preparing for syscall write
            mov rdx, rax
            mov rax, 0x1
            mov rdi, 0x1
            mov rsi, rcx
            syscall

            mov qword [rel PrintfBufferSize], 0
            mov rax, 0  

            pop rdi
            pop rcx
PrintToBufferPushChar:
            mov [rcx + rax], dil
            inc qword [rel PrintfBufferSize]

            ret  

;------------------------------------------------

section .data

PrintfBufferSize     dq 0
PrintfBuffer         db 8 dup(0)
PrintfBufferCapacity dq $ - PrintfBuffer

section .rodata
 
HexASCII db "0123456789ABCDEF"
 
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


