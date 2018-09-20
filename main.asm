/*
 * Christopher Mailloux
 * Final Project
 *
 * CDA 3104 - CRN 80584
 * Florida Gulf Coast University
 * Last Modified 12/12/2017
 */

;------------------------------------------------------------------------------
; Project description:
;
; See the project proposal documentation for exact program specifications.
;------------------------------------------------------------------------------
; Hardware usage notes:
;------------------------------------------------------------------------------
; PORT D4 - Red LED
; PORT D5 - Green LED
; PORT R0 - Top Yellow LED
; PORT R1 - Bottom Yellow LED
;
; PORT A3 - LCD Reset Bar
; PORT F3 - LCD CS Bar
; PORT D0 - LCD AO Line
; PORT D1 - LCD XCK
; PORT D3 - LCD TX
; PORT E4 - LCD Backlight
;
; PORT E5 - Button 1
; PORT F1 - Button 2
; PORT F2 - Button 3
; PORT A4 - External Button 1
; PORT A6 - External Button 2
; PORT B0 - External Button 3
; PORT B2 - External Button 4
;------------------------------------------------------------------------------
            .include<ATXMega256A3BUdef.inc>     ;
            .device     ATXMega256A3BU          ;

            .equ        BOT_LED     =0b00000010 ; The top yellow LED pin
            .equ        TOP_LED     =0b00000001 ; The botton yellow LED pin
            .equ        RED_LED     =0b00010000 ; The red LED pin
            .equ        GRN_LED     =0b00100000 ; The green LED pin
            .equ        BOTH_LED    =0b00110000 ; Bitmask for red and green LED
            .equ        LEFT_WNG    =0x00       ; Left winglet selected
            .equ        RIGHT_WNG   =0x01       ; Right winglet selected
            .equ        BOTH_WNG    =0x02       ; Both winglets selected
            .equ        TRUE        =0x00       ;
            .equ        FALSE       =0x01       ;

            .def        tmp         =r16        ;
            .def        tmp2        =r17        ;

            .dseg                               ;
left_qty:   .byte       1                       ;
right_qty:  .byte       1                       ;
sequences:  .byte       1                       ; Num sequnces; 0 = inf
seq_flrs:   .byte       1                       ; Num flares in each sequence
interval:   .byte       1                       ; Interval; see specs for notes
wng_sel:    .byte       1                       ; Which winglet is selected
prog_mode:  .byte       1                       ; Boolean - if programming true
dispensing: .byte       1                       ; Boolean - true if prg running
seq_rem:    .byte       1                       ; Sequences remaining in progrm
intval_per: .byte       2                       ; The PER value for the cur int
intval_srem:.byte       1                       ; Interval seconds remaining
sec_mode:   .byte       1                       ; Whether seq timer is in secs
ib1downcnt: .byte       1                       ; These variables represent the
ib2downcnt: .byte       1                       ; number of times in a row that
ib3downcnt: .byte       1                       ; a button has been recorded as
eb1downcnt: .byte       1                       ; being down; this is used for
eb2downcnt: .byte       1                       ; debouncing the switch inputs.
eb3downcnt: .byte       1                       ;       *
eb4downcnt: .byte       1                       ;       *

            .cseg                               ;
            .org        0x00                    ;
            jmp         start                   ;
            .org        TCC0_OVF_VECT           ; Config int vectors
            jmp         tcc0_isr                ;       *
            .org        TCF0_OVF_VECT           ;       *
            jmp         tcf0_isr                ;       *
            .org        PORTF_INT0_vect         ;       *
            jmp         portf_int0_isr          ;       *
            .org        0xF6                    ;
start:      ldi         tmp,low(RAMEND)         ; Init stack pointer
            ldi         tmp2,high(RAMEND)       ;       *
            sts         CPU_SPL,tmp             ;       *
            sts         CPU_SPH,tmp2            ;       *
            call        set_cpu_clk_32_int      ;
            call        init_lcd                ;
            call        prepare_leds            ;
            call        prepare_buttons         ;
            call        prepare_seq_timer       ;
            ldi         tmp,0b00000111          ; Enable hi,med,lo interrupts
            sts         PMIC_CTRL,tmp           ;       *
            sei                                 ; Enable global interrupts
            call        set_init_flr_qtys       ; Assume fully loaded w/flrs
            call        load_def_program        ; Load the default program
            ldi         tmp,FALSE               ; Load defaults for flags
            sts         dispensing,tmp          ;       *
            sts         prog_mode,tmp           ;       *
            call        update_display          ;
            call        init_key_scanner        ;            
done:       jmp         done                    ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - prepare_buttons
;------------------------------------------------------------------------------
; Prepares the buttons for input. Call only at the beginning of the program.
;------------------------------------------------------------------------------
prepare_buttons:                                ;
            ldi         tmp,0b01010000          ; Set ext. btns 1,2 to in
            sts         PORTA_DIRCLR,tmp        ;       *
            ldi         tmp,0b00000101          ; Set ext. btns 3,4 to in
            sts         PORTB_DIRCLR,tmp        ;       *
            ldi         tmp,0b00100000          ; Set int. btn 1 to in
            sts         PORTE_DIRCLR,tmp        ;       *
            ldi         tmp,0b00000101          ; Set int. btns 2,3 to in
            sts         PORTF_DIRCLR,tmp        ;       *
            ldi         tmp,0b00000110          ; Enable int0 on pins F1,F2
            sts         PORTF_INT0MASK,tmp      ;       *
            ldi         tmp,0b00000011          ; Set int0_portf_to hi pri.
            sts         PORTF_INTCTRL,tmp       ;       *
            ldi         tmp,0b00011010          ; Sense falling edge, pullup
            sts         PORTA_PIN6CTRL,tmp      ;       *
            sts         PORTA_PIN4CTRL,tmp      ;       *
            sts         PORTB_PIN0CTRL,tmp      ;       *
            sts         PORTB_PIN2CTRL,tmp      ;       *
            sts         PORTF_PIN1CTRL,tmp      ;       *
            sts         PORTF_PIN2CTRL,tmp      ;       *
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - prepare_leds
;------------------------------------------------------------------------------
; Initializes the LEDs used by the program.
;------------------------------------------------------------------------------
prepare_leds:                                   ;
            ldi         r16,0b00000011          ; Set all LED pins to out
            sts         PORTR_DIR,r16           ;       *
            ldi         r16,0b00110000          ;       *
            sts         PORTD_DIR,r16           ;       *
            ldi         r16,TOP_LED             ; Turn off LEDs initially
            sts         PORTR_OUTSET,r16        ;       *
            ldi         r16,BOT_LED             ;       *
            sts         PORTR_OUTSET,r16        ;       *
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - init_key_scanner
;------------------------------------------------------------------------------
; Prepares the timer used for the key scanning and debouncing.
;------------------------------------------------------------------------------
init_key_scanner:                               ;
            ldi         r16,0                   ; Set TCF0 to normal mode
            sts         TCF0_CTRLB,r16          ;       *
            ldi         r16,0x06                ; Set TCF0 prescale to div 256
            sts         TCF0_CTRLA,r16          ;       *
            ldi         r16,0x02                ; Enable med. timer ints
            sts         TCF0_INTCTRLA,r16       ;       *
            clr         r16                     ; Let cnt go up from 0
            sts         TCF0_CNT,r16            ;       *
            sts         TCF0_CNT+1,r16          ;       *
            ldi         r16,low(12500)          ; Set PER to 100ms
            ldi         r17,high(12500)         ;       *
            sts         TCF0_PER,r16            ;       *
            sts         TCF0_PER+1,r17          ;       *
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - prepare_seq_timer
;------------------------------------------------------------------------------
; Configures TCC0 for usage as the timer that coordinates the interval between
; the execution of each sequence of the countermeasures program.
;
; Does not set the period or enable the interrupts. To use the timer, the user
; must do both of these things, as well as initialize the count to 0.
;------------------------------------------------------------------------------
prepare_seq_timer:                              ;
            ldi         r16,0                   ; Set TCC0 to normal mode
            sts         TCC0_CTRLB,r16          ;       *
            ldi         r16,0x07                ; Set clksel to clk_per/1024
            sts         TCC0_CTRLA,r16          ;       *
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - start_seq_timer
;------------------------------------------------------------------------------
; Starts the timer that controls the interval between countermeasures
; sequences. Assumes that calc_interval has already been called to determine
; the running period before the start of the countermeasures program.
;
; This routine saves all registers it uses so it is safe to use in an ISR.
;------------------------------------------------------------------------------
start_seq_timer:                                ;
            push        r16                     ; Save registers
            push        r17                     ;       *
            ldi         r16,0x03                ; Enable hi level tcc0 int
            sts         TCC0_INTCTRLA,r16       ;       *
            clr         r16                     ; Let count go up from 0
            sts         TCC0_CNT,r16            ;       *
            sts         TCC0_CNT+1,r16          ;       *
            lds         r16,intval_per          ; Set period
            lds         r17,intval_per+1        ;       *
            sts         TCC0_PER,r16            ;       *
            sts         TCC0_PER+1,r16          ;       *
            pop         r17                     ; Restore registers
            pop         r16                     ;       *
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - stop_seq_timer
;------------------------------------------------------------------------------
; Stops the timer that controls the interval between countermeasures sequences.
;
; This routine saves all registers that it uses, so it is safe in an ISR.
;------------------------------------------------------------------------------
stop_seq_timer:                                 ;
            push        r16                     ; Save registers
            ldi         r16,0x00                ; Disable tcc0 ovf interrupts
            sts         TCC0_INTCTRLA,r16       ;       *
            pop         r16                     ; Restore registers
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Timer interrupt service routine - tcf0_isr
;------------------------------------------------------------------------------
; Scans the buttons used in the program and handles their debouncing.
;
; Once each button has been debounced, it's appropriate handler routine is
; called.
;------------------------------------------------------------------------------
.equ        DEBOUNCE_VALUE   =0xFF              ;

tcf0_isr:                                       ;
            push        r16                     ; Save registers
            push        r17                     ;       *
check_ib1:                                      ;
            lds         r16,PORTE_IN            ;
            sbrs        r16,5                   ; if(ib1 is down)
            rjmp        ib1_is_down             ;   debounce ib1
            clr         r16                     ; else reset debounce counter
            sts         ib1downcnt,r16          ;       *
            jmp         check_eb1               ;       *
ib1_is_down:                                    ; Debounce ib1
            lds         r16,ib1downcnt          ;       *
            inc         r16                     ;       *
            sts         ib1downcnt,r16          ;       *
            cpi         r16,DEBOUNCE_VALUE      ;       *
            breq        ib1_debounced           ;       *
ib1_debounced:                                  ;       *
            call        sel_wng_pressed         ; Call ib1 service routine
check_eb1:                                      ; Repeat for eb1
            lds         r16,PORTA_IN            ;       *
            sbrs        r16,4                   ;       *
            rjmp        eb1_is_down             ;       *
            clr         r16                     ;       *
            sts         eb1downcnt,r16          ;       *
            jmp         check_eb2               ;       *
eb1_is_down:                                    ;       *
            lds         r16,eb1downcnt          ;       *
            inc         r16                     ;       *
            sts         eb1downcnt,r16          ;       *
            cpi         r16,DEBOUNCE_VALUE      ;       *
            breq        eb1_debounced           ;       *
eb1_debounced:                                  ;       *
            call        inc_prog_dig_1          ;       *
check_eb2:                                      ; Repeat for eb2
            lds         r16,PORTA_IN            ;       *
            sbrs        r16,6                   ;       *
            rjmp        eb2_is_down             ;       *
            clr         r16                     ;       *
            sts         eb2downcnt,r16          ;       *
            jmp         check_eb3               ;       *
eb2_is_down:                                    ;       *
            lds         r16,eb2downcnt          ;       *
            inc         r16                     ;       *
            sts         eb2downcnt,r16          ;       *
            cpi         r16,DEBOUNCE_VALUE      ;       *
            breq        eb2_debounced           ;       *
eb2_debounced:                                  ;       *
            call        inc_prog_dig_2          ;       *
check_eb3:                                      ; Repeat for eb3
            lds         r16,PORTB_IN            ;       *
            sbrs        r16,0                   ;       *
            rjmp        eb3_is_down             ;       *
            clr         r16                     ;       *
            sts         eb3downcnt,r16          ;       *
            jmp         check_eb4               ;       *
eb3_is_down:                                    ;       *
            lds         r16,eb3downcnt          ;       *
            inc         r16                     ;       *
            sts         eb3downcnt,r16          ;       *
            cpi         r16,DEBOUNCE_VALUE      ;       *
            breq        eb3_debounced           ;       *
eb3_debounced:                                  ;       *
            call        inc_prog_dig_3          ;       *
check_eb4:                                      ; Repeat for eb4
            lds         r16,PORTB_IN            ;       *
            sbrs        r16,2                   ;       *
            rjmp        eb4_is_down             ;       *
            clr         r16                     ;       *
            sts         eb4downcnt,r16          ;       *
            jmp         end_btn_scan            ;       *
eb4_is_down:                                    ;       *
            lds         r16,eb4downcnt          ;       *
            inc         r16                     ;       *
            sts         eb4downcnt,r16          ;       *
            cpi         r16,DEBOUNCE_VALUE      ;       *
            breq        eb4_debounced           ;       *
eb4_debounced:                                  ;       *
            call        tgl_prog_mode           ;       *
end_btn_scan:                                   ;
            pop         r17                     ; Restore registers
            pop         r16                     ;       *
            reti                                ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Timer interrupt service routine -tcc0_isr
;------------------------------------------------------------------------------
; This timer is used to handle the interval between dispensing sequences.
;
; Because long delays are needed in some cases, this delay uses both the PER
; registers of the timer and an additional counter for counting the number
; of seconds remaining to delay for.
;------------------------------------------------------------------------------
tcc0_isr:                                       ;
            push        r16                     ; Save registers
            lds         r16,dispensing          ; Check if we should continue
            cpi         r16,TRUE                ; dispensing.
            brne        program_over            ; If not, finish up the routine
            lds         r16,sec_mode            ; Check if delaying for seconds
            cpi         r16,TRUE                ;       *
            brne        tcc0_isr_dispense       ;       *
            lds         r16,intval_srem         ; Decrement seconds remaining
            dec         r16                     ;       *
            sts         intval_srem,r16         ;       *
            cpi         r16,0                   ; If no seconds remaining, jmp
            breq        tcc0_isr_dispense       ;       *
            jmp         return_tcc0_isr         ; and end the ISR.
tcc0_isr_dispense:                              ;
            lds         r16,seq_rem             ; Check how many sequences left
            cpi         r16,0                   ;       *
            breq        program_over            ; If none left, end cms program
			call		stop_seq_timer			; Timer stop during sequences
            push        r16                     ; Else execute next sequence
            call        execute_sequence        ;       *
            pop         r16                     ;       *
            dec         r16                     ; Decrement sequences remaining
            sts         seq_rem,r16             ;       *
            cpi         r16,0                   ; If that was last seq, end prg
            breq        program_over            ;       *
            call        calc_interval           ; Else, start the timer for the
            call        start_seq_timer         ; next sequence to run
            jmp         return_tcc0_isr         ;       *
program_over:                                   ;
            ldi         r16,FALSE               ; Disable dispensing flag
            sts         dispensing,r16          ;       *
            call        stop_seq_timer          ; Stop the timer interrupts
return_tcc0_isr:                                ;
            pop         r16                     ; Restore registers
            reti                                ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Interrupt service routine - portf_int0_isr
;------------------------------------------------------------------------------
; Handles int0 on portf. This interrupt is currently associated with input from
; buttons 2 and 3 on the board.
;------------------------------------------------------------------------------
portf_int0_isr:                                 ;
            push        r16                     ; Save registers
            push        r17                     ;       *
            lds         r16,PORTF_IN            ; Read port values
            sbrs        r16,1                   ; Check if btn 2
            breq        btn_2_down              ;		*
            sbrs        r16,2                   ; Check if btn 3
            breq        btn_3_down              ;		*
            jmp         end_portf_int0_isr      ;
btn_2_down:                                     ;
            call        execute_program         ;
            jmp         end_portf_int0_isr      ;
btn_3_down:                                     ;
            call        stop_program            ;
end_portf_int0_isr:                             ;
            pop         r17                     ; Restore registers
            pop         r16                     ;       *
            reti                                ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - inc_prog_dig_1
;------------------------------------------------------------------------------
; Increments the first digit of the countermeasures program (the number of
; sequences of flares to dispense) by 1, wrapping around to 0 as necessary.
;------------------------------------------------------------------------------
inc_prog_dig_1:                                 ;
            lds         r16,sequences           ;
            call        inc_single_digit        ;
            sts         sequences,r16           ;
            call        update_display          ;
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - inc_prog_dig_2
;------------------------------------------------------------------------------
; Increments the second digit of the countermeasures program (the number of
; flares to dispense per sequence) by 1, wrapping around to 0 as necessary.
;------------------------------------------------------------------------------
inc_prog_dig_2:                                 ;
            lds         r16,seq_flrs            ;
            call        inc_single_digit        ;
            sts         seq_flrs,r16            ;
            call        update_display          ;
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - inc_prog_dig_3
;------------------------------------------------------------------------------
; Increments the third digit of the countermeasures program (the dispensing
; interval) by 1, wrapping around to 0 as necessary.
;------------------------------------------------------------------------------
inc_prog_dig_3:                                 ;
            lds         r16,interval            ;
            call        inc_single_digit        ;
            sts         interval,r16            ;
            call        update_display          ;
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - inc_single_digit
;------------------------------------------------------------------------------
; Increments a single digit provided in r16 by 1 and returns the results also
; in r16.
;
; This subroutine is useful for incrementing digits displayed in a physical
; display such as a 7-segment display, for example.
;
; If the value overflows a single digit (that is, 9 is passed in), it cycles
; back to 0.
;------------------------------------------------------------------------------
inc_single_digit:                               ;
            inc         r16                     ;
            cpi         r16,10                  ;
            breq        wrap_around_to_0        ;
            jmp         end_inc_single_digit    ;
wrap_around_to_0:                               ;
            clr         r16                     ;
end_inc_single_digit:                           ;
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - tgl_prog_mode
;------------------------------------------------------------------------------
; Called whenever the programming mode button has been pressed.
;
; Toggles the state of programming mode.
;
; If programming mode is off, the remaining quantity of flares for the
; selected winglets is displayed on the screen instead.
;------------------------------------------------------------------------------
tgl_prog_mode:                                  ;
            lds         r16,prog_mode           ;
            cpi         r16,TRUE                ;
            brne        t_prog_mode_when_false  ;
t_prog_mode_when_true:                          ;
            ldi         r16,FALSE               ;
            jmp         end_tgl_prog_mode       ;
t_prog_mode_when_false:                         ;
            ldi         r16,TRUE                ;
end_tgl_prog_mode:                              ;
            sts         prog_mode,r16           ;
            call        update_display          ;
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - execute_program
;------------------------------------------------------------------------------
; Begins execution of the current countermeasures program.
;
; If a program is aready running, executing this routine again does nothing.
;------------------------------------------------------------------------------
execute_program:                                ;
            push        r16                     ; Save registers
            lds         r16,dispensing          ; If(dispensing already)
            cpi         r16,TRUE                ;    do nothing
            breq        end_execute_program     ;       *
            ldi         r16,TRUE                ; Else set dispensing true
            sts         dispensing,r16          ;       *
            lds         r16,sequences           ;
            sts         seq_rem,r16             ;
            push        r16                     ; Start the first sequence
            call        execute_sequence        ;		*
            pop         r16						;		*
            lds         r16,seq_rem             ;
            dec         r16                     ;
            sts         seq_rem,r16             ;
            call        calc_interval           ; Start the timer until nxt seq
            call        start_seq_timer         ;
end_execute_program:                            ;
            pop         r16                     ; Restore registers
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - calc_interval
;------------------------------------------------------------------------------
; Sets values that configure the interval to wait between the firing of each
; countermeasures sequence, by filling global variables for timer values.
;
; There are 3 special cases handled, for the values of 0, 7, and 9. These cases
; are described in more detail in the project proposal and requirements docs.
;
; Because the timer can't wait for a large number of seconds, in cases where
; interval is 1 or more seconds, the timer period is set to one second, and
; a multiplier value is set that will be decremented by each timer interrupt.
;
; So, the timer interrupt routine should check to see the value of the period,
; and if this value is equal to 31,250 (1 second), it should check the value
; of the multiplier and decrement it and continue for another iteration.
;
; Assumes the timer clock runs at 32MHz / 1024
;------------------------------------------------------------------------------
calc_interval:                                  ;
            push        r17                     ; Save registers
            push        r16                     ;       *
            ldi         r16,TRUE                ; Set second mode off
            sts         sec_mode                ;       *
            lds         r16,interval            ;
            mov         r17,r16                 ; Keep a copy to prevent probs
            cpi         r16,0                   ;
            breq        calc_int_spec_case_0    ;
            cpi         r16,7                   ;
            breq        calc_int_spec_case_7    ;
            cpi         r16,9                   ;
            breq        calc_int_spec_case_9    ;
            jmp         calc_int_general_case   ;
calc_int_spec_case_0:                           ;
            ldi         r16,low(3906)           ; 0.125 seconds
            sts         intval_per,r16          ;       *
            ldi         r16,high(3906)          ;       *
            sts         intval_per+1,r16        ;       *
            rjmp        end_calc_interval       ;
calc_int_spec_case_7:                           ;
            ldi         r16,low(7812)           ; 0.25 seconds
            sts         intval_per,r16          ;       *
            ldi         r16,high(7812)          ;       *
            sts         intval_per+1,r16        ;       *
            rjmp        end_calc_interval       ;
calc_int_spec_case_9:                           ;
            ldi         r16,low(15625)          ; 0.5 seconds
            sts         intval_per,r16          ;       *
            ldi         r16,high(15625)         ;       *
            sts         intval_per+1,r16        ;       *
            rjmp        end_calc_interval       ;
calc_int_general_case:                          ;
            ldi         r16,low(31250)          ; 1 second * multiplier n
            sts         intval_per,r16          ;       *
            ldi         r16,high(31250)         ;       *
            sts         intval_per+1,r16        ;       *
            ldi         r16,TRUE                ; Set seconds mode on
            sts         sec_mode                ;       *
            sts         intval_srem,r17         ; Seconds remaining to wait
end_calc_interval:                              ;
            pop         r16                     ; Restore registers
            pop         r17                     ;       *
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - execute_sequence
;------------------------------------------------------------------------------
; Executes a single sequence from the current countermeasures program.
;------------------------------------------------------------------------------
execute_sequence:                               ;
            lds         r16,seq_flrs            ;
dispense_flr_in_seq:                            ;
            cpi         r16,0                   ; Dispense the required num of
            breq        done_exec_sequence      ; flares for the programmed
            call        disp_flares             ; sequence
            call        del_50_ms               ;		*
            dec         r16                     ;		*
            rjmp        dispense_flr_in_seq     ;
done_exec_sequence:                             ;
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - stop_program
;------------------------------------------------------------------------------
; Sets the flag that indicates whether a countermeasures program is currently
; running to false. The program has the duty to check the flag's value
; regularly during operation, and stop as appropriate.
;------------------------------------------------------------------------------
stop_program:                                   ;
            ldi         r16,FALSE               ;
            sts         dispensing,r16          ;
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - sel_wng_pressed
;------------------------------------------------------------------------------
; Selects the next winglet (LEFT/RIGHT/BOTH) for dispensing, and updates the
; wing selection LED.
;------------------------------------------------------------------------------
sel_wng_pressed:                                ;
            lds         r16,wng_sel             ; Determine current selection
            inc         r16                     ; Go to next mode
            cpi         r16,0x03                ;       *
            breq        cycle_through           ;       *
            jmp         end_sel_wng_pressed     ;       *
cycle_through:                                  ;       *
            ldi         r16,LEFT_WNG            ;       *
end_sel_wng_pressed:                            ;
            sts         wng_sel,r16             ; Store updated value
            call        update_wng_sel_led      ;
            call        update_display          ;
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - update_wng_sel_led
;------------------------------------------------------------------------------
; Reads the currently selected winglet and increments to the next value.
;
; The order followed is LEFT/RIGHT/BOTH.
;------------------------------------------------------------------------------
update_wng_sel_led:                             ;
            push        r16                     ;
            ldi         r16,RED_LED             ; Turn both LEDs off
            sts         PORTD_OUTSET,r16        ;       *
            ldi         r16,GRN_LED             ;       *
            sts         PORTD_OUTCLR,r16        ;       *
            lds         r16,wng_sel             ; Check selected winglet
            cpi         r16,LEFT_WNG            ;
            breq        red_led_on              ; Red is traditionally left
            cpi         r16,RIGHT_WNG           ;
            breq        grn_led_on              ; Green is traditionally right
            cpi         r16,BOTH_WNG            ; Both gives orange
            breq        both_leds_on            ;
red_led_on:                                     ;
            ldi         r16,RED_LED             ; Turn on red LED
            sts         PORTD_OUTCLR,r16        ;       *
            jmp         end_update_wng_sel_led  ;
grn_led_on:                                     ;
            ldi         r16,GRN_LED             ; Turn on green LED
            sts         PORTD_OUTSET,r16        ;       *
            jmp         end_update_wng_sel_led  ;
both_leds_on:                                   ;
            ldi         r16,GRN_LED             ; Turn on both LEDs
            sts         PORTD_OUTSET,r16        ;       *
            ldi         r16,RED_LED             ;       *
            sts         PORTD_OUTCLR,r16        ;       *
end_update_wng_sel_led:                         ;
            pop         r16                     ;
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - disp_flares
;------------------------------------------------------------------------------
; Dispenses flares according to the current winglet selection.
;------------------------------------------------------------------------------
disp_flares:                                    ;
            push        r16                     ; Save registers
            lds         r16,wng_sel             ; Check the current selection
            cpi         r16,LEFT_WNG            ;       *
            breq        disp_left_flare         ;       *
            cpi         r16,RIGHT_WNG           ;       *
            breq        disp_right_flare        ;       *
            jmp         disp_both_flares        ;       *
disp_left_flare:                                ;
            call        fire_left_flare         ;
            jmp         end_disp_flares         ;
disp_right_flare:                               ;
            call        fire_right_flare        ;
            jmp         end_disp_flares         ;
disp_both_flares:                               ;
            call        fire_both_flares        ;
end_disp_flares:                                ;
            call        update_display          ;
            pop         r16                     ; Restore registers
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - update_display
;------------------------------------------------------------------------------
; Refreshes the display with current data from the program.
;
; If in programming mode, updates the program currently displayed.
; If not in programming mode, updates the current qty displayed.
;------------------------------------------------------------------------------
update_display:                                 ;
            lds         r16,prog_mode           ;
            cpi         r16,TRUE                ;
            brne        update_display_else     ;
            call        display_cur_program     ;
            jmp         end_update_display      ;
update_display_else:                            ;
            call        display_rem_qty         ;
end_update_display:                             ;
            ret                                 ;
;-----------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - fire_left_flare
;------------------------------------------------------------------------------
; Fires a flare from the left winglet, if any remaining.
;------------------------------------------------------------------------------
fire_left_flare:                                ;
            lds         r16,left_qty            ; Check if any remaining
            cpi         r16,0                   ;       *
            breq        left_flares_empty       ;       *
            dec         r16                     ; Decrement qty
            sts         left_qty,r16            ;       *
            ldi         r16,TOP_LED             ; Turn on top yellow LED
            sts         PORTR_OUTCLR,r16        ;       *
            call        del_50_ms               ; Delay 0.05 s
            ldi         r16,TOP_LED             ; Turn off top yellow LED
            sts         PORTR_OUTSET,r16        ;       *
left_flares_empty:                              ;
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - fire_right_flare
;------------------------------------------------------------------------------
; Fires a flare from the right winglet, if any remaining.
;------------------------------------------------------------------------------
fire_right_flare:                               ;
            lds         r16,right_qty           ; Check if any remaining
            cpi         r16,0                   ;       *
            breq        right_flares_empty      ;       *
            dec         r16                     ; Decrement right qty
            sts         right_qty,r16           ;       *
            ldi         r16,BOT_LED             ; Turn on bottom yellow LED
            sts         PORTR_OUTCLR,r16        ;       *
            call        del_50_ms               ; Delay 0.05 s
            ldi         r16,BOT_LED             ; Turn off bottom yellow LED
            sts         PORTR_OUTSET,r16        ;       *
right_flares_empty:                             ;
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - fire_both_flares
;------------------------------------------------------------------------------
; Fires a flare from both winglets, if any remaining.
;
; If one winglet is out of flares, that other will still fire.
;------------------------------------------------------------------------------
fire_both_flares:                               ;
            lds         r16,left_qty            ; Check left quantity
            cpi         r16,0                   ;       *
            breq        check_right             ;
            dec         r16                     ; Decrement left qty
            sts         left_qty,r16            ;       *
            ldi         r16,TOP_LED             ; Turn on top yellow LED
            sts         PORTR_OUTCLR,r16        ;       *
            jmp         check_right             ;
check_right:                                    ;
            lds         r16,right_qty           ; Check right quantity
            cpi         r16,0                   ;       *
            breq        end_fire_both_flares    ;
            dec         r16                     ; Decrement right qty
            sts         right_qty,r16           ;       *
            ldi         r16,BOT_LED             ; Turn on bottom yellow LED
            sts         PORTR_OUTCLR,r16        ;       *
            jmp         end_fire_both_flares    ;
end_fire_both_flares:                           ;
            call        del_50_ms               ; Delay 0.05 s
            ldi         r16,TOP_LED             ; Turn off both yellow LEDs
            ori         r16,BOT_LED             ;       *
            sts         PORTR_OUTSET,r16        ;       *
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - display_rem_qty
;------------------------------------------------------------------------------
; Displays the remaining quantity of flares on the LCD, depending on which
; winglet is currently selected.
;------------------------------------------------------------------------------
display_rem_qty:                                ;
            push        r0                      ; Save registers
            push        r1                      ;       *
			push		r16						;		*
			push		r17						;		*
            push        r18                     ;       *
            push        r19                     ;       *
            push        r20                     ;       *
            push        ZL                      ;       *
            push        ZH                      ;       *
            call        lcd_clear_screen        ; Clear the display
            call        lcd_set_cursor_tleft    ; Position the cursor
            ldi         ZL,low(s_quantity << 1) ; Display "Remaining"
            ldi         ZH,high(s_quantity << 1); on line 1
            call        lcd_printf              ;       *
            ldi         r16,0                   ;       *
            ldi         r17,1                   ;       *
            mov         r0,r16                  ;       *
            mov         r1,r17                  ;       *
            call        lcd_set_cursor          ; Move cursor to second row
            lds         r16,wng_sel             ; Determine selected winglet
            cpi         r16,LEFT_WNG            ;       *
            breq        disp_left_qty           ; Jump left winglet logic
            cpi         r16,RIGHT_WNG           ; Jump to right winglet logic
            breq        disp_right_qty          ;
            jmp         disp_total_qty          ; Jump to logic for both wngs
disp_left_qty:                                  ;
            lds         r16,left_qty            ; Display left qty remaining
            jmp         end_display_rem_qty     ;
disp_right_qty:                                 ; Display right qty remaining
            lds         r16,right_qty           ;
            jmp         end_display_rem_qty     ;
disp_total_qty:                                 ; Display total qty remaining
            lds         r16,left_qty            ; Sum the quantities
            lds         r17,right_qty           ;       *
            add         r16,r17                 ;       *
            jmp         end_display_rem_qty     ;
end_display_rem_qty:                            ;
            clr         r17                     ; Clear r17 just in case
            call        binbcd                  ;
            mov         r20,r16                 ; Save r16 in temp register
            mov         r16,r18                 ; Print first digit
            call        lcd_write_ascii_char    ;       *
            mov         r16,r17                 ; Print second digit
            call        lcd_write_ascii_char    ;       *
            mov         r16,r20                 ; Print third digit
            call        lcd_write_ascii_char    ;       *
            pop         ZH                      ; Restore registers
            pop         ZL                      ;       *
            pop         r20                     ;       *
            pop         r19                     ;       *
            pop         r18                     ;       *
			pop			r17						;		*
			pop			r16						;		*
            pop         r1                      ;       *
            pop         r0                      ;       *
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - display_cur_program
;------------------------------------------------------------------------------
; Displays the current countermeasures program on the LCD.
;------------------------------------------------------------------------------
display_cur_program:                            ;
            push        r0                      ; Save registers
            push        r1                      ;       *
            push        ZL                      ;       *
            push        ZH                      ;       *
            call        lcd_clear_screen        ; Clear the screen
            call        lcd_set_cursor_tleft    ; Position the cursor
            ldi         ZL,low(s_program << 1)  ; Display "program"
            ldi         ZH,high(s_program << 1) ; on line 1
            call        lcd_printf              ;       *
            ldi         r16,0                   ;       *
            ldi         r17,1                   ;       *
            mov         r0,r16                  ;       *
            mov         r1,r17                  ;       *
            call        lcd_set_cursor          ; Then on line 2:
            lds         r16,sequences           ; Display program digit 1
            call        lcd_write_dec_digit     ;       *
            lds         r16,seq_flrs            ; Display program digit 2
            call        lcd_write_dec_digit     ;       *
            lds         r16,interval            ; Display program digit 3
            call        lcd_write_dec_digit     ;       *
            pop         ZH                      ; Restore registers
            pop         ZL                      ;       *
            pop         r1                      ;       *
            pop         r0                      ;       *
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - set_init_flr_qtys
;------------------------------------------------------------------------------
; Initializes the flare quantities to their initial amount, as prescribed in
; the specifications.
;------------------------------------------------------------------------------
set_init_flr_qtys:
            ldi         r16,64                  ; Initialze the flare counts
            sts         left_qty,r16            ;       *
            sts         right_qty,r16           ;       *
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - load_def_program
;------------------------------------------------------------------------------
; Loads the default program of 0,0,0.
;------------------------------------------------------------------------------
load_def_program:
            clr         r16                     ;
            sts         sequences,r16           ; Set all three prog digs to 0
            sts         seq_flrs,r16            ;       *
            sts         interval,r16            ;       *
            ldi         r16,BOTH_WNG            ; Default to both wngs selected
            sts         wng_sel,r16             ;       *
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - del_50_ms
;------------------------------------------------------------------------------
; Generated by delay loop calculator at
; http://www.bretmulvey.com/avrdelay.html
;
; Delay 1,600,000 cycles
; 50ms at 32 MHz
;------------------------------------------------------------------------------
del_50_ms:                                      ;
            push        r18                     ; Save registers
            push        r19                     ;       *
            push        r20                     ;       *
            push        r16                     ;       *
            ldi         r18,9                   ;
            ldi         r19,30                  ;
            ldi         r20,223                 ;
d50L1:      dec         r20                     ;
            brne        d50L1                   ;
            dec         r19                     ;
            brne        d50L1                   ;
            dec         r18                     ;
            brne        d50L1                   ;
            pop         r16                     ; Restore registers
            pop         r20                     ;       *
            pop         r19                     ;       *
            pop         r18                     ;       *
            nop                                 ;
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - init_lcd
;------------------------------------------------------------------------------
; This subroutine must be called before the LCD can be used.
;------------------------------------------------------------------------------
init_lcd:                                       ;
            call        set_cpu_clk_32_int      ;
            call        lcd_setup_usart_spi_pins;
            call        lcd_setup_spi_on_uartd0 ;
            call        lcd_reset               ;
            ldi         r16,0xA0                ; Cmd = A0 (adc normal)
            call        lcd_write_cmd           ;       *
            ldi         r16,0xA6                ; Cmd = A6 (display norm mode)
            call        lcd_write_cmd           ;       *
            ldi         r16,0xC8                ; Cmd = C8 (reverse scan)
            call        lcd_write_cmd           ;       *
            ldi         r16,0xA2                ; Cmd = A2 (lcd bias)
            call        lcd_write_cmd           ;       *
            call        wlittle                 ; Need a small delay here
            ldi         r16,0x2F                ; Cmd = 2F (power ctrl)
            call        lcd_write_cmd           ;       *
            ldi         r16,0xF8                ; Cmd = F8 (set booster ratio)
            call        lcd_write_cmd           ;       *
            ldi         r16,0x00                ; Cmd = 00 (boostr ratio 2x-4x)
            call        lcd_write_cmd           ;       *
            ldi         r16,0x21                ; Cmd = 21 (register ratio)
            call        lcd_write_cmd           ;       *
            ldi         r16,0x1F                ; Cmd = 1F (set contrast)
            call        lcd_write_cmd           ;       *
            ldi         r16,0xAF                ; Cmd = AF (LCD on)
            call        lcd_write_cmd           ;       *
            call        lcd_clear_screen        ;
            call        lcd_set_cursor_tleft    ;
            call        lcd_backlight_on        ;
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - lcd_set_cursor
;------------------------------------------------------------------------------
; Positions the cursor in the LCD.
;
; Input:
; r0 = column        (0 ... 131)
; r1 = row (page)    (0 ... 3)
;
; Each page is a block of 8 rows.
; Each letter is an 8 x 6 matrix with the image in the top left 7x5 corner.
; Leave the bottom row and the rightmost column blank for spacing.
;------------------------------------------------------------------------------
lcd_set_cursor:                                 ;
            mov         r16,r0                  ; Set the MSB of the col addr
            andi        r16,0xF0                ; Code is 0001xxxx
            swap        r16                     ;       *
            ori         r16,0x10                ;       *
            call        lcd_write_cmd           ;       *
            mov         r16,r0                  ; Set the LSB of the col addr
            andi        r16,0x0F                ; Code is 0000xxxx
            rcall       lcd_write_cmd           ;       *
            mov         r16,r1                  ; Set the row (page)
            andi        r16,0x0F                ; Code is 1011xxxx
            ori         r16,0xB0                ;       *
            rcall       lcd_write_cmd           ;       *
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - lcd_set_cursor_tleft
;------------------------------------------------------------------------------
; Sets the cursor position of the LCD to 0,0, which is the top left of the
; display screen.
;------------------------------------------------------------------------------
lcd_set_cursor_tleft:                           ;
            push        r0                      ; Save registers
            push        r1                      ;       *
            ldi         r16,0                   ; Position cursor in top left
            mov         r0,r16                  ;       *
            mov         r1,r16                  ;       *
            call        lcd_set_cursor          ;       *
            pop         r1                      ; Restore registers
            pop         r0                      ;       *
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - lcd_write_data
;------------------------------------------------------------------------------
; Writes the next byte in r16 to the current col. of the current row of the
; LCD. The next column is incremented automatically.
;
; Makes AO line high, then sends the byte to the LCD.
;
; Input: r16 = byte with bit pattern to display.
;------------------------------------------------------------------------------
lcd_write_data:                                 ;
            push        r17                     ; Save registers
            ldi         r17,0b00000001          ; LCD_AO high (D0 <- 1)
            sts         PORTD_OUTSET,r17        ;       *
            call        lcd_send_byte           ; Send the byte
            pop         r17                     ; Restore registers
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - lcd_draw_6_col_fig
;------------------------------------------------------------------------------
; Draws a figure that has 6 columns at the location of the cursor.
;
; Z = Address of the first of the 6 bytes to draw.
;------------------------------------------------------------------------------
lcd_draw_6_col_fig:                             ;
            push        r16                     ; Save registers
            lpm         r16,Z+                  ; Write (LCDData[Z++])
            rcall       lcd_write_data          ;       *
            lpm         r16,Z+                  ; Repeat for all 6 columns
            rcall       lcd_write_data          ;       *
            lpm         r16,Z+                  ;       *
            rcall       lcd_write_data          ;       *
            lpm         r16,Z+                  ;       *
            rcall       lcd_write_data          ;       *
            lpm         r16,Z+                  ;       *
            rcall       lcd_write_data          ;       *
            lpm         r16,Z+                  ;       *
            rcall       lcd_write_data          ;       *
            pop         r16                     ; Restore registers
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - lcd_clear_screen
;------------------------------------------------------------------------------
; Clears the whole LCD screen.
;
; Traverses each block of the screen writing a 00 bit pattern.
;------------------------------------------------------------------------------
lcd_clear_screen:                               ;
            push        r16                     ; Save registers
            push        r17                     ;       *
            push        r18                     ;       *
            push        r0                      ;       *
            push        r1                      ;       *
            clr         r0                      ; Set column
            ldi         r16,3                   ;       *
            mov         r1,r16                  ; Set row (page)
lcd_while2:                                     ;
            call        lcd_set_cursor          ;
            clr         r16                     ;
            ldi         r17,132                 ;
lcd_while3:                                     ;
            call        lcd_write_data          ;
            dec         r17                     ;
            brne        lcd_while3              ;
            dec         r1                      ;
            brge        lcd_while2              ;
            pop         r1                      ; Restore registers
            pop         r0                      ;       *
            pop         r18                     ;       *
            pop         r17                     ;       *
            pop         r16                     ;       *
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - lcd_backlight_on
;------------------------------------------------------------------------------
; Turns on the backlight to the LCD screen.
;------------------------------------------------------------------------------
lcd_backlight_on:                               ;
            push        r16                     ; Save registers
            ldi         r16,0b00010000          ; E4 <- 1 (LCD backlight on)
            sts         PORTE_OUTSET,r16        ;
            pop         r16                     ; Restore registers
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - lcd_write_cmd
;------------------------------------------------------------------------------
; Writes the byte in r16 to the LCD as a command.
;
; Makes AO line low then sends the byte to the LCD.
;
; Input: r16 = the byte command.
;------------------------------------------------------------------------------
lcd_write_cmd:                                  ;
            push        r17                     ; Save registers
            ldi         r17,0b00000001          ; LCD_AO low (DO <- 0)
            sts         PORTD_OUTCLR,r17        ;       *
            call        lcd_send_byte           ; Send the byte
            pop         r17                     ; Restore registers
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - lcd_send_byte
;------------------------------------------------------------------------------
; Sends the byte in r16 to the LCD.
;------------------------------------------------------------------------------
lcd_send_byte:                                  ;
            push        r17                     ; Save registers
            ldi         r17,0b00001000          ; Make CS low (F3 <- 0)
            sts         PORTF_OUTCLR,r17        ;       *
wcmd1:                                          ;
            lds         r17,USARTD0_STATUS      ; Loop till the data buf is clr
            sbrs        r17,5                   ;
            rjmp        wcmd1                   ;
            sts         USARTD0_DATA,r16        ; Send the byte to the LCD
wcmd2:                                          ;
            lds         r17,USARTD0_STATUS      ; Loop until transmit is done
            sbrs        r17,6                   ;       *
            rjmp        wcmd2                   ;       *
            cbr         r17,6                   ;       *
            sts         USARTD0_STATUS,r17      ; CLEAR TRANSMIT COMPLETE
            pop         r17                     ; Restore registers
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - wlittle
;------------------------------------------------------------------------------
; Waits a little bit; this is a short delay used in RESET of the ST7565r.
;
; For LCD5 added an outer loop to increase the delay by a factor of 4.
;------------------------------------------------------------------------------
wlittle:                                        ;
            push        r17                     ; Save registers
            push        r18                     ;       *
            ldi         r18,4                   ;
agab:                                           ;
            ldi         r17,85                  ;
agaa:                                           ;
            nop                                 ;
            nop                                 ;
            nop                                 ;
            dec         r17                     ;
            brne        agaa                    ;
            dec         r18                     ;
            brne        agab                    ;
            pop         r18                     ; Restore registers
            pop         r17                     ;       *
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - lcd_setup_usart_spi_pins
;------------------------------------------------------------------------------
; Setup the pins used for the SPI on the USART D0.
;
; A3 = Reset/
; F3 = CS/
; D0 = AO of the LCD
; D1 = XCK
; D3 = TX
; E4 = Backlight (1 = on, 0 = off)
;------------------------------------------------------------------------------
lcd_setup_usart_spi_pins:                       ;
            push        r16                     ; Save registers
            ldi         r16,0b00001000          ; Set USART-SPI ports
            sts         PORTA_DIRSET,r16        ; A3 out for Reset
            sts         PORTA_OUTSET,r16        ;   high
            sts         PORTF_DIRSET,r16        ; F3 out for CS
            sts         PORTF_OUTSET,r16        ;   high
            ldi         r16,0b00001011          ;
            sts         PORTD_DIRSET,r16        ; D0,1,3 out for D0=A0,D1=xkcd,
            sts         PORTD_OUTSET,r16        ; D3=TX. (high)
            ldi         r16,0b00010000          ; Set USART-SPI ports
            sts         PORTE_DIRSET,r16        ; E4 out for backlight
            sts         PORTE_OUTSET,r16        ;   on
            pop         r16                     ; Restore registers
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - lcd_reset
;------------------------------------------------------------------------------
; Resets the LCD.
;
; Makes CS/ low, then Reset/ low then wait 1ms then Reset/ high.
;------------------------------------------------------------------------------
lcd_reset:                                      ;
            push        r16                     ; Save registers
            ldi         r16,0b00001000          ;
            sts         PORTF_OUTCLR,r16        ; F3 = 0 (cs_bar low=active)
            sts         PORTA_OUTCLR,r16        ; A3 = 0 (reset_bar low=start)
            call        wlittle                 ; Delay 1 ms
            sts         PORTA_OUTSET,r16        ; A3 = 1 (reset_bar high)
            pop         r16                     ; Restore registers
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - lcd_setup_spi_on_uartd0
;------------------------------------------------------------------------------
; Setup master SPI on UARTD0.
;
; USART initialization should use the following sequence:
; 1. Set the TxD pin high, and optionally XCK pin low.
; 2. Set the TxD and optionally the XCK pin as output. (done above)
; 3. Set the baud rate and frame format.
; 4. Set the mode of operation (enables the XCK pin output in synchronous mode)
; 5. Enable the transmitter or the receiever, depending on the usage.
;------------------------------------------------------------------------------
lcd_setup_spi_on_uartd0:                        ;
            push        r16                     ; Save registers
            ldi         r16,0b01000000          ; Step 1&2. Invert xck
            sts         PORTD_PIN1CTRL,r16      ; This is part of "SPI MODE 3"
            ldi         r16,0b00000010          ; XCK
            sts         PORTD_OUTCLR,r16        ;
            ldi         r16,0b00001111          ; 3. Set BSEL USART XCK 0x0F
            sts         USARTD0_BAUDCTRLA,r16   ;
            ldi         r16,0b11000011          ; 4. Master, MSB first, hafl of
            sts         USARTD0_CTRLC,r16       ; mode 3, bit0 ???
            ldi         r16,0b00011000          ; 5.
            sts         USARTD0_CTRLB,r16       ; TX & RX Enable
            pop         r16                     ; Restore registers
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - lcd_write_ascii_char
;------------------------------------------------------------------------------
; Displays the character whose ASCII code is stored in r16.
;
; The bit patterns for each ASCII character are stored in the table LCDData in
; program memory. Each character has 6 columns from the table for 6 bytes per
; display box.
;
; The code is multiplied by 6 to produce a byte offset from the start of the
; table. Then the offset is added to the start of the table and that address
; points to the first of the 6 bytes for that code. Each of the 6 bytes is sent
; to the LCD to display.
;
; Input: r16 = ASCII code (00 ... 7F).
;------------------------------------------------------------------------------
lcd_write_ascii_char:                           ;
            push        r17                     ; Save register values
            push        r0                      ;       *
            push        r1                      ;       *
            push        ZL                      ;       *
            push        ZH                      ;       *
            ldi         ZL,low(LCDData << 1)    ; Z = LCDData
            ldi         ZH,high(LCDData << 1)   ;       *
            ldi         r17,6                   ; Z = LCDData + (ASCII * 6)
            mul         r16,r17                 ;       *
            add         ZL,r0                   ;       *
            adc         ZH,r1                   ;       *
            call        lcd_draw_6_col_fig      ; Draw the character on screen
            pop         ZH                      ; Restore register values
            pop         ZL                      ;       *
            pop         r1                      ;       *
            pop         r0                      ;       *
            pop         r17                     ;       *
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - lcd_printf
;------------------------------------------------------------------------------
; Input: Z = address of 0 terminated string.
;
; Algorithm:
;
; while(mem(Z) != 0)
;     lcd_write_ascii_char(mem(Z++))
;------------------------------------------------------------------------------
lcd_printf:                                     ;
            push        r16                     ;
if:                                             ;
            lpm         r16,Z+                  ;
            cpi         r16,0                   ;
            breq        endif                   ;
            call        lcd_write_ascii_char    ;
            jmp         if                      ;
endif:                                          ;
            pop         r16                     ;
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - set_cpu_clk_32_int
;------------------------------------------------------------------------------
; Configures the cpu clock to operate at 32 MHz, using the internal
; oscillator.
;------------------------------------------------------------------------------
set_cpu_clk_32_int:
            lds         r16,OSC_CTRL            ; Enable the 32 MHz oscillator
            ori         r16,0b00000010          ;       *
            sts         OSC_CTRL,r16            ;       *
while1:                                         ;
            lds         r16,OSC_STATUS          ; Wait until it is stable
            andi        r16,0x02                ;       *
            breq        while1                  ;       *
            ldi         r16,0xD8                ; Unlock protected register
            out         CPU_CCP,r16             ;       *
            ldi         r16,0x01                ; Connect 32M osc to sysclk
            sts         CLK_CTRL,r16            ;       *
            ldi         r16,0xD8                ; Unlock protected register
            out         CPU_CCP,r16             ;       *
            ldi         r16,0x00                ; Reset prescaler A,B,C to 1
            sts         CLK_PSCTRL,r16          ;       *
            ldi         r16,0xD8                ; Unlock protected register
            out         CPU_CCP,r16             ;       *
            lds         r16,OSC_DFLLCTRL        ; Sel. int 32.768 KHz src for
            andi        r16,0b11111101          ; the RC32M DFLL
            sts         OSC_DFLLCTRL,r16        ;       *
            lds         r16,DFLLRC32M_CTRL      ; Enable the DFLL for RC32MHz
            ori         r16,0x01                ;       *
            sts         DFLLRC32M_CTRL,r16      ;       *
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - lcd_write_dec_digit
;------------------------------------------------------------------------------
; Converts a binary number to a decimal digt in ASCII.
;
; Input: r16 a binary number less than ten.
; Output: r16 the ascii code of the number.
;------------------------------------------------------------------------------
lcd_write_dec_digit:                            ;
            push        r17                     ;
            ldi         r17,'0'                 ;
            add         r16,r17                 ;
            mov         r0,r16                  ;
            call        lcd_write_ascii_char    ;
            pop         r17                     ;
            ret                                 ;
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Subroutine - binbcd
;------------------------------------------------------------------------------
; Converts unsigned 16 bit number to 5 digit ASCII number in decimal.
;
; Input: R17, R16 = 16 bit value 0 ... 65535.
; Output: R20, R19, R18, R17, R16 = 5 digits (ASCII).
;------------------------------------------------------------------------------
binbcd:                                         ;
            ldi         r20, -1 + '0'           ;
_bcd1:      inc         r20                     ;
            subi        r16, low(10000)         ; -10000
            sbci        r17, high(10000)        ;
            brcc        _bcd1                   ;
            ldi         r19, 10 + '0'           ;
_bcd2:      dec         r19                     ;
            subi        r16, low(-1000)         ; +1000
            sbci        r17, high(-1000)        ;
            brcs        _bcd2                   ;
            ldi         r18, -1 + '0'           ;
_bcd3:      inc         r18                     ;
            subi        r16, low(100)           ; -100
            sbci        r17, high(100)          ;
            brcc        _bcd3                   ;
            ldi         r17, 10 + '0'           ;
_bcd4:      dec         r17                     ;
            subi        r16, -10                ; +10
            brcs        _bcd4                   ;
            subi        r16, -'0'               ;
            ret                                 ;
;-------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Array - LCDData
;------------------------------------------------------------------------------
; The data containing the pixel positions for the ASCII (128) codes for
; various characters.
;
; Notice that many characters are absent from this list and will need to be
; added if they are to be used.
;------------------------------------------------------------------------------
LCDData:                                              ;
LCDData00:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; NULL
LCDData01:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; SOH
LCDData02:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; STX
LCDData03:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; ETX
LCDData04:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; EOT
LCDData05:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; ENQ
LCDData06:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; ACK
LCDData07:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; BEL
LCDData08:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; BS
LCDData09:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; HT
LCDData0A:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; LF
LCDData0B:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; VT
LCDData0C:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; FF
LCDData0D:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; CR
LCDData0E:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; SO
LCDData0F:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; SI
LCDData10:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; DLE
LCDData11:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; DC1
LCDData12:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; DC2
LCDData13:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; DC3
LCDData14:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; DC4
LCDData15:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; NAK
LCDData16:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; SYN
LCDData17:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; ETB
LCDData18:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; CAN
LCDData19:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; EM
LCDData1A:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; SUB
LCDData1B:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; ESC
LCDData1C:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; FS
LCDData1D:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; GS
LCDData1E:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; RS
LCDData1F:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; US
LCDData20:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; BLANK
LCDData21:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; !
LCDData22:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; "
LCDData23:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; #
LCDData24:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; $
LCDData25:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; %
LCDData26:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; &
LCDData27:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; '
LCDData28:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; (
LCDData29:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; )
LCDData2A:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; *
LCDData2B:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; +
LCDData2C:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; ,
LCDData2D:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; -
LCDData2E:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; .
LCDData2F:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; /
LCDData30:  .db         0x3E,0x45,0x49,0x51,0x3E,0x00 ; 0
LCDData31:  .db         0x00,0x00,0x7F,0x00,0x00,0x00 ; 1
LCDData32:  .db         0x32,0x49,0x49,0x49,0x06,0x00 ; 2
LCDData33:  .db         0x41,0x49,0x49,0x49,0x36,0x00 ; 3
LCDData34:  .db         0x0F,0x08,0x7E,0x08,0x08,0x00 ; 4
LCDData35:  .db         0x2F,0x49,0x49,0x49,0x31,0x00 ; 5
LCDData36:  .db         0x3E,0x49,0x49,0x49,0x32,0x00 ; 6
LCDData37:  .db         0x41,0x21,0x11,0x09,0x07,0x00 ; 7
LCDData38:  .db         0x36,0x49,0x49,0x49,0x36,0x00 ; 8
LCDData39:  .db         0x06,0x09,0x09,0x09,0x7E,0x00 ; 9
LCDData3A:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; :
LCDData3B:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; ;
LCDData3C:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; <
LCDData3D:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; =
LCDData3E:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; >
LCDData3F:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; ?
LCDData40:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; @
LCDData41:  .db         0x7C,0x12,0x11,0x12,0x7C,0x00 ; A
LCDData42:  .db         0x7F,0x49,0x49,0x49,0x36,0x00 ; B
LCDData43:  .db         0x3E,0x41,0x41,0x41,0x41,0x00 ; C
LCDData44:  .db         0x7F,0x41,0x41,0x41,0x3E,0x00 ; D
LCDData45:  .db         0x7F,0x49,0x49,0x41,0x41,0x00 ; E
LCDData46:  .db         0x7F,0x09,0x09,0x01,0x00,0x00 ; F
LCDData47:  .db         0x3E,0x41,0x41,0x49,0x3A,0x00 ; G
LCDData48:  .db         0x7F,0x08,0x08,0x08,0x7F,0x00 ; H
LCDData49:  .db         0x41,0x41,0x7F,0x41,0x41,0x00 ; I
LCDData4A:  .db         0x21,0x41,0x3F,0x01,0x01,0x00 ; J
LCDData4B:  .db         0x7F,0x08,0x14,0x22,0x41,0x00 ; K
LCDData4C:  .db         0x7F,0x40,0x40,0x40,0x40,0x00 ; L
LCDData4D:  .db         0x7F,0x02,0x04,0x02,0x7F,0x00 ; M
LCDData4E:  .db         0x7F,0x04,0x08,0x10,0x7F,0x00 ; N
LCDData4F:  .db         0x3E,0x41,0x41,0x41,0x3E,0x00 ; O
LCDData50:  .db         0x7F,0x09,0x09,0x09,0x06,0x00 ; P
LCDData51:  .db         0x3E,0x41,0x41,0x21,0x5E,0x00 ; Q
LCDData52:  .db         0x7E,0x09,0x19,0x29,0x46,0x00 ; R
LCDData53:  .db         0x26,0x49,0x49,0x49,0x32,0x00 ; S
LCDData54:  .db         0x01,0x01,0x7F,0x01,0x01,0x00 ; T
LCDData55:  .db         0x3F,0x40,0x40,0x40,0x3F,0x00 ; U
LCDData56:  .db         0x1F,0x20,0x40,0x20,0x1F,0x00 ; V
LCDData57:  .db         0x3F,0x40,0x20,0x40,0x3F,0x00 ; W
LCDData58:  .db         0x63,0x14,0x08,0x14,0x63,0x00 ; X
LCDData59:  .db         0x03,0x04,0x78,0x04,0x03,0x00 ; Y
LCDData5A:  .db         0x61,0x51,0x49,0x45,0x43,0x00 ; Z
LCDData5B:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; [
LCDData5C:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; BACKSLASH
LCDData5D:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; ]
LCDData5E:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; ^
LCDData5F:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; _
LCDData60:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; `
LCDData61:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; a
LCDData62:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; b
LCDData63:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; c
LCDData64:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; d
LCDData65:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; e
LCDData66:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; f
LCDData67:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; g
LCDData68:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; h
LCDData69:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; i
LCDData6A:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; j
LCDData6B:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; k
LCDData6C:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; l
LCDData6D:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; m
LCDData6E:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; n
LCDData6F:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; o
LCDData70:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; p
LCDData71:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; q
LCDData72:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; r
LCDData73:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; s
LCDData74:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; t
LCDData75:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; u
LCDData76:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; v
LCDData77:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; w
LCDData78:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; x
LCDData79:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; y
LCDData7A:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; z
LCDData7B:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; {
LCDData7C:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; |
LCDData7D:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; }
LCDData7E:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; ~
LCDData7F:  .db         0x00,0x00,0x00,0x00,0x00,0x00 ; DEL
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
; Strings
;------------------------------------------------------------------------------
s_program:    .db         "PROGRAM",0x00           ;
s_quantity:   .db         "QUANTITY",0x00          ;
;------------------------------------------------------------------------------
