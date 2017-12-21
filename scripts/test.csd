<CsoundSynthesizer>
<CsOptions>
; Select audio/midi flags here according to platform
-odac   ;;;realtime audio out
;-iadc    ;;;uncomment -iadc if realtime audio input is needed too
; For Non-realtime ouput leave only the line below:
; -o tigoto.wav -W ;;; for file output any platform
</CsOptions>
<CsInstruments>

sr = 44100 
ksmps = 32 
nchnls = 2
0dbfs  = 1 


instr 2
  k1 = 1
  i1 = 1
  if (p3==p3) then
    prints "JES\n"
  endif
endin

instr 1
  ; Get the value of the 4th p-field from the score.
  kparam = p4
  printk 0.1, kparam
  print i(kparam)
  ; If iparam is 1 then play the high note.
  ; If not then play the low note.
      if (kparam == 1) igoto highnot__e
      igoto lownote

    highnot__e:
	ifreq = 880
	goto playit

      lownote:
	ifreq = 440
	goto playit

      playit:
	; Print the values of iparam and ifreq.
	;; print iparam
 	;; print ifreq

	a1 oscil 0.1, ifreq, 1
	out a1
endin



</CsInstruments>
<CsScore>
; Table #1: a simple sine wave.
f 1 0 32768 10 1

; p4: 1 = high note, anything else = low note
; Play Instrument #1 for one second, a low note.
i 1 0 1 0
; Play a Instrument #1 for one second, a high note.
i 1 1 1 1

</CsScore>
</CsoundSynthesizer>
