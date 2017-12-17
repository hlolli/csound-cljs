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


instr 1

  ires abs -0.5
  ival = (1 + 2 + 3)
  print ival
  a1 poscil 0.2, 330
  out a1 + ires

endin


</CsInstruments>
<CsScore>

f1 0 4096 10 1

i 1 0 1
e


</CsScore>
</CsoundSynthesizer>
