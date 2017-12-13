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

  reset:
    print 3/2

  contin:
    print times()
    rireturn

endin


</CsInstruments>
<CsScore>

f1 0 4096 10 1

i1 0 10
e


</CsScore>
</CsoundSynthesizer>
