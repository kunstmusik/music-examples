<CsoundSynthesizer>
<CsOptions>
-odac --port=10000      
</CsOptions>
<CsInstruments>

sr	    =  48000
ksmps	    = 32 
nchnls    =  2
0dbfs	    =  1

gilisten  OSCinit  10005 

opcode declick, a, a
  ain xin
  aenv = linseg:a(0, 0.01, 1, p3 - 0.02, 1, 0.01, 0, 0.01, 0)
  xout ain * aenv
endop

instr 1 
  print p3
  print p4 
  print p5
  icut = sr / 3
  asig = vco2(ampdbfs(-12), p4) 
  asig += vco2(ampdbfs(-12), p4 * 1.5) 
  asig = zdf_ladder(asig, expon(icut, p3, 400), 5)
  asig = declick(asig) * p5
  outc(asig, asig)
endin

instr Server

kinstr init 0
kdur init 0
kamp init 0
kfreq init 0

msgloop:
kk = OSClisten(gilisten, "/i", "iddd", kinstr, kdur, kfreq, kamp) 

if (kk == 1) then
  event("i", kinstr, 0, kdur, kfreq, min(kamp, 1.0))  
  kgoto msgloop
endif

endin

schedule("Server", 0, 10000)

</CsInstruments>
<CsScore>
f0 1000000
</CsScore>
</CsoundSynthesizer>
