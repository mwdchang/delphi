import math

def solar__assign__sradj_0(srad):
    return (srad*1000000.0)

def solar__assign__ssin_0(rad, dec, xlat):
    return (math.sin((rad*dec))*math.sin((rad*xlat)))

def solar__assign__ccos_0(rad, dec, xlat):
    return (math.cos((rad*dec))*math.cos((rad*xlat)))

def solar__assign__soc_0(ssin, ccos):
    return (ssin/ccos)

def solar__assign__soc_1(soc):
    return min(max(soc, -(1.0)), 1.0)

def solar__assign__dsinb_0(dayl, ssin, pi, ccos, soc):
    return (3600.0*((dayl*ssin)+(((24.0/pi)*ccos)*math.sqrt((1.0-(soc**2))))))

def solar__assign__s0n_0(sc):
    return sc

def solar__assign__s0d_0(s0n, dsinb):
    return (s0n*dsinb)

def solar__assign__amtrd_0(sradj, s0d):
    return (sradj/s0d)

def solar__assign__sclear_0(amtrcs, s0d):
    return ((amtrcs*s0d)*1e-06)

def solar__assign__clouds_0(srad, sclear):
    return min(max((1.0-(srad/sclear)), 0.0), 1.0)

def solar__assign__isinb_0(dayl, ssin, ccos, pi, soc):
    return (3600.0*((dayl*(ssin+(0.4*((ssin**2)+(0.5*(ccos**2))))))+((((24.0/pi)*ccos)*(1.0+((1.5*0.4)*ssin)))*math.sqrt((1.0-(soc**2))))))

