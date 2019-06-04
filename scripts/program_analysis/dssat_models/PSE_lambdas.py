import math

def pse__assign__ke_0():
    return 0

def pse__assign__refet_0():
    return 0

def pse__condition__IF_1_0(ke):
    return (ke >= 0.0)

def pse__assign__eos_0(ke, refet):
    return (ke*refet)

def pse__assign__eos_1(eo, xlai):
    return (eo*(1.0-(0.39*xlai)))

def pse__assign__eos_2(eo, xlai):
    return ((eo/1.1)*math.exp(-((0.4*xlai))))

def pse__assign__eos_3(eo, ksevap, xlai):
    return (eo*math.exp(-((ksevap*xlai))))

def pse__decision__eos_4(IF_1_0, eos_-1, eos_0):
    return eos_0 if IF_1_0 else eos_-1

def pse__condition__IF_1_1(ksevap):
    return (ksevap <= 0.0)

def pse__decision__eos_5(IF_1_1, eos_4, eos_-1):
    return eos_-1 if IF_1_1 else eos_4

def pse__assign__eos_6(eos):
    return max(eos, 0.0)

