import math

def flood_evap__assign__ef_0(eo, xlai):
    return (eo*(1.0-(0.53*xlai)))

def flood_evap__condition__IF_1_0(xlai):
    return (xlai > 0.85)

def flood_evap__assign__ef_1(eo, xlai):
    return ((eo/1.1)*math.exp(-((0.6*xlai))))

def flood_evap__decision__ef_2(IF_1_0, ef_0, ef_1):
    return ef_1 if IF_1_0 else ef_0

