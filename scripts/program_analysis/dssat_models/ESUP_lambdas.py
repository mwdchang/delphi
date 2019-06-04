import math

def esup__assign__sumes1_0(sumes1, eos):
    return (sumes1+eos)

def esup__condition__IF_1_0(sumes1, u):
    return (sumes1 > u)

def esup__assign__es_0(eos, sumes1, u):
    return (eos-(0.4*(sumes1-u)))

def esup__assign__sumes2_0(sumes1, u):
    return (0.6*(sumes1-u))

def esup__assign__t_0(sumes2):
    return ((sumes2/3.5)**2)

def esup__assign__sumes1_1(u):
    return u

def esup__assign__es_1(eos):
    return eos

def esup__decision__t_1(IF_1_0, t_-1, t_0):
    return t_0 if IF_1_0 else t_-1

def esup__decision__sumes2_1(IF_1_0, sumes2_-1, sumes2_0):
    return sumes2_0 if IF_1_0 else sumes2_-1

def esup__decision__es_2(IF_1_0, es_1, es_0):
    return es_0 if IF_1_0 else es_1

def esup__decision__sumes1_2(IF_1_0, sumes1_0, sumes1_1):
    return sumes1_1 if IF_1_0 else sumes1_0

