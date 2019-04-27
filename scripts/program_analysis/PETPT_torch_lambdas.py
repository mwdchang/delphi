import torch


def petpt__assign__td_0(tmax, tmin):
    return ((0.6*tmax)+(0.4*tmin))


def petpt__condition__IF_1_0(xhlai):
    return (xhlai <= 0.0)


def petpt__assign__albedo_0(msalb):
    return msalb


def petpt__assign__albedo_1(msalb, xhlai):
    return (0.23-((0.23-msalb)*torch.exp(-((0.75*xhlai)))))


def petpt__decision__albedo_2(IF_1_0, albedo_1, albedo_0):
    return torch.where(IF_1_0, albedo_0, albedo_1)


def petpt__assign__slang_0(srad):
    return srad*23.923


def petpt__assign__eeq_0(slang, albedo, td):
    return (slang*(0.000204-(0.000183*albedo)))*(td+29.0)


def petpt__assign__eo_0(eeq):
    return eeq*1.1


def petpt__condition__IF_2_0(tmax):
    return (tmax > 35.0)


def petpt__assign__eo_1(eeq, tmax):
    return (eeq*(((tmax-35.0)*0.05)+1.1))


def petpt__assign__eo_2(eeq, tmax):
    return ((eeq*0.01)*torch.exp((0.18*(tmax+20.0))))


def petpt__decision__eo_3(IF_1_1, eo_0, eo_1):
    return torch.where(IF_1_1, eo_1, eo_0)


def petpt__condition__IF_2_1(tmax):
    return (tmax < 5.0)


def petpt__decision__eo_4(IF_2_0, eo_3, eo_2):
    return torch.where(IF_2_0, eo_2, eo_3)


def petpt__assign__eo_5(eo):
    return torch.max(eo, torch.full_like(eo, 0.0001))
