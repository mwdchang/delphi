import torch

def petasce__assign__tavg_0(tmax, tmin):
    return ((tmax+tmin)/2.0)

def petasce__assign__patm_0(xelev):
    return (101.3*(((293.0-(0.0065*xelev))/293.0)**5.26))

def petasce__assign__psycon_0(patm):
    return (0.000665*patm)

def petasce__assign__udelta_0(tavg):
    return ((2503.0*torch.exp(((17.27*tavg)/(tavg+237.3))))/((tavg+237.3)**2.0))

def petasce__assign__emax_0(tmax):
    return (0.6108*torch.exp(((17.27*tmax)/(tmax+237.3))))

def petasce__assign__emin_0(tmin):
    return (0.6108*torch.exp(((17.27*tmin)/(tmin+237.3))))

def petasce__assign__es_0(emax, emin):
    return ((emax+emin)/2.0)

def petasce__assign__ea_0(tdew):
    return (0.6108*torch.exp(((17.27*tdew)/(tdew+237.3))))

def petasce__assign__rhmin_0(ea, emax):
    return torch.max(
        torch.full_like(ea, 20.0),
        torch.min(torch.full_like(ea, 80.0), ((ea/emax)*100.0))
    )

def petasce__condition__IF_1_0(xhlai):
    return (xhlai <= 0.0)

def petasce__assign__albedo_0(msalb):
    return msalb

def petasce__assign__albedo_1():
    return 0.23

def petasce__decision__albedo_2(IF_1_0, albedo_1, albedo_0):
    return torch.where(IF_1_0, albedo_0, albedo_1)

def petasce__assign__rns_0(albedo, srad):
    return ((1.0-albedo)*srad)

def petasce__assign__pie_0():
    return 3.14159265359

def petasce__assign__dr_0(pie, doy):
    return (1.0+(0.033*torch.cos((((2.0*pie)/365.0)*doy))))

def petasce__assign__ldelta_0(pie, doy):
    return (0.409*torch.sin(((((2.0*pie)/365.0)*doy)-1.39)))

def petasce__assign__ws_0(xlat, pie, ldelta):
    return torch.acos(-(((1.0*torch.tan(((xlat*pie)/180.0)))*torch.tan(ldelta))))

def petasce__assign__ra1_0(ws, xlat, pie, ldelta):
    return ((ws*torch.sin(((xlat*pie)/180.0)))*torch.sin(ldelta))

def petasce__assign__ra2_0(xlat, pie, ldelta, ws):
    return ((torch.cos(((xlat*pie)/180.0))*torch.cos(ldelta))*torch.sin(ws))

def petasce__assign__ra_0(pie, dr, ra1, ra2):
    return ((((24.0/pie)*4.92)*dr)*(ra1+ra2))

def petasce__assign__rso_0(xelev, ra):
    return ((0.75+(2e-05*xelev))*ra)

def petasce__assign__ratio_0(srad, rso):
    return (srad/rso)

def petasce__condition__IF_2_0(ratio):
    return (ratio < 0.3)

def petasce__assign__ratio_1():
    return 0.3

def petasce__assign__ratio_2():
    return 1.0

def petasce__decision__ratio_3(IF_2_0, ratio_0, ratio_1):
    return torch.where(IF_2_0, ratio_1, ratio_0)

def petasce__condition__IF_2_1(ratio):
    return (ratio > 1.0)

def petasce__decision__ratio_4(IF_2_1, ratio_3, ratio_2):
    return torch.where(IF_2_1, ratio_2, ratio_3)

def petasce__assign__fcd_0(ratio):
    return ((1.35*ratio)-0.35)

def petasce__assign__tk4_0(tmax, tmin):
    return ((((tmax+273.16)**4.0)+((tmin+273.16)**4.0))/2.0)

def petasce__assign__rnl_0(fcd, ea, tk4):
    return (((4.901e-09*fcd)*(0.34-(0.14*torch.sqrt(ea))))*tk4)

def petasce__assign__rn_0(rns, rnl):
    return (rns-rnl)

def petasce__assign__g_0():
    return 0.0

def petasce__assign__windsp_0(windrun):
    return ((((windrun*1000.0)/24.0)/60.0)/60.0)

def petasce__assign__wind2m_0(windsp, windht):
    return (windsp*(4.87/torch.log(((67.8*windht)-5.42))))

def petasce__assign__cn_0():
    return 0.0

def petasce__assign__cd_0():
    return 0.0

def petasce__condition__IF_3_0(meevp):
    return torch.tensor((meevp == "A").astype(int), dtype=torch.uint8)

def petasce__assign__cn_1():
    return 1600.0

def petasce__assign__cd_1():
    return 0.38

def petasce__assign__cn_2():
    return 900.0

def petasce__assign__cd_2():
    return 0.34

def petasce__decision__cd_3(IF_3_0, cd_0, cd_1):
    return torch.where(IF_3_0, cd_1, cd_0)

def petasce__decision__cn_3(IF_3_0, cn_0, cn_1):
    return torch.where(IF_3_0, cn_1, cn_0)

def petasce__condition__IF_3_1(meevp):
    return torch.tensor((meevp == "G").astype(int), dtype=torch.uint8)

def petasce__decision__cd_4(IF_3_1, cd_3, cd_2):
    return torch.where(IF_3_1, cd_2, cd_3)

def petasce__decision__cn_4(IF_3_1, cn_3, cn_2):
    return torch.where(IF_3_1, cn_2, cn_3)

def petasce__assign__refet_0(udelta, rn, g, psycon, cn, tavg, wind2m, es, ea):
    return (((0.408*udelta)*(rn-g))+(((psycon*(cn/(tavg+273.0)))*wind2m)*(es-ea)))

def petasce__assign__refet_1(refet, udelta, psycon, cd, wind2m):
    return (refet/(udelta+(psycon*(1.0+(cd*wind2m)))))

def petasce__assign__refet_2(refet):
    return torch.max(torch.full_like(refet, 0.0001), refet)

def petasce__assign__skc_0():
    return 0.8

def petasce__assign__kcbmin_0():
    return 0.3

def petasce__assign__kcbmax_0():
    return 1.2

def petasce__condition__IF_4_0(xhlai):
    return (xhlai <= 0.0)

def petasce__assign__kcb_0():
    return 0.0

def petasce__assign__kcb_1(kcbmin, kcbmax, skc, xhlai):
    return torch.max(
        torch.full_like(kcbmin, 0.0),
        (kcbmin+((kcbmax-kcbmin)*(1.0-torch.exp(-(((1.0*skc)*xhlai))))))
    )

def petasce__decision__kcb_2(IF_4_0, kcb_1, kcb_0):
    return torch.where(IF_4_0, kcb_0, kcb_1)

def petasce__assign__wnd_0(wind2m):
    return torch.max(
        torch.full_like(wind2m, 1.0),
        torch.min(wind2m, torch.full_like(wind2m, 6.0))
    )

def petasce__assign__cht_0(canht):
    return torch.max(torch.full_like(canht, 0.001), canht)

def petasce__assign__kcmax_0():
    return 0.5

def petasce__condition__IF_5_0(meevp):
    return torch.tensor((meevp == "A").astype(int), dtype=torch.uint8)

def petasce__assign__kcmax_1(kcb):
    return torch.max(torch.full_like(kcb, 1.0), (kcb+0.05))

def petasce__assign__kcmax_2(wnd, rhmin, cht, kcb):
    return torch.max(
        (1.2+(((0.04*(wnd-2.0))-(0.004*(rhmin-45.0)))*((cht/3.0)**0.3))),
        (kcb+0.05)
    )

def petasce__decision__kcmax_3(IF_5_0, kcmax_0, kcmax_1):
    return torch.where(IF_5_0, kcmax_1, kcmax_0)

def petasce__condition__IF_5_1(meevp):
    return torch.tensor((meevp == "G").astype(int), dtype=torch.uint8)

def petasce__decision__kcmax_4(IF_5_1, kcmax_3, kcmax_2):
    return torch.where(IF_5_1, kcmax_2, kcmax_3)

def petasce__condition__IF_6_0(kcb, kcbmin):
    return (kcb <= kcbmin)

def petasce__assign__fc_0():
    return 0.0

def petasce__assign__fc_1(kcb, kcbmin, kcmax, canht):
    return (((kcb-kcbmin)/(kcmax-kcbmin))**(1.0+(0.5*canht)))

def petasce__decision__fc_2(IF_6_0, fc_1, fc_0):
    return torch.where(IF_6_0, fc_0, fc_1)

def petasce__assign__fw_0():
    return 1.0

def petasce__assign__few_0(fc, fw):
    return torch.min((1.0-fc), fw)

def petasce__assign__ke_0(kcmax, kcb, few):
    return torch.max(
        torch.full_like(kcmax, 0.0),
        torch.min((1.0*(kcmax-kcb)), (few*kcmax))
    )

def petasce__assign__eo_0(kcb, ke, refet):
    return ((kcb+ke)*refet)

def petasce__assign__eo_1(eo):
    return torch.max(eo, torch.full_like(eo, 0.0001))
