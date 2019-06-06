function petasce(canht, doy, msalb, meevp, srad, tdew, tmax, tmin, windht, windrun, xhlai, xlat, xelev, eo)
	tavg = (tmax+tmin)/2.0
	patm = 101.3*(((293.0-(0.0065*xelev))/293.0)^5.26)
	psycon = 0.000665*patm
	udelta = (2503.0*exp(((17.27*tavg)/(tavg+237.3))))/((tavg+237.3)^2.0)
	emax = 0.6108*exp(((17.27*tmax)/(tmax+237.3)))
	emin = 0.6108*exp(((17.27*tmin)/(tmin+237.3)))
	es = (emax+emin)/2.0
	ea = 0.6108*exp(((17.27*tdew)/(tdew+237.3)))
	rhmin = max(20.0, min(80.0, ((ea/emax)*100.0)))
	albedo = 0.23
	albedo = ifelse(xhlai <= 0.0, msalb, albedo)
	rns = (1.0-albedo)*srad
	pie = 3.14159265359
	dr = 1.0+(0.033*cos((((2.0*pie)/365.0)*doy)))
	ldelta = 0.409*sin(((((2.0*pie)/365.0)*doy)-1.39))
	ws = acos(-(((1.0*tan(((xlat*pie)/180.0)))*tan(ldelta))))
	ra1 = (ws*sin(((xlat*pie)/180.0)))*sin(ldelta)
	ra2 = (cos(((xlat*pie)/180.0))*cos(ldelta))*sin(ws)
	ra = (((24.0/pie)*4.92)*dr)*(ra1+ra2)
	rso = (0.75+(2e-05*xelev))*ra
	ratio = srad/rso
	ratio = ifelse(ratio < 0.3, 0.3, ratio)
	ratio = ifelse(ratio > 1.0, 1.0, ratio)
	fcd = (1.35*ratio)-0.35
	tk4 = (((tmax+273.16)^4.0)+((tmin+273.16)^4.0))/2.0
	rnl = ((4.901e-09*fcd)*(0.34-(0.14*sqrt(ea))))*tk4
	rn = rns-rnl
	g = 0.0
	windsp = (((windrun*1000.0)/24.0)/60.0)/60.0
	wind2m = windsp*(4.87/log(((67.8*windht)-5.42)))
	cn = 0.0
	cd = 0.0
	cn = ifelse(meevp == "A", 1600.0, cn)
	cd = ifelse(meevp == "A", 0.38, cd)
	cn = ifelse(meevp == "G", 900.0, cn)
	cd = ifelse(meevp == "G", 0.34, cd)
	refet = ((0.408*udelta)*(rn-g))+(((psycon*(cn/(tavg+273.0)))*wind2m)*(es-ea))
	refet = refet/(udelta+(psycon*(1.0+(cd*wind2m))))
	refet = max(0.0001, refet)
	skc = 0.8
	kcbmin = 0.3
	kcbmax = 1.2
	kcb = max(0.0, (kcbmin+((kcbmax-kcbmin)*(1.0-exp(-(((1.0*skc)*xhlai)))))))
	kcb = ifelse(xhlai <= 0.0, 0.0, kcb)
	wnd = max(1.0, min(wind2m, 6.0))
	cht = max(0.001, canht)
	kcmax = 0.5
	kcmax = ifelse(meevp == "A", max(1.0, (kcb+0.05)), kcmax)
	kcmax = ifelse(meevp == "G", max((1.2+(((0.04*(wnd-2.0))-(0.004*(rhmin-45.0)))*((cht/3.0)^0.3))), (kcb+0.05)), kcmax)
	fc = ((kcb-kcbmin)/(kcmax-kcbmin))^(1.0+(0.5*canht))
	fc = ifelse(kcb <= kcbmin, 0.0, fc)
	fw = 1.0
	few = min((1.0-fc), fw)
	ke = max(0.0, min((1.0*(kcmax-kcb)), (few*kcmax)))
	eo = (kcb+ke)*refet
	eo = max(eo, 0.0001)
end