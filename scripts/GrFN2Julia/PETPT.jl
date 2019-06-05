function petpt(msalb, srad, tmax, tmin, xhlai, eo)
	td = (0.6*tmax)+(0.4*tmin)
	albedo = (xhlai <= 0.0) ? msalb : 0.23-((0.23-msalb)*exp(-((0.75*xhlai))))
	slang = srad*23.923
	eeq = (slang*(0.000204-(0.000183*albedo)))*(td+29.0)
	eo = (tmax > 35.0) ? eeq*(((tmax-35.0)*0.05)+1.1) : eeq*1.1
	eo = (tmax < 5.0) ? (eeq*0.01)*exp((0.18*(tmax+20.0))) : eo
	eo = max(eo, 0.0001)
end