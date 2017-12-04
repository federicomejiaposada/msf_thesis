***********************************************************************
************** FECHAS OIL ******************************************


clear all
set more off

import excel "C:\Users\Earendil\Google Drive\01_MSc_Finance\001_Thesis_proposal\Programs\data_colombia.xlsx", sheet("returns") firstrow


format DATE %tdnn/dd/YY
rename DATE Date
tsset Date
gen index=Date
gen index_2= _n


gen indica=0


local fechas_1 " 3/5/2008 4/29/2008 5/6/2008 6/23/2008 9/23/2008 11/7/2008 12/22/2008 4/29/2009 7/28/2009 2/22/2010 6/21/2010 8/17/2010 9/2/2010 9/9/2010 11/22/2010 1/11/2011  "

local fechas_2 " 2/7/2011 2/25/2011 3/22/2011 4/4/2011 4/12/2011 4/19/2011 11/2/2011 1/17/2012 3/12/2012 3/20/2012 5/9/2012 7/3/2012 7/23/2012 8/14/2012 8/21/2012 1/3/2013   "

local fechas_3 "  1/21/2013 2/8/2013 5/20/2013 6/24/2013 7/4/2013 7/29/2013 10/3/2013 12/3/2013 1/2/2014 1/27/2014 2/21/2014 3/17/2014 3/25/2014 5/2/2014 6/5/2014 6/25/2014 "

local fechas_4 "   7/11/2014 7/31/2014 8/13/2014 9/22/2014 10/17/2014 11/5/2014 11/26/2014 4/6/2015 4/20/2015 6/1/2015 6/9/2015 6/16/2015 6/22/2015 6/30/2015 8/3/2015 9/7/2015 2/8/2016 3/14/2016 4/29/2016 5/17/2016 9/5/2016 10/4/2016 10/24/2016 "

local fechas " `fechas_1' `fechas_2' `fechas_3' `fechas_4'"

foreach x of local fechas {
local i = `i' + 1
display "`x'"

gen newrecord_date = subinstr("`x'", "/", "/", .)
gen D=date(newrecord_date,"MDY")
format D %tdnn/dd/YY
drop  newrecord_date
replace indica=`i' if D==Date
drop D




}











display "*************************************************************************************************"
display "REGRESIONES AZULES"

foreach x of local fechas {
******Esta es la fecha analizada***
display "Esta es la fecha analizada"
display "`x'"
display ""
******Este es el evento numero***

local e = `e' + 1
display ""
display "Este es el evento numero"
display "`e'"
display ""

quietly sum index_2 if indica==`e'
local down= r(min)-251
local up= r(min)
*display `down'
*display `up'
*reg COLCAP IBOV if (index_2 > `down'  & `up' > index_2 )
quietly arch COLCAP L.COLCAP IBOV  if (index_2 > `down'  & `up' > index_2 ) ,  arch(1) garch(1) iter(50)
matrix A =r(table)
local alp`e' = A[1,3]
display "ESTA ES LA CONSTANTE DEL MODELO DE MEDIA"
display `alp`e''
display ""
local beta`e' = A[1,1]
display "ESTE ES EL PRIMER COEFICIENTE DEL MODELO DE MEDIA"
display `beta`e''
display ""
local etha`e'= A[1,2]
display "ESTE ES EL SEGUNDO  COEFICIENTE DEL MODELO DE MEDIA"
display `etha`e''
display ""

local gama_cero`e' = A[1,6]
display "ESTA ES LA CONSTANTE DEL MODELO DE VOLATILIDAD GAMMA_0"
display `gama_cero`e''
display ""
local gamma_uno`e' = A[1,4]
display "ESTE ES EL PRIMER COEFICIENTE DEL MODELO GARCH GAMMA_1"
display `gamma_uno`e''
display ""
local gamma_dos`e'= A[1,5]
display "ESTE ES EL SEGUNDO  COEFICIENTE DEL MODELO GARCH GAMMA_2"
display `gamma_dos`e''
display ""

************PONERLE ESTO A TODOS LOS CODIGOS
predict variance
local h_t_`e'=variance[`up']
display "ESTE ES EL H_T"
display `h_t_`e''
drop variance
************PONERLE ESTO A TODOS LOS CODIGOS



foreach tau of numlist 0/4{
local ar_`e'_`tau'=COLCAP[`up'+`tau']- `alp`e''- `beta`e''*COLCAP[`up'+`tau'-1] -`etha`e''*IBOV[`up'+`tau']
display "ESTA ES LA ar_`e'_`tau'"
display `ar_`e'_`tau''
display ""
}


display ""
display "________________________________________________________________________________________________________________________________"
display ""
display ""

}



******************************************************************************
******************* ARS PARA EVENTOS Y TAOS***********************************


***********TAO_0**************************************************************
local sumatoria_eventos_tao0=0
foreach x of local fechas {
local n = `n' + 1
local sumatoria_eventos_tao0= `sumatoria_eventos_tao0' + `ar_`n'_0'

display "ar_`n'_0"
display `ar_`n'_0'
local ar_23_0=0
local ar_64_0=0
local ar_65_0=0



}
display "SUMA DE TODOS LOS AR VENTANA_0"
display `sumatoria_eventos_tao0'
display ""


***********TAO_1**************************************************************
local sumatoria_eventos_tao1=0
foreach x of local fechas {
local z = `z' + 1
local sumatoria_eventos_tao1= `sumatoria_eventos_tao1' + `ar_`z'_1'
display "ar_`z'_1"
display `ar_`z'_1'
local ar_34_1=0
local ar_57_1=0
local ar_65_1=0



}
display "SUMA DE TODOS LOS AR VENTANA_1"
display `sumatoria_eventos_tao1'
display ""


***********TAO_2**************************************************************
local sumatoria_eventos_tao2=0
foreach x of local fechas {
local y = `y' + 1
local sumatoria_eventos_tao2= `sumatoria_eventos_tao2' + `ar_`y'_2'

display "ar_`y'_2"
display `ar_`y'_2'

local ar_7_2=0
local ar_34_2=0
local ar_69_2=0

}
display "SUMA DE TODOS LOS AR VENTANA_2"
display `sumatoria_eventos_tao2'
display ""



***********TAO_3**************************************************************
local sumatoria_eventos_tao3=0
foreach x of local fechas {
local l = `l' + 1
local sumatoria_eventos_tao3= `sumatoria_eventos_tao3' + `ar_`l'_3'

display "ar_`l'_3"
display `ar_`l'_3'
local ar_13_3 =0
local ar_37_3 =0
local ar_58_3=0



}
display "SUMA DE TODOS LOS AR VENTANA_3"
display `sumatoria_eventos_tao3'
display ""



***********TAO_4**************************************************************
local sumatoria_eventos_tao4=0
foreach x of local fechas {
local q = `q' + 1
local sumatoria_eventos_tao4= `sumatoria_eventos_tao4' + `ar_`q'_4'


display "ar_`q'_4"
display `ar_`q'_4'
local ar_28_4=0
local ar_33_4=0

}
display "SUMA DE TODOS LOS AR VENTANA_4"
display `sumatoria_eventos_tao4'
display ""


*****************CAR REGRESIONES AZULES************************************
local CAR =0
foreach a of numlist 0/4{
local CAR = `CAR' +  `sumatoria_eventos_tao`a''
*display "CAR ESTIMADO"
*display `sumatoria_eventos_tao`a''



}

display `CAR'/71
display ""



macro drop _all



