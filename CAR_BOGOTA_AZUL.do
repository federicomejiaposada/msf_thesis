***********************************************************************
************** FECHAS BOGOTA ******************************************


clear all
set more off

import excel "C:\Users\Earendil\Google Drive\01_MSc_Finance\001_Thesis_proposal\Programs\data_colombia.xlsx", sheet("returns") firstrow


format DATE %tdnn/dd/YY
rename DATE Date
tsset Date
gen index=Date
gen index_2= _n


gen indica=0


*local fechas " 3/5/2008 6/10/2008 7/31/2008 8/11/2008 8/21/2008 10/3/2008 10/23/2008 12/3/2008 12/29/2008 1/27/2009 8/12/2010 4/13/2012 5/15/2012 3/6/2015 7/2/2015 4/20/2016 "
local fechas " 3/5/2008 6/10/2008 7/31/2008 8/11/2008 8/21/2008 10/3/2008 10/23/2008 12/3/2008 12/29/2008 1/27/2009 8/12/2010 4/13/2012 5/15/2012 3/6/2015 7/2/2015 "



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
local down= r(min)-501
local up= r(min)
*display `down'
*display `up'
*reg COLCAP IBOV if (index_2 > `down'  & `up' > index_2 )
quietly arch COLCAP IBOV if (index_2 > `down'  & `up' > index_2 ) ,  arch(1) garch(1) iter(50)
matrix A =r(table)
local alp`e' = A[1,2]
display "ESTA ES LA CONSTANTE DEL MODELO DE MEDIA"
display `alp`e''
display ""
local beta`e' = A[1,1]
display "ESTE ES EL COEFICIENTE DEL MODELO DE MEDIA"
display `beta`e''
display ""

*local etha`e'= A[1,2]
*display "ESTE ES EL SEGUNDO  COEFICIENTE DEL MODELO DE MEDIA"
*display `etha`e''
*display ""

local gama_cero`e' = A[1,5]
display "ESTA ES LA CONSTANTE DEL MODELO DE VOLATILIDAD GAMMA_0"
display `gama_cero`e''
display ""
local gamma_uno`e' = A[1,3]
display "ESTE ES EL PRIMER COEFICIENTE DEL MODELO GARCH GAMMA_1"
display `gamma_uno`e''
display ""
local gamma_dos`e'= A[1,4]
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

local e_t_`e'=e(archi)
display "ESTE ES EL e2_T"
display `e_t_`e''



foreach tau of numlist 0/4{
*local ar_`e'_`tau'=COLCAP[`up'+`tau']- `alp`e''- `beta`e''*COLCAP[`up'+`tau'-1] -`etha`e''*IBOV[`up'+`tau']
local ar_`e'_`tau'=COLCAP[`up'+`tau']- `alp`e'' -`beta`e''*IBOV[`up'+`tau']

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




}
display "SUMA DE TODOS LOS AR VENTANA_0"
display `sumatoria_eventos_tao0'
display ""


***********TAO_1**************************************************************
local sumatoria_eventos_tao1=0
foreach x of local fechas {
local z = `z' + 1
local sumatoria_eventos_tao1= `sumatoria_eventos_tao1' + `ar_`z'_1'
*display "ar_`z'_1"
*display `ar_`z'_1'
local ar_16_1=0


}
display "SUMA DE TODOS LOS AR VENTANA_1"
display `sumatoria_eventos_tao1'
display ""


***********TAO_2**************************************************************
local sumatoria_eventos_tao2=0
foreach x of local fechas {
local y = `y' + 1
local sumatoria_eventos_tao2= `sumatoria_eventos_tao2' + `ar_`y'_2'




}
display "SUMA DE TODOS LOS AR VENTANA_2"
display `sumatoria_eventos_tao2'
display ""



***********TAO_3**************************************************************
local sumatoria_eventos_tao3=0
foreach x of local fechas {
local l = `l' + 1
local sumatoria_eventos_tao3= `sumatoria_eventos_tao3' + `ar_`l'_3'




}
display "SUMA DE TODOS LOS AR VENTANA_3"
display `sumatoria_eventos_tao3'
display ""



***********TAO_4**************************************************************
local sumatoria_eventos_tao4=0
foreach x of local fechas {
local q = `q' + 1
local sumatoria_eventos_tao4= `sumatoria_eventos_tao4' + `ar_`q'_4'




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

display `CAR'/16
display ""



macro drop _all



