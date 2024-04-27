library(tidyverse)
library(rio)
library(estadistica)

aa.uni <- import("./R/aa2.xlsx")
aa.nouni <- import("./R/aa1.xlsx")



media.nueva(aa.uni)
media.nueva(aa.uni,c(1,3,4))
aa.uni |>
  group_by(grupo) |>
  media.nueva(c(1,3,4))

aa.uni |>  media.nueva()

media.nueva(aa.uni$x)
media.nueva(salarios2018$SALARIO.BRUTO.ANUAL,salarios2018$FACTOR.ELEVACION)
salarios2018 |> media.nueva("SALARIO.BRUTO.ANUAL","FACTOR.ELEVACION")

# aquí da un error:
aa.uni |>
  group_by(grupo) |>
  media.nueva()


# ahora con pesos
media.nueva(aa.nouni,c(1,3,4),5)

aa.nouni |>
  group_by(grupo) |>
  media.nueva(c(1,3,4),5)

aa.nouni |>  media.nueva(pesos=5)

############### moda

#install.packages("poliscidata")
library(poliscidata)
moda.nueva(salarios2018,"SALARIO.BRUTO.ANUAL")
wtd.mode(salarios2018$SALARIO.BRUTO.ANUAL)

moda.nueva(salarios2018,"SALARIO.BRUTO.ANUAL","FACTOR.ELEVACION")
wtd.mode(salarios2018$SALARIO.BRUTO.ANUAL,salarios2018$FACTOR.ELEVACION)

moda.nueva(salarios2018$SALARIO.BRUTO.ANUAL)
moda.nueva(salarios2018$SALARIO.BRUTO.ANUAL,salarios2018$FACTOR.ELEVACION)

############# MEDIANA
mediana.nueva(salarios2018,"SALARIO.BRUTO.ANUAL","FACTOR.ELEVACION")

salarios2018 |>
  group_by(SEXO) |>
  mediana.nueva("SALARIO.BRUTO.ANUAL","FACTOR.ELEVACION")

mediana.nueva(salarios2018$SALARIO.BRUTO.ANUAL,salarios2018$FACTOR.ELEVACION)


############## VARIANZA ###########

# varianza muestral unitarias
varianza.nueva(aa.uni)
apply(aa.uni,2,var)*(15/16)

# varianza cuasi unitaria
varianza.nueva(aa.uni,c("x","y","z"),tipo="cuasi")
apply(aa.uni,2,var)

# varianza muestral no unitarias
varianza.nueva(aa.nouni,pesos=5)
Hmisc::wtd.var(aa.nouni$X,weights = aa.nouni$n,method="ML")

# varianza cuasi no unitarias
varianza.nueva(aa.nouni,c("X","Y","Z"),pesos=5,tipo="cuasi")
Hmisc::wtd.var(aa.nouni$X,weights = aa.nouni$n,method="unbiased")
Hmisc::wtd.var(aa.nouni$Y,weights = aa.nouni$n,method="unbiased")
Hmisc::wtd.var(aa.nouni$Z,weights = aa.nouni$n,method="unbiased")

#esto da un error
aa.uni |>
  group_by(grupo) |>
  varianza.nueva()

aa.uni |>
  group_by(grupo) |>
  varianza.nueva(c("x","y","z"),tipo="cuasi")

aa.uni |>
  group_by(grupo) |>
  summarize(var(x))

aa.uni |>
  group_by(grupo) |>
  varianza.nueva(c("x","y","z"))

aa.uni |>
  group_by(grupo) |>
  summarize(var(x)*(n()-1)/n())

# con posiciones da un warning
aa.uni |>
  group_by(grupo) |>
  varianza.nueva(c(1,3,4))

aa.nouni |>
  group_by(grupo) |>
  varianza.nueva(c("X","Y","Z"),pesos=5)

# con posiciones da un warning
aa.nouni |>
  group_by(grupo) |>
  varianza.nueva(c(1,3,4),pesos="n")

varianza.nueva(aa.uni$x)
varianza.nueva(aa.nouni$X,aa.nouni$n)
varianza.nueva(aa.nouni$X,aa.nouni$n,aa.nouni$Z)

###### desviacion

# DESVIACION muestral unitarias
desviacion.nueva(aa.uni)
apply(aa.uni,2,sd)*sqrt((15/16))

# DESVIACION cuasi unitaria
desviacion.nueva(aa.uni,c("x","y","z"),tipo="cuasi")
apply(aa.uni,2,sd)

# DESVIACION muestral no unitarias
desviacion.nueva(aa.nouni,pesos=5)
Hmisc::wtd.var(aa.nouni$X,weights = aa.nouni$n,method="ML")^.5

# DESVIACION cuasi no unitarias
desviacion.nueva(aa.nouni,c("X","Y","Z"),pesos=5,tipo="cuasi")
Hmisc::wtd.var(aa.nouni$X,weights = aa.nouni$n,method="unbiased")^.5
Hmisc::wtd.var(aa.nouni$Y,weights = aa.nouni$n,method="unbiased")^.5
Hmisc::wtd.var(aa.nouni$Z,weights = aa.nouni$n,method="unbiased")^.5

#esto da un error
aa.uni |>
  group_by(grupo) |>
  desviacion.nueva()

aa.uni |>
  group_by(grupo) |>
  desviacion.nueva(c("x","y","z"),tipo="cuasi")

aa.uni |>
  group_by(grupo) |>
  summarize(sd(x))

aa.uni |>
  group_by(grupo) |>
  desviacion.nueva(c("x","y","z"))

aa.uni |>
  group_by(grupo) |>
  summarize(sd(x)*sqrt((n()-1)/n()))

# con posiciones da un warning
aa.uni |>
  group_by(grupo) |>
  desviacion.nueva(c(1,3,4))

aa.nouni |>
  group_by(grupo) |>
  desviacion.nueva(c("X","Y","Z"),pesos=5)

# con posiciones da un warning
aa.nouni |>
  group_by(grupo) |>
  desviacion.nueva(c(1,3,4),pesos="n")

desviacion.nueva(salarios2018)
desviacion.nueva(salarios2018$SALARIO.BRUTO.ANUAL)
desviacion.nueva(salarios2018$SALARIO.BRUTO.ANUAL,salarios2018$FACTOR.ELEVACION)
Hmisc::wtd.var(salarios2018$SALARIO.BRUTO.ANUAL,weights = salarios2018$FACTOR.ELEVACION,method="ML")^.5


desviacion.nueva(salarios2018$SALARIO.BRUTO.ANUAL,salarios2018$FACTOR.ELEVACION,tip="cuasi")
Hmisc::wtd.var(salarios2018$SALARIO.BRUTO.ANUAL,weights = salarios2018$FACTOR.ELEVACION,method="unbiased")^.5

#################### coeficiente variacion
#install.packages("Weighted.Desc.Stat")
library(Weighted.Desc.Stat)

coeficiente.variacion.nuevo(aa.uni)

coeficiente.variacion.nuevo(aa.uni,c(1,3,4))

aa.uni |>
  group_by(grupo) |>
  coeficiente.variacion.nuevo(c(1,3,4))

aa.uni |>  coeficiente.variacion.nuevo()

coeficiente.variacion.nuevo(aa.uni$x)
aa.nouni |> coeficiente.variacion.nuevo("X","n")
coeficiente.variacion.nuevo(aa.nouni$X,aa.nouni$n)

w.cv(aa.nouni$X,aa.nouni$n)
sd.w <- desviacion.nueva(aa.uni$x)
m.w <- media.nueva(aa.uni$x)
sd.w/m.w

aa.uni |>
  group_by(grupo) |>
  coeficiente.variacion.nuevo()


# ahora con pesos
coeficiente.variacion.nuevo(aa.nouni,c(1,3,4),5)

aa.nouni |>
  group_by(grupo) |>
  coeficiente.variacion.nuevo(c(1,3,4),5)

aa.nouni |>  coeficiente.variacion.nuevo(pesos=5)


coeficiente.variacion.nuevo(aa.uni,tipo="cuasi")

coeficiente.variacion.nuevo(aa.uni,c(1,3,4),tipo="cuasi")

aa.uni |>
  group_by(grupo) |>
  coeficiente.variacion.nuevo(c(1,3,4),tipo="cuasi")

aa.uni |>  coeficiente.variacion.nuevo(tipo="cuasi")

coeficiente.variacion.nuevo(aa.uni$x,tipo="cuasi")
aa.nouni |> coeficiente.variacion.nuevo("X","n",tipo="cuasi") # no da lo mismo
coeficiente.variacion.nuevo(aa.nouni$X,aa.nouni$n,tipo="cuasi")  # no da lo mismo

sd.w <- desviacion.nueva(aa.uni$x,tipo="cuasi")
m.w <- media.nueva(aa.uni$x)
sd.w/m.w


sd.w <- desviacion.nueva(aa.nouni$X,aa.nouni$n,tipo="cuasi")
m.w <- media.nueva(aa.nouni$X,aa.nouni$n) # este resultado no es correcto
sd.w/m.w


aa.uni |>
  group_by(grupo) |>
  coeficiente.variacion.nuevo()


# ahora con pesos
coeficiente.variacion.nuevo(aa.nouni,c(1,3,4),5)

aa.nouni |>
  group_by(grupo) |>
  coeficiente.variacion.nuevo(c(1,3,4),5)

aa.nouni |>  coeficiente.variacion.nuevo(pesos=5)
