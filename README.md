---
title: "Dia_69_24mayo"
author: "Daniel Felipe Villa Rengifo"
date: "27/5/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, eval=FALSE, include=TRUE}
"Protocolo:

1. Daniel Felipe Villa Regifo

2. Lenguaje: R

3. Tema: Cree funciones que manejen factores en R  (realice al menos dos ejercicios que requieran cargar archivos externos *.csv cada uno con al menos 50 filas y tres datos por fila, lea y procese la información del archvo leído, y guarde las respuestas a los ejercicios  en archivos independientes tipo *.txt)

4. Fuentes:
   https://www.generatedata.com"
```


# Ejercicio 1

```{r eval=FALSE, message=FALSE, warning=FALSE, include=FALSE}
#install.packages(tidyverse)<- Ejecutar de ser necesario}
library(tidyverse)
library(dplyr)
```

```{r}
# Iportamos la base
base = read.csv("Base1.csv", header = T, sep = ",")
```

```{r}
#Convertir las variables a tipo factor
names <- c(2:3)
base[,names] <- lapply(base[,names] , factor)
```


```{r}
# Funcion que me da una tabla de frecuencias de uan variable tipo factor 
f = function(data, x){
  #guardo la tabla en una variable j
  frecuencia = data %>% 
    # agrupo los niveles de la variable tipo factor y les hago un conteo y lo asocio una nueva variable 
    # llamada cantidad
    group_by(data[x]) %>% 
    summarise(cantida = n())
  #retorno la tabal de frecuencias 
  return(frecuencia)
}
  
```

```{r}
# indico la variable  de la cual quiero la tabal de frecuencias  
frecuencia <- f(base, 2)

write.table(frecuencia, file="Nombre.txt", row.names = F)
```

# ejercicio 2

```{r}
base2 <- read.csv(file = "BaseFactor.csv", sep = ",", header = T)
print(base2)
```


```{r}
# Comvertir las variables a tipo factor
names <- c(2:3)
base2[,names] <- lapply(base2[,names] , factor)
str(base2)
```


```{r}
# Verifica si uan variable es tipo factor y si lo es imprime los niveles de la funcion 
factorIS <- function(x){
  "Verifica si uan variable es tipo factor y si lo es imprime los niveles de la funcion "
  if(is.factor(x) == TRUE){
    v <- as.vector(levels(x))
    df <- data.frame("Niveles (Modelos de Autos)"= v)
    return(df)
  }
}

```

```{r}
factorIS(base2$Modelo)

write.table(factorIS(base2$Modelo), file="Nombre2.txt", row.names = F)
```