#Eric Manuel Navarro Martínez 	A01746219
#Laura Andrea Quiroz Ortega      A01026486
#Esteban Valle Sánchez      A01747146
#Análisis y comparación de las secuencias de nucleotidos del SARS-CoV-2 de diferentes especies
#Editado después de cali

dividir_vector <- function(sec)
{
  sec_cont <- gsub("[\r\n]", "", sec)
  sec_split <- c(strsplit(sec_cont, split = ""))
  return(sec_split[[1]])
}


calcular_longitud <- function(sec) 
{
  lengthsec <- length(sec)
  return(lengthsec)
}


calcular_porcentaje_l <- function(sec, l)
{
  vec_l <- list()
  for (a in sec)
  {
    if (a == l)
    {
      vec_l = c(vec_l, a)
    }
  }
  promedio_l <- calcular_longitud(vec_l)*100/length(sec)
  return(promedio_l)
  
}

hacer_dataframe <- function(sec1, sec2, sec3, sec4, sec5, sec6, sec7, sec8, sec9, sec10)
{
  df <- data.frame(Huesped = rep(c("Humano", "Perro", "León", "Venado", "Roedor", "Leopardo", "Tigre", "Visón", "Hurón", "Gato"), each = 4),
                   Nucleotidos = rep(c('A', 'T', 'G', 'C'), times = 10),
                   Porcentajes = c(calcular_porcentaje_l(sec1, "A"), calcular_porcentaje_l(sec1, "T"), calcular_porcentaje_l(sec1, "G"), calcular_porcentaje_l(sec1, "C"),
                                   calcular_porcentaje_l(sec2, "A"), calcular_porcentaje_l(sec2, "T"), calcular_porcentaje_l(sec2, "G"), calcular_porcentaje_l(sec2, "C"),
                                   calcular_porcentaje_l(sec3, "A"), calcular_porcentaje_l(sec3, "T"), calcular_porcentaje_l(sec3, "G"), calcular_porcentaje_l(sec3, "C"),
                                   calcular_porcentaje_l(sec4, "A"), calcular_porcentaje_l(sec4, "T"), calcular_porcentaje_l(sec4, "G"), calcular_porcentaje_l(sec4, "C"),
                                   calcular_porcentaje_l(sec5, "A"), calcular_porcentaje_l(sec5, "T"), calcular_porcentaje_l(sec5, "G"), calcular_porcentaje_l(sec5, "C"),
                                   calcular_porcentaje_l(sec6, "A"), calcular_porcentaje_l(sec6, "T"), calcular_porcentaje_l(sec6, "G"), calcular_porcentaje_l(sec6, "C"),
                                   calcular_porcentaje_l(sec7, "A"), calcular_porcentaje_l(sec7, "T"), calcular_porcentaje_l(sec7, "G"), calcular_porcentaje_l(sec7, "C"),
                                   calcular_porcentaje_l(sec8, "A"), calcular_porcentaje_l(sec8, "T"), calcular_porcentaje_l(sec8, "G"), calcular_porcentaje_l(sec8, "C"),
                                   calcular_porcentaje_l(sec9, "A"), calcular_porcentaje_l(sec9, "T"), calcular_porcentaje_l(sec9, "G"), calcular_porcentaje_l(sec9, "C"),
                                   calcular_porcentaje_l(sec10, "A"), calcular_porcentaje_l(sec10, "T"), calcular_porcentaje_l(sec10, "G"), calcular_porcentaje_l(sec10, "C"))
  )
  df                 
  
}

hacer_grafica_grouped_bars_v <- function(datafr)
{
  library(ggplot2)
  
  ggplot(datafr, aes(fill = Nucleotidos, y = Porcentajes, x = Huesped)) + 
    geom_bar(position='dodge', stat='identity')
}


hacer_grafica_grouped_bars_n <- function(datafr)
{
  library(ggplot2)
  
  ggplot(datafr, aes(fill = Huesped, y = Porcentajes, x = Nucleotidos)) + 
    geom_bar(position='dodge', stat='identity')
}


comparar_secuencias <- function(vec1, vec2, nombre)
{
  longitud <- 0
  longitud_vector <- length(vec1)
  if (longitud_vector > length(vec2))
  { 
    for (i in 1:length(vec2))
    {
      if (vec1[i] == vec2[i])
      {
        longitud = longitud + 1
      }
    }
    porcentaje_similitud = longitud*100/calcular_longitud(vec2)
    sec_porc = c(nombre, porcentaje_similitud)
  }
  else 
  {
    for (i in 1:longitud_vector)
    {
      if (vec1[i] == vec2[i])
      {
        longitud = longitud + 1
      }
    }
    porcentaje_similitud = longitud*100/calcular_longitud(vec1)
    sec_porc = c(nombre, porcentaje_similitud)
  }
  return(sec_porc)
}

ordenar_porcentajes <- function(lista_porcentajes)
{
  porcent_ordenados = sort(lista_porcentajes, decreasing = TRUE)
  return(porcent_ordenados)
}

ordenar_secuencias <- function(lista_buena, lista)
{
  for (i in 1:length(lista))
  {
    if (lista[i] == lista_buena[1])
    {
      primero = c(lista[i-1], lista[i])
    }
    else if (lista[i] == lista_buena[2])
    {
      segundo = c(lista[i-1], lista[i])
    }
    else if (lista[i] == lista_buena[3])
    {
      tercero = c(lista[i-1], lista[i])
    }
    else if (lista[i] == lista_buena[4])
    {
      cuarto = c(lista[i-1], lista[i])
    }
    else if (lista[i] == lista_buena[5])
    {
      quinto = c(lista[i-1], lista[i])
    }
    else if (lista[i] == lista_buena[6])
    {
      sexto = c(lista[i-1], lista[i])
    }
    else if (lista[i] == lista_buena[7])
    {
      septimo = c(lista[i-1], lista[i])
    }
    else if (lista[i] == lista_buena[8])
    {
      octavo = c(lista[i-1], lista[i])
    }
    else if (lista[i] == lista_buena[9])
    {
      noveno = c(lista[i-1], lista[i])
    }
    else if (lista[i] == lista_buena[10])
    {
      decimo = c(lista[i-1], lista[i])
    }
  }
  sec_ordenada <- c(primero, segundo, tercero, cuarto, quinto, sexto, septimo, octavo, noveno, decimo)
  return(sec_ordenada)
}

ordenar_secuencia <- function(lista)
{
  
}

human <- c("Humano", "ATGGAGAGCCTTGTCCCTGGTTTCAACGAGAAAACACACGTCCAACTCAGTTTGCCTGTTTTACAGGTTC")
dog <- c("Perro", "ATGGAGAGCCTTGTCCCTGGTTTCAACGAGAAAACACACGTCCAACTCAGTTTGCCTGTTTTACAGGTTC
GCGACGTGCTCGTAC")
lion <- c("León", "ATGGAGAGCCTTGTCCCTGGTTTCAACGAGAAAACACACGTCCA")
dear <- c("Venado", "AGATTGCTGATTATAATTATAAATTANCAGATGATTTTACAGGCTGCGTTATAGCTTGGAATTCTAACAA
TCTTGATTCTAAGGTTGGT")
rodent <- c("Roedor", "ATGGAGAGCCTTGTCCCTGGTTTCAACGAGAAAACACACGTC")
leopard <- c("Leopardo", "ATGGAGAGCCTTGTCCCTGGTTTCAACGAGAAAACACACGTCCAACTCAGTTTGCCTGTTTTACAGGTTC
GCGACGTGCTCGTACGTGGCTTTGGAGACTCCGTGGAGGAGGTCT")
tiger <- c("Tigre", "AAAAGAAGGTCAAATCAATGATATGATTTTATCTCTTCTTAGTAAAGGTAGACTTATAATTAGAGAAAAC
AACAGAGTTGTTATTTCTAGTGATGTTCTTGTTAACAACTAA")
mink <- c("Visón", "ATGGAGAGCCTTGTCCCTGGTTTCAACGAGAAAACACAC")
ferret <- c("Hurón", "ATGGAGAGCCTTGTCCCTGGTTTCAACGAGAAAACACACGTCCAACT")
cat <- c("Gato", "ATGGAGAGCCTTGTCCCTGGTTTCAACGAGAAAACACACGTCCAACTCAGTTTGCCTGTTTTACAGGTTC
GCGACGTGCTCGTACGTGGCTTTGGAGACTCCGTGGAGGAGGTCTTATCAGAGGCACGTCA")

humans <- dividir_vector(human[2])
dogs <- dividir_vector(dog[2])
lions <- dividir_vector(lion[2])
dears <- dividir_vector(dear[2])
rodents <- dividir_vector(rodent[2])
leopards <- dividir_vector(leopard[2])
tigers <- dividir_vector(tiger[2])
minks <- dividir_vector(mink[2])
ferrets <- dividir_vector(ferret[2])
cats <- dividir_vector(cat[2])

cat("Longitud de la secuencia del virus de humano:", calcular_longitud(humans), "\n")
cat("Longitud de la secuencia del virus de perro:", calcular_longitud(dogs), "\n")
cat("Longitud de la secuencia del virus de león:", calcular_longitud(lions), "\n")
cat("Longitud de la secuencia del virus de venado:", calcular_longitud(dears), "\n")
cat("Longitud de la secuencia del virus de roedor:", calcular_longitud(rodents), "\n")
cat("Longitud de la secuencia del virus de leopardo:", calcular_longitud(leopards), "\n")
cat("Longitud de la secuencia del virus de tigre:", calcular_longitud(tigers), "\n")
cat("Longitud de la secuencia del virus de visón:", calcular_longitud(minks), "\n")
cat("Longitud de la secuencia del virus de hurón:", calcular_longitud(ferrets), "\n")
cat("Longitud de la secuencia del virus de gato:", calcular_longitud(cats), "\n")


data_fr = hacer_dataframe(humans, dogs, lions, dears, rodents, leopards, tigers, minks, ferrets, cats)
hacer_grafica_grouped_bars_v(data_fr)
hacer_grafica_grouped_bars_n(data_fr)


hu <- dividir_vector(human[2])
nombre_hu <- human[1]
do <- dividir_vector(dog[2])
nombre_do <- dog[1]
li <- dividir_vector(lion[2])
nombre_li <- lion[1]
de <- dividir_vector(dear[2])
nombre_de <- dear[1]
ro <- dividir_vector(rodent[2])
nombre_ro <- rodent[1]
le <- dividir_vector(leopard[2])
nombre_le <- leopard[1]
ti <- dividir_vector(tiger[2])
nombre_ti <- tiger[1]
mi <- dividir_vector(mink[2])
nombre_mi <- mink[1]
fe <- dividir_vector(ferret[2])
nombre_fe <- ferret[1]
ca <- dividir_vector(cat[2])
nombre_ca <- cat[1]

simil_huxhu <- comparar_secuencias(hu, hu, nombre_hu)
simil_huxdo <- comparar_secuencias(hu, do, nombre_do)
simil_huxli <- comparar_secuencias(hu, li, nombre_li)
simil_huxde <- comparar_secuencias(hu, de, nombre_de)
simil_huxro <- comparar_secuencias(hu, ro, nombre_ro)
simil_huxle <- comparar_secuencias(hu, le, nombre_le)
simil_huxti <- comparar_secuencias(hu, ti, nombre_ti)
simil_huxmi <- comparar_secuencias(hu, mi, nombre_mi)
simil_huxfe <- comparar_secuencias(hu, fe, nombre_fe)
simil_huxca <- comparar_secuencias(hu, ca, nombre_ca)


porcentajes = c(as.numeric(simil_huxhu[2]), as.numeric(simil_huxdo[2]), as.numeric(simil_huxli[2]), as.numeric(simil_huxde[2]), as.numeric(simil_huxro[2]), as.numeric(simil_huxle[2]), as.numeric(simil_huxti[2]), as.numeric(simil_huxmi[2]), as.numeric(simil_huxfe[2]), as.numeric(simil_huxca[2]))
porc_ord <- ordenar_porcentajes(porcentajes)

sec_nom_porcent = c(simil_huxhu, simil_huxdo, simil_huxli, simil_huxde, simil_huxro, simil_huxle, simil_huxti, simil_huxmi, simil_huxfe, simil_huxca)

ordenada <- ordenar_secuencias(porc_ord, sec_nom_porcent)

cat("Porcentaje de similitud con la secuencia del virus de humano \n")
cat(ordenada[1], ordenada[2], "\n")
cat(ordenada[3], ordenada[4], "\n")
cat(ordenada[5], ordenada[6], "\n")
cat(ordenada[7], ordenada[8], "\n")
cat(ordenada[9], ordenada[10], "\n")
cat(ordenada[11], ordenada[12], "\n")
cat(ordenada[13], ordenada[14], "\n")
cat(ordenada[15], ordenada[16], "\n")
cat(ordenada[17], ordenada[18], "\n")
cat(ordenada[19], ordenada[20], "\n")


#Referencias
#Human (OM570283.1) : NCBI. (2022). Severe acute respiratory syndrome coronavirus 2 isolate SARS-CoV-2/human/BGD/TND-04-0748/2022 ORF1ab polyprotein (ORF1ab), ORF1a polyprotein (ORF1ab), surface glycoprotein (S), ORF3a protein (ORF3a), envelope protein (E), membrane glycoprotein (M), ORF6 protei.... Recuperado de: https://www.ncbi.nlm.nih.gov/nuccore/OM570283.1
#Dog (OK539641.1): NCBI. (2021). Severe acute respiratory syndrome coronavirus 2 isolate SARS-CoV-2/Canis lupus familiaris/THA/CU27791/2021 ORF1ab polyprotein (ORF1ab), ORF1a polyprotein (ORF1ab), surface glycoprotein (S), ORF3a protein (ORF3a), envelope protein (E), membrane glycoprotein (M)... Recuperado de: https://www.ncbi.nlm.nih.gov/nuccore/OK539641.1
#Lion (OM181956.1): NCBI. (2022). Severe acute respiratory syndrome coronavirus 2 isolate SARS-CoV-2/Panthera leo/USA/CO-CDPHE-2102014241/2021 ORF1ab polyprotein (ORF1ab), ORF1a polyprotein (ORF1ab), surface glycoprotein (S), ORF3a protein (ORF3a), envelope protein (E), membrane glycoprotein (.... Recuperado de: https://www.ncbi.nlm.nih.gov/nuccore/OM181956.1
#Dear (ON350846.1): NCBI. (2022). Severe acute respiratory syndrome coronavirus 2 isolate SARS-CoV-2/Odocoileus virginianus/USA/VSP3614/2021 surface glycoprotein (S) gene, partial cds Recuperado de: https://www.ncbi.nlm.nih.gov/nuccore/ON350846.1
#Rodent (OL913103.2): NCBI. (2021). Severe acute respiratory syndrome coronavirus 2 isolate SARS-CoV-2/Rodent/CHN/BMA8/2020, complete genome. Recuperado de: https://www.ncbi.nlm.nih.gov/nuccore/OL913103.2
#Leopard (OL752440.1): NCBI. (2021). Severe acute respiratory syndrome coronavirus 2 isolate SARS-CoV-2/Prionailurus bengalensis euptilurus/CZE/4102-5/2021 ORF1ab polyprotein (ORF1ab), ORF1a polyprotein (ORF1ab), surface glycoprotein (S), ORF3a protein (ORF3a), envelope protein (E), membrane glyc.... Recuperado de: https://www.ncbi.nlm.nih.gov/nuccore/OL752440.1
#Tiger (MT724347.1): NCBI. (2020). Severe acute respiratory syndrome coronavirus 2 isolate SARS-CoV-2/Panthera tigris/USA/tiger-5-040420/2020 ORF1ab polyprotein, 2'-O-ribose methyltransferase region, (ORF1ab) gene, partial cds; surface glycoprotein (S) gene, complete cds; and ORF3a protein (ORF... Recuperado de: https://www.ncbi.nlm.nih.gov/nuccore/MT724347.1 
#Mink (MW626385.1): NCBI. (2021). Severe acute respiratory syndrome coronavirus 2 isolate SARS-CoV-2/mink/USA/WI-CDC-3892840-001/2020, complete genome. Recuperado de: https://www.ncbi.nlm.nih.gov/nuccore/MW626385.1
#Ferret (MZ433219.1): NCBI. (2021).Severe acute respiratory syndrome coronavirus 2 isolate SARS-CoV-2/ferret/USA/G452/2021 ORF1ab polyprotein (ORF1ab), ORF1a polyprotein (ORF1ab), surface glycoprotein (S), ORF3a protein (ORF3a), envelope protein (E), membrane glycoprotein (M), ORF6 protein (ORF....  Recuperado de: https://www.ncbi.nlm.nih.gov/nuccore/MZ433219.1 
#Cat (MW064259.1): NCBI. (2020). Severe acute respiratory syndrome coronavirus 2 isolate SARS-CoV-2/Cat/CHL/UCH_NRL_014/2020, complete genome Recuperado de: https://www.ncbi.nlm.nih.gov/nuccore/MW064259.1
