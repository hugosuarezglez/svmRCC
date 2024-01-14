modelo_SVM <- function(data, data_labels) {
ruta <- system.file('extdata', package = 'svmRCC')

#### modelo


# modelo <- readRDS(file=paste0(ruta,"/modelo_SVM_843.Rda"))
# maximo <- readRDS(file=paste0(ruta,"/maximo_train.Rda"))
# minimo <- readRDS(file=paste0(ruta,"/minimo_train.Rda"))

#########################################
modelo <- readRDS(system.file("extdata", "modelo_SVM_843.Rda", package = "svmRCC"))
maximo <- readRDS(system.file("extdata", "maximo_train.Rda", package = "svmRCC"))
minimo <- readRDS(system.file("extdata", "minimo_train.Rda", package = "svmRCC"))
genes_modelo <- readRDS(system.file("extdata", "genes_modelo_SVM.Rda", package = "svmRCC"))


## seleccionar datos
# genes_modelo <- readRDS(file=paste0(ruta,"/genes_modelo_SVM.Rda"))

data <- data[,genes_modelo]


### Normalizacion
data <- log2(data[,1:(ncol(data))]+1)


# Función para normalizar
normalize <- function(x) {
  return((x - minimo) / (maximo - minimo))
}



# Aplicamos la función
data <- as.data.frame(lapply(data, normalize))


if (prop.table(table(is.na(data)))['FALSE']!=1){
  return('Inserta datos sin valores faltantes')
}else {


predict <- predict(modelo, data)
results <- data.frame('Etiqueta'=character(), 'Tumor'=character())

for (i in 1:length(predict)) {
results <- rbind(results, data.frame(Etiqueta=data_labels[i,1],
                 Tumor=as.character(predict[i])))
  #print('Los resultados de las muestras son: ')
  #print(results)

}
return(results)
}
}
