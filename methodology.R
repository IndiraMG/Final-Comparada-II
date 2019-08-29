install.packages("picante")
install.packages("ape")
library(ape)
library(picante)

## SIMULACION DE ARBOLES

listado<-list() ##List vacio para guardar todos los arboles simulados


#bucles para generar arboles de 5, 25 y 125 terminales con distrib uniforme, exponencial y 
#lognormal, cada uno con 30 replicas

#################### Uniforme  ########################################

### Arboles con 5 terminales y ramas con distribucion uniforme

brlength_unif5<- runif(8, min=1, max = 10 ) ## se generan los valores para las ramas de la topologia con una distribucion uniforme, el numero de ramas es el doble de los nodos internos

## bucle para generar las 30 replicas
for (replic in 1:30) {
  
  listado[[replic]]<-rtree(5,rooted =T, 
                      tip.label=paste("U",replic,"-",5,"-","T", 1:5, sep = ""),  ## etiqueta de las termunales: U (Uniforme), replic(replica), 5 (numero de temrinales), T(numero de terminal)
                      br= brlength_unif5)
  
  ## etiquetas para arboles en el listado
  names(listado)[replic]<-paste("U",replic,"-",5, sep = "")
}

### Arboles con 25 terminales y ramas con distribucion uniforme

brlength_unif25<- runif(48, min=1, max = 10 )

for (replic in 1:30) {
  
  listado[[replic+30]]<-rtree(25,rooted =T, 
                      tip.label=paste("U",replic,"-",25,"-","T", 1:25, sep = ""), 
                      br= brlength_unif25)
  
  names(listado)[replic+30]<-paste("U",replic,"-",25, sep = "")
}

### Arboles con 125 terminales y ramas con distribucion uniforme

brlength_unif125<- runif(248, min=1, max = 10 )

for (replic in 1:30) {
  
  listado[[replic+60]]<-rtree(125,rooted =T, 
                         tip.label=paste("U",replic,"-",125,"-","T", 1:125, sep = ""), 
                         br= brlength_unif125)
  
  names(listado)[replic+60]<-paste("U",replic,"-",125, sep = "")
}


############### Exponencial  ##################################

### Arboles con 5 terminales y ramas con distribucion exponencial

brlength_exp5<- rexp(8)

for (replic in 1:30) {
  
  listado[[replic+90]]<-rtree(5,rooted =T, 
                      tip.label=paste("E",replic,"-",5,"-","T", 1:5, sep = ""), 
                      br= brlength_exp5)
  
  names(listado)[replic+90]<-paste("E",replic,"-",5, sep = "")
}

### Arboles con 25 terminales y ramas con distribucion exponencial

brlength_exp25<- rexp(48)

for (replic in 1:30) {
  
  listado[[replic+120]]<-rtree(25,rooted =T, 
                         tip.label=paste("E",replic,"-",25,"-","T", 1:25, sep = ""), 
                         br= brlength_exp25)
  
  names(listado)[replic+120]<-paste("E",replic,"-",25, sep = "")
}

### Arboles con 125 terminales y ramas con distribucion exponencial

brlength_exp125<- rexp(248)

for (replic in 1:30) {
  
  listado[[replic+150]]<-rtree(125,rooted =T, 
                         tip.label=paste("E",replic,"-",125,"-","T", 1:125, sep = ""), 
                         br= brlength_exp125)
  
  names(listado)[replic+150]<-paste("E",replic,"-",125, sep = "")
}


############### Log-normal  ##################################

### Arboles con 5 terminales y ramas con distribucion Log-normal

brlength_ln5<- rlnorm(8)

for (replic in 1:30) {
  
  listado[[replic+180]]<-rtree(5,rooted =T, 
                         tip.label=paste("LogN",replic,"-",5,"-","T", 1:5, sep = ""), 
                         br= brlength_ln5)
  
  names(listado)[replic+180]<-paste("LogN",replic,"-",5, sep = "")
}

### Arboles con 25 terminales y ramas con distribucion log-normal

brlength_ln25<- rlnorm(48)

for (replic in 1:30) {
  
  listado[[replic+210]]<-rtree(25,rooted =T, 
                          tip.label=paste("LogN",replic,"-",25,"-","T", 1:25, sep = ""), 
                          br= brlength_ln25)
  
  names(listado)[replic+210]<-paste("LogN",replic,"-",25, sep = "")
}

### Arboles con 125 terminales y ramas con distribucion exponencial

brlength_ln125<- rexp(248)

for (replic in 1:30) {
  
  listado[[replic+240]]<-rtree(125,rooted =T, 
                          tip.label=paste("LogN",replic,"-",125,"-","T", 1:125, sep = ""), 
                          br= brlength_ln125)
  
  names(listado)[replic+240]<-paste("LogN",replic,"-",125, sep = "")
}

## DISTRIBUCIONES Y COORDENADAS 

#Tabla de coordenadas de 1000 puntos con dist. uniforme
x<-runif(10^3,min=0, max=1)
y<-runif(10^3, min=0, max=1)
coordenate<-data.frame(x,y)

##funcion para crear cuadriculas
grid <- function(size_grid_x, size_grid_y, Length_x, Length_y) {

##size_grid_x = tamaño de la cuadricula en el eje x
##size_grid_y = tamaño de la cuadricula en el eje y
##Length_x = Longitud del area total en el eje x
##Length_y = Longitud del area total en el eje y
  
  limit_inf_x<-rep(seq(0,(Length_x-size_grid_x),size_grid_x),each=(Length_x/size_grid_x))
  limit_sup_x<-rep(seq((0+size_grid_x),Length_x,size_grid_x),each=(Length_x/size_grid_x))
  limit_inf_y<-rep(seq(0,(Length_y-size_grid_y),size_grid_y),times=(Length_y/size_grid_y))
  limit_sup_y<-rep(seq((0+size_grid_y),Length_y,size_grid_y),times=(Length_y/size_grid_y))
  
  data.frame(id=1:((Length_x/size_grid_x)*(Length_y/size_grid_y)),limit_inf_x,limit_sup_x,limit_inf_y,limit_sup_y)
## EL data frame almacena los limites tanto en x como en y de cada cuadricula y le asigna un id para cada una
}

area05<-grid(0.5,0.5,1,1) #cuadricua con areas de 0.5x0.5
area0125<-grid(0.125,0.125,1,1) #cuadricua con areas de 0.125x0.125
area003125<-grid(0.03125,0.03125,1,1) #cuadricua con areas de 0.03125x0.03125

list_coord<-list()
distribution<- rep(c("U","E","LogN"),each=3)
n_terminal<- rep(c(5,25,125),3)

##este bucle crea las tablas de coordenadas para cada grupo de topologias en funcion de la distribucion y el numero de terminales
for (replic in 1:9) {
labels<-paste(distribution[replic],"-",n_terminal[replic],"-","T",1:n_terminal[replic], sep = "")

list_coord[[replic]]<- data.frame(Terminal=sample(labels, nrow(coordenate), replace = T),
                      coordenate_x= coordenate$x,
                      coordenate_y= coordenate$y,
                      area_05=rep(NA,nrow(coordenate)),
                      area_0125=rep(NA,nrow(coordenate)),
                      area_003125=rep(NA,nrow(coordenate)))
}

names(list_coord)<-paste(rep(c("U","E","LogN"),each=3), c(5,25,125), sep="-")

##bucle para determinar en que cuadricula se encuentra cada punto de las tablas anterores para la las cuadriculas de 0.5 x 0.5

for (k in 1:9) {
  for (i in 1: nrow(list_coord[[k]])) {
    for (j in 1:nrow(area05)) {
      if(area05[j,2]<list_coord[[k]][i,2]&&
         list_coord[[k]][i,2]<=area05[j,3]&&
         area05[j,4]<list_coord[[k]][i,3]&&
         list_coord[[k]][i,3]<=area05[j,5]) 
      
      {list_coord[[k]][i,4]=area05[j,1]
        finish<-TRUE}
##una vez se encuentre la cuadricula a la que corresponde el punto, se para la comparacion de las cuadriculas y se pasa a la siguiente coordenada
      if (finish==TRUE) 
        {finish=FALSE
         break }
      }
  }
}

##bucle para determinar en que cuadricula se encuentra cada punto de las tablas anterores para la las cuadriculas de 0.125 x 0.125

for (k in 1:9) {
  for (i in 1: nrow(list_coord[[k]])) {
    for (j in 1:nrow(area0125)) {
      if(area0125[j,2]<list_coord[[k]][i,2]&&
         list_coord[[k]][i,2]<=area0125[j,3]&&
         area0125[j,4]<list_coord[[k]][i,3]&&
         list_coord[[k]][i,3]<=area0125[j,5]) 
        
      {list_coord[[k]][i,5]=area0125[j,1]
      finish<-TRUE} 
      
      if (finish==TRUE) 
        {finish=FALSE
         break }
    }
  }
  print(k)
}


##bucle para determinar en que cuadricula se encuentra cada punto de las tablas anterores para la las cuadriculas de 0.03125 x 0.03125

for (k in 1:9) {
  for (i in 1: nrow(list_coord[[k]])) {
    for (j in 1:nrow(area003125)) {
      if(area003125[j,2]<list_coord[[k]][i,2]&&
         list_coord[[k]][i,2]<=area003125[j,3]&&
         area003125[j,4]<list_coord[[k]][i,3]&&
         list_coord[[k]][i,3]<=area003125[j,5]) 
        
      {list_coord[[k]][i,6]=area003125[j,1]
      finish<-TRUE}
      
      if (finish==TRUE) 
        {finish=FALSE
         break }
    }
  }
  print(k)
}

posicion<-seq(1,27,3) #posicion donde se van a guardar las tablas de comunidades

#bucle para crear las tablas de comunidades 
for (l in 1:9) {
  ##tabla de comunidades para la primer cuadricula
  comunidades[[posicion[l]]]<-as.data.frame.matrix(
    table(list_coord[[l]]$area_05,
          list_coord[[l]]$Terminal))
  ##reemplazo lo svalores por ausencia presencia
  comunidades[[posicion[l]]]<-replace(comunidades[[posicion[l]]],
                                      comunidades[[posicion[l]]]>=1,1)
  
  ##tabla de comunidades para la segunda cuadricula
  comunidades[[posicion[l]+1]]<-as.data.frame.matrix(
    table(list_coord[[l]]$area_0125,
          list_coord[[l]]$Terminal))
  
  comunidades[[posicion[l]+1]]<-replace(comunidades[[posicion[l]+1]],
                                      comunidades[[posicion[l]+1]]>=1,1)
  
  ##tabla de comunidades para la tercer cuadricula
  comunidades[[posicion[l]+2]]<-as.data.frame.matrix(
    table(list_coord[[l]]$area_003125,
          list_coord[[l]]$Terminal))
  
  comunidades[[posicion[l]+2]]<-replace(comunidades[[posicion[l]+2]],
                                      comunidades[[posicion[l]+2]]>=1,1)
}

#nombres para identificar las tablas en el list
names(comunidades)<-paste(rep(names(list_coord),each=3),
                  rep(c("0.5x0.5","0.125x0.125","0.03125x0.03125"),3), 
                  sep="-")


## CALCULO DE PD
#list vacio para guardar los pd
PD<-list()

#vector con las posiciones de cada tabla de comunidades, las cuales se repiten 10 veces porque son 30 PD en total, por tipo de distribucion y por numero de terminales
comunity<-c(rep(1:3,length.out=90), rep(4:6,length.out=90),
        rep(7:9,length.out=90), rep(10:12,length.out=90),
        rep(13:15,length.out=90), rep(16:18,length.out=90),
        rep(19:21,length.out=90), rep(22:24,length.out=90),
        rep(25:27,length.out=90))

##esta repeticion es para asignar a cada tabla de comunidad su respectivo arbol
tree<-rep(1:270,each=3)

##esta repeticion es para poder cambiar las etiquetas de especies de cada tabla de comunidades a medida que se va usando para pd
repeticion<-rep(rep(1:30, each=3),9)


#Bucle para calcular PD a cada tabla de comunidades y a cada topologia. Qudan en el mismo orden que estan las topologias 

##este bucle es para calcular el pd de todas las topologias con dist uniforme
for (m in 1:270) {
  tabla<-comunidades[[comunity[m]]]
  arbol<- listado[[tree[m]]]
  nombres<-colnames(tabla)
  nombres<-gsub("U",paste("U",repeticion[m],sep=""),nombres)
  colnames(tabla)<-nombres
  PD[[m]]<- pd(tabla, arbol )
  
  print(m)
}

##este bucle es para calcular el pd de todas las topologias con dist exponencial
for (m in 271:540) {
  tabla<-comunidades[[comunity[m]]]
  arbol<- listado[[tree[m]]]
  nombres<-colnames(tabla)
  nombres<-gsub("E",paste("E",repeticion[m],sep=""),nombres)
  colnames(tabla)<-nombres
  PD[[m]]<- pd(tabla, arbol )
  
  print(m)
}

##este bucle es para calcular el pd de todas las topologias con dist log-normal
for (m in 541:810) {
  tabla<-comunidades[[comunity[m]]]
  arbol<- listado[[tree[m]]]
  nombres<-colnames(tabla)
  nombres<-gsub("LogN",paste("LogN",repeticion[m],sep=""),nombres)
  colnames(tabla)<-nombres
  PD[[m]]<- pd(tabla, arbol )
  
  print(m)
}


##asigno los nombres a cada tabla de pd de acuerdo al arbol usado y el tama?o de cuadricula evaluada
names(PD)<-paste(rep(names(listado),each=3),
                 rep(c("0.5x0.5","0.125x0.125","0.03125x0.03125"),270), sep="-")

###Bucle para guardar las tablas de pd en un directorio llamado pd
for (variable in 1:length(PD)) {
  write.csv(PD[[variable]], paste("./pd/",names(PD)[variable],".csv",sep = ""))  
}


###Aqui vuelvo a cargar todas las tablas de PD que guarde, por si al cerrar rstudio se borraba el trabajo. Cada tabla tiene su nombre  de acuerdo a la distribucionde la longitud de ramas, replica, numero de terminales y tam?o de area evaluada
setwd()
temp = list.files(pattern="*.csv")
PD = lapply(temp, read.csv)
names(PD)<-temp

####Voy a unir las tablas de PD por distribucion en la longitud de rama Y numero de terminales Y tama?o de area, en total saldran 27 tablas que contendran los pd de las 30 replicas

#####Uniforme-5 terminales-0.5x0.5 (30 replicas)####
first_table<-Reduce(intersect,list(grep(pattern = "U", names(PD)), 
                      grep(pattern = "-5-", names(PD)),
                      grep(pattern = "0.5x0.5", names(PD))))

U505<-data.frame()
for (variable in 1:length(first_table)) {
  U505<-rbind(U505,PD[[first_table[variable]]], make.row.names=F)
}
U505<-cbind(dist_ramas=rep("UNIFORME",nrow(U505)), 
            terminales=rep(5,nrow(U505)), 
            area=rep(0.5*0.5,nrow(U505)), U505)

#####Uniforme-5 terminales-0.125x0.125 (30 replicas)#####
second_table<-Reduce(intersect,list(grep(pattern = "U", names(PD)), 
                                   grep(pattern = "-5-", names(PD)),
                                   grep(pattern = "0.125x0.125", 
                                        names(PD))))

U50125<-data.frame()
for (variable in 1:length(second_table)) {
  U50125<-rbind(U50125,PD[[second_table[variable]]], make.row.names=F)
}
U50125<-cbind(dist_ramas=rep("UNIFORME",nrow(U50125)), 
            terminales=rep(5,nrow(U50125)), 
            area=rep(0.125*0.125,nrow(U50125)), U50125)


#######Uniforme-5 terminales-0.03125x0.03125 (30 replicas)#######
third_table<-Reduce(intersect,list(grep(pattern = "U", names(PD)), 
                                    grep(pattern = "-5-", names(PD)),
                                    grep(pattern = "0.03125x0.03125", 
                                         names(PD))))

U5003125<-data.frame()
for (variable in 1:length(third_table)) {
  U5003125<-rbind(U5003125,PD[[third_table[variable]]], make.row.names=F)
}
U5003125<-cbind(dist_ramas=rep("UNIFORME",nrow(U5003125)), 
              terminales=rep(5,nrow(U5003125)), 
              area=rep(0.03125*0.03125,nrow(U5003125)), U5003125)

U5<-rbind(U505,U50125,make.row.names=F)
U5<-rbind(U5,U5003125,make.row.names=F)


########Uniforme-25 terminales-0.5x0.5 (30 replicas)#######
first_table<-Reduce(intersect,list(grep(pattern = "U", names(PD)), 
                                   grep(pattern = "-25-", names(PD)),
                                   grep(pattern = "0.5x0.5", names(PD))))

U2505<-data.frame()
for (variable in 1:length(first_table)) {
  U2505<-rbind(U2505,PD[[first_table[variable]]], make.row.names=F)
}
U2505<-cbind(dist_ramas=rep("UNIFORME",nrow(U2505)), 
            terminales=rep(25,nrow(U2505)), 
            area=rep(0.5*0.5,nrow(U2505)), U2505)

########Uniforme-25 terminales-0.125x0.125 (30 replicas)#######
second_table<-Reduce(intersect,list(grep(pattern = "U", names(PD)), 
                                    grep(pattern = "-25-", names(PD)),
                                    grep(pattern = "0.125x0.125", 
                                         names(PD))))

U250125<-data.frame()
for (variable in 1:length(second_table)) {
  U250125<-rbind(U250125,PD[[second_table[variable]]], make.row.names=F)
}
U250125<-cbind(dist_ramas=rep("UNIFORME",nrow(U250125)), 
              terminales=rep(25,nrow(U250125)), 
              area=rep(0.125*0.125,nrow(U250125)), U250125)


########Uniforme-25 terminales-0.03125x0.03125 (30 replicas)######
third_table<-Reduce(intersect,list(grep(pattern = "U", names(PD)), 
                                   grep(pattern = "-25-", names(PD)),
                                   grep(pattern = "0.03125x0.03125", 
                                        names(PD))))

U25003125<-data.frame()
for (variable in 1:length(third_table)) {
  U25003125<-rbind(U25003125,PD[[third_table[variable]]], 
                   make.row.names=F)
}
U25003125<-cbind(dist_ramas=rep("UNIFORME",nrow(U25003125)), 
                terminales=rep(25,nrow(U25003125)), 
                area=rep(0.03125*0.03125,nrow(U25003125)), U25003125)

U25<-rbind(U2505,U250125,make.row.names=F)
U25<-rbind(U25,U25003125,make.row.names=F)

######Uniforme-125 terminales-0.5x0.5 (30 replicas)#####
first_table<-Reduce(intersect,list(grep(pattern = "U", names(PD)), 
                                   grep(pattern = "-125-", names(PD)),
                                   grep(pattern = "0.5x0.5", names(PD))))

U12505<-data.frame()
for (variable in 1:length(first_table)) {
  U12505<-rbind(U12505,PD[[first_table[variable]]], make.row.names=F)
}
U12505<-cbind(dist_ramas=rep("UNIFORME",nrow(U12505)), 
             terminales=rep(125,nrow(U12505)), 
             area=rep(0.5*0.5,nrow(U12505)), U12505)

#######Uniforme-125 terminales-0.125x0.125 (30 replicas)######
second_table<-Reduce(intersect,list(grep(pattern = "U", names(PD)), 
                                    grep(pattern = "-125-", names(PD)),
                                    grep(pattern = "0.125x0.125", 
                                         names(PD))))

U1250125<-data.frame()
for (variable in 1:length(second_table)) {
  U1250125<-rbind(U1250125,PD[[second_table[variable]]], 
                  make.row.names=F)
}
U1250125<-cbind(dist_ramas=rep("UNIFORME",nrow(U1250125)), 
               terminales=rep(125,nrow(U1250125)), 
               area=rep(0.125*0.125,nrow(U1250125)), U1250125)


######Uniforme-125 terminales-0.03125x0.03125 (30 replicas)######
third_table<-Reduce(intersect,list(grep(pattern = "U", names(PD)), 
                                   grep(pattern = "-125-", names(PD)),
                                   grep(pattern = "0.03125x0.03125", 
                                        names(PD))))

U125003125<-data.frame()
for (variable in 1:length(third_table)) {
  U125003125<-rbind(U125003125,PD[[third_table[variable]]], 
                   make.row.names=F)
}
U125003125<-cbind(dist_ramas=rep("UNIFORME",nrow(U125003125)), 
                 terminales=rep(125,nrow(U125003125)), 
                 area=rep(0.03125*0.03125,nrow(U125003125)), U125003125)

U125<-rbind(U12505,U1250125,make.row.names=F)
U125<-rbind(U125,U125003125,make.row.names=F)

#####Exponencial-5 terminales-0.5x0.5 (30 replicas)####
first_table<-Reduce(intersect,list(grep(pattern = "E", names(PD)), 
                                   grep(pattern = "-5-", names(PD)),
                                   grep(pattern = "0.5x0.5", names(PD))))

E505<-data.frame()
for (variable in 1:length(first_table)) {
  E505<-rbind(E505,PD[[first_table[variable]]], make.row.names=F)
}
E505<-cbind(dist_ramas=rep("EXPONENCIAL",nrow(E505)), 
            terminales=rep(5,nrow(E505)), 
            area=rep(0.5*0.5,nrow(E505)), E505)

#####Exponencial-5 terminales-0.125x0.125 (30 replicas)#####
second_table<-Reduce(intersect,list(grep(pattern = "E", names(PD)), 
                                    grep(pattern = "-5-", names(PD)),
                                    grep(pattern = "0.125x0.125", 
                                         names(PD))))

E50125<-data.frame()
for (variable in 1:length(second_table)) {
  E50125<-rbind(E50125,PD[[second_table[variable]]], make.row.names=F)
}
E50125<-cbind(dist_ramas=rep("EXPONENCIAL",nrow(E50125)), 
              terminales=rep(5,nrow(E50125)), 
              area=rep(0.125*0.125,nrow(E50125)), E50125)


#######Exponencial-5 terminales-0.03125x0.03125 (30 replicas)#######
third_table<-Reduce(intersect,list(grep(pattern = "E", names(PD)), 
                                   grep(pattern = "-5-", names(PD)),
                                   grep(pattern = "0.03125x0.03125", 
                                        names(PD))))

E5003125<-data.frame()
for (variable in 1:length(third_table)) {
  E5003125<-rbind(E5003125,PD[[third_table[variable]]], 
                  make.row.names=F)
}
E5003125<-cbind(dist_ramas=rep("EXPONENCIAL",nrow(E5003125)), 
                terminales=rep(5,nrow(E5003125)), 
                area=rep(0.03125*0.03125,nrow(E5003125)), E5003125)

E5<-rbind(E505,E50125,make.row.names=F)
E5<-rbind(E5,E5003125,make.row.names=F)


########Exponencial-25 terminales-0.5x0.5 (30 replicas)#######
first_table<-Reduce(intersect,list(grep(pattern = "E", names(PD)), 
                                   grep(pattern = "-25-", names(PD)),
                                   grep(pattern = "0.5x0.5", names(PD))))

E2505<-data.frame()
for (variable in 1:length(first_table)) {
  E2505<-rbind(E2505,PD[[first_table[variable]]], make.row.names=F)
}
E2505<-cbind(dist_ramas=rep("EXPONENCIAL",nrow(E2505)), 
             terminales=rep(25,nrow(E2505)), 
             area=rep(0.5*0.5,nrow(E2505)), E2505)

########Exponencial-25 terminales-0.125x0.125 (30 replicas)#######
second_table<-Reduce(intersect,list(grep(pattern = "E", names(PD)), 
                                    grep(pattern = "-25-", names(PD)),
                                    grep(pattern = "0.125x0.125", 
                                         names(PD))))

E250125<-data.frame()
for (variable in 1:length(second_table)) {
  E250125<-rbind(E250125,PD[[second_table[variable]]], make.row.names=F)
}
E250125<-cbind(dist_ramas=rep("EXPONENCIAL",nrow(E250125)), 
               terminales=rep(25,nrow(E250125)), 
               area=rep(0.125*0.125,nrow(E250125)), E250125)


########Exponencial-25 terminales-0.03125x0.03125 (30 replicas)######
third_table<-Reduce(intersect,list(grep(pattern = "E", names(PD)), 
                                   grep(pattern = "-25-", names(PD)),
                                   grep(pattern = "0.03125x0.03125", 
                                        names(PD))))

E25003125<-data.frame()
for (variable in 1:length(third_table)) {
  E25003125<-rbind(E25003125,PD[[third_table[variable]]], 
                   make.row.names=F)
}
E25003125<-cbind(dist_ramas=rep("EXPONENCIAL",nrow(E25003125)), 
                 terminales=rep(25,nrow(E25003125)), 
                 area=rep(0.03125*0.03125,nrow(E25003125)), E25003125)

E25<-rbind(E2505,E250125,make.row.names=F)
E25<-rbind(E25,E25003125,make.row.names=F)

######Exponencial-125 terminales-0.5x0.5 (30 replicas)#####
first_table<-Reduce(intersect,list(grep(pattern = "E", names(PD)), 
                                   grep(pattern = "-125-", names(PD)),
                                   grep(pattern = "0.5x0.5", names(PD))))

E12505<-data.frame()
for (variable in 1:length(first_table)) {
  E12505<-rbind(E12505,PD[[first_table[variable]]], make.row.names=F)
}
E12505<-cbind(dist_ramas=rep("EXPONENCIAL",nrow(E12505)), 
              terminales=rep(125,nrow(E12505)), 
              area=rep(0.5*0.5,nrow(E12505)), E12505)

#######Exponencial-125 terminales-0.125x0.125 (30 replicas)######
second_table<-Reduce(intersect,list(grep(pattern = "E", names(PD)), 
                                    grep(pattern = "-125-", names(PD)),
                                    grep(pattern = "0.125x0.125", 
                                         names(PD))))

E1250125<-data.frame()
for (variable in 1:length(second_table)) {
  E1250125<-rbind(E1250125,PD[[second_table[variable]]], 
                  make.row.names=F)
}
E1250125<-cbind(dist_ramas=rep("EXPONENCIAL",nrow(E1250125)), 
                terminales=rep(125,nrow(E1250125)), 
                area=rep(0.125*0.125,nrow(E1250125)), E1250125)


######Exponencial-125 terminales-0.03125x0.03125 (30 replicas)######
third_table<-Reduce(intersect,list(grep(pattern = "E", names(PD)), 
                                   grep(pattern = "-125-", names(PD)),
                                   grep(pattern = "0.03125x0.03125", 
                                        names(PD))))

E125003125<-data.frame()
for (variable in 1:length(third_table)) {
  E125003125<-rbind(E125003125,PD[[third_table[variable]]], 
                    make.row.names=F)
}
E125003125<-cbind(dist_ramas=rep("EXPONENCIAL",nrow(E125003125)), 
                  terminales=rep(125,nrow(E125003125)), 
                  area=rep(0.03125*0.03125,nrow(E125003125)), E125003125)

E125<-rbind(E12505,E1250125,make.row.names=F)
E125<-rbind(E125,E125003125,make.row.names=F)


#####Log-normal-5 terminales-0.5x0.5 (30 replicas)####
first_table<-Reduce(intersect,list(grep(pattern = "L", names(PD)), 
                                   grep(pattern = "-5-", names(PD)),
                                   grep(pattern = "0.5x0.5", names(PD))))

L505<-data.frame()
for (variable in 1:length(first_table)) {
  L505<-rbind(L505,PD[[first_table[variable]]], make.row.names=F)
}
L505<-cbind(dist_ramas=rep("LOG-NORMAL",nrow(L505)), 
            terminales=rep(5,nrow(L505)), 
            area=rep(0.5*0.5,nrow(L505)), L505)

#####Log-normal-5 terminales-0.125x0.125 (30 replicas)#####
second_table<-Reduce(intersect,list(grep(pattern = "L", names(PD)), 
                                    grep(pattern = "-5-", names(PD)),
                                    grep(pattern = "0.125x0.125", 
                                         names(PD))))

L50125<-data.frame()
for (variable in 1:length(second_table)) {
  L50125<-rbind(L50125,PD[[second_table[variable]]], make.row.names=F)
}
L50125<-cbind(dist_ramas=rep("LOG-NORMAL",nrow(L50125)), 
              terminales=rep(5,nrow(L50125)), 
              area=rep(0.125*0.125,nrow(L50125)), L50125)


#######Log-normal-5 terminales-0.03125x0.03125 (30 replicas)#######
third_table<-Reduce(intersect,list(grep(pattern = "L", names(PD)), 
                                   grep(pattern = "-5-", names(PD)),
                                   grep(pattern = "0.03125x0.03125", 
                                        names(PD))))

L5003125<-data.frame()
for (variable in 1:length(third_table)) {
  L5003125<-rbind(L5003125,PD[[third_table[variable]]], 
                  make.row.names=F)
}
L5003125<-cbind(dist_ramas=rep("LOG-NORMAL",nrow(L5003125)), 
                terminales=rep(5,nrow(L5003125)), 
                area=rep(0.03125*0.03125,nrow(L5003125)), L5003125)

L5<-rbind(L505,L50125,make.row.names=F)
L5<-rbind(L5,L5003125,make.row.names=F)


########Log-normal-25 terminales-0.5x0.5 (30 replicas)#######
first_table<-Reduce(intersect,list(grep(pattern = "L", names(PD)), 
                                   grep(pattern = "-25-", names(PD)),
                                   grep(pattern = "0.5x0.5", names(PD))))

L2505<-data.frame()
for (variable in 1:length(first_table)) {
  L2505<-rbind(L2505,PD[[first_table[variable]]], make.row.names=F)
}
L2505<-cbind(dist_ramas=rep("LOG-NORMAL",nrow(L2505)), 
             terminales=rep(25,nrow(L2505)), 
             area=rep(0.5*0.5,nrow(L2505)), L2505)

########Log-normal-25 terminales-0.125x0.125 (30 replicas)#######
second_table<-Reduce(intersect,list(grep(pattern = "L", names(PD)), 
                                    grep(pattern = "-25-", names(PD)),
                                    grep(pattern = "0.125x0.125", 
                                         names(PD))))

L250125<-data.frame()
for (variable in 1:length(second_table)) {
  L250125<-rbind(L250125,PD[[second_table[variable]]], make.row.names=F)
}
L250125<-cbind(dist_ramas=rep("LOG-NORMAL",nrow(L250125)), 
               terminales=rep(25,nrow(L250125)), 
               area=rep(0.125*0.125,nrow(L250125)), L250125)


########Log-normal-25 terminales-0.03125x0.03125 (30 replicas)######
third_table<-Reduce(intersect,list(grep(pattern = "L", names(PD)), 
                                   grep(pattern = "-25-", names(PD)),
                                   grep(pattern = "0.03125x0.03125", 
                                        names(PD))))

L25003125<-data.frame()
for (variable in 1:length(third_table)) {
  L25003125<-rbind(L25003125,PD[[third_table[variable]]], 
                   make.row.names=F)
}
L25003125<-cbind(dist_ramas=rep("LOG-NORMAL",nrow(L25003125)), 
                 terminales=rep(25,nrow(L25003125)), 
                 area=rep(0.03125*0.03125,nrow(L25003125)), L25003125)

L25<-rbind(L2505,L250125,make.row.names=F)
L25<-rbind(L25,L25003125,make.row.names=F)

######Log-normal-125 terminales-0.5x0.5 (30 replicas)#####
first_table<-Reduce(intersect,list(grep(pattern = "L", names(PD)), 
                                   grep(pattern = "-125-", names(PD)),
                                   grep(pattern = "0.5x0.5", names(PD))))

L12505<-data.frame()
for (variable in 1:length(first_table)) {
  L12505<-rbind(L12505,PD[[first_table[variable]]], make.row.names=F)
}
L12505<-cbind(dist_ramas=rep("LOG-NORMAL",nrow(L12505)), 
              terminales=rep(125,nrow(L12505)), 
              area=rep(0.5*0.5,nrow(L12505)), L12505)

#######Log-normal-125 terminales-0.125x0.125 (30 replicas)######
second_table<-Reduce(intersect,list(grep(pattern = "L", names(PD)), 
                                    grep(pattern = "-125-", names(PD)),
                                    grep(pattern = "0.125x0.125", 
                                         names(PD))))

L1250125<-data.frame()
for (variable in 1:length(second_table)) {
  L1250125<-rbind(L1250125,PD[[second_table[variable]]], 
                  make.row.names=F)
}
L1250125<-cbind(dist_ramas=rep("LOG-NORMAL",nrow(L1250125)), 
                terminales=rep(125,nrow(L1250125)), 
                area=rep(0.125*0.125,nrow(L1250125)), L1250125)


######Log-normal-125 terminales-0.03125x0.03125 (30 replicas)######
third_table<-Reduce(intersect,list(grep(pattern = "L", names(PD)), 
                                   grep(pattern = "-125-", names(PD)),
                                   grep(pattern = "0.03125x0.03125", 
                                        names(PD))))

L125003125<-data.frame()
for (variable in 1:length(third_table)) {
  L125003125<-rbind(L125003125,PD[[third_table[variable]]], 
                    make.row.names=F)
}
L125003125<-cbind(dist_ramas=rep("LOG-NORMAL",nrow(L125003125)), 
                  terminales=rep(125,nrow(L125003125)), 
                  area=rep(0.03125*0.03125,nrow(L125003125)), L125003125)

L125<-rbind(L12505,L1250125,make.row.names=F)
L125<-rbind(L125,L125003125,make.row.names=F)

######Uniendo todas las tablas de uniforme, exponencial y log-normal####

##Tabla con todos los datos uniformes
unif<-rbind(U5,U25,U125)
unif$terminales<-as.factor(unif$terminales)
unif$area<-as.factor(unif$area)

##Tabla con todos los datos exponencial
expo<-rbind(E5,E25 ,E125)
expo$terminales<-as.factor(expo$terminales)
expo$area<-as.factor(expo$area)

##Tabla con todos los datos log-normal
log<- rbind(L5,L25,L125)
log$terminales<-as.factor(log$terminales)
log$area<-as.factor(log$area)


###########Boxplot ###########
library(ggplot2)

##Boxplot de distribucion de ramas uniforme
(ggplot(unif, aes(x=area, y=PD,fill=terminales))
  + geom_boxplot()
  + scale_fill_manual(values=c("#C6D876", "#DEAA8A", "#00B6FF"))
  + labs( x="Area", y= "PD", fill="Terminals")
  + theme_bw()
  + theme(axis.title.x = element_text(vjust = 0,
                                      face= "bold",
                                      size=12,
                                      family = "serif"),
          axis.title.y = element_text(vjust = 0,
                                      face= "bold",
                                      size=12,
                                      family = "serif"),
          plot.title = element_text(vjust = 2,
                                    size=12,
                                    family="serif",
                                    hjust = 0.5),
          axis.text = element_text(vjust = 0,
                                   size=12,
                                   family="serif",
                                   colour = "black"))
)

##Boxplot de distribucion de ramas exponencial
(ggplot(expo, aes(x=area, y=PD,fill=terminales))
  + geom_boxplot()
  + scale_fill_manual(values=c("#C6D876", "#DEAA8A", "#00B6FF"))
  + labs( x="Area", y= "PD", fill="Terminals")
  + theme_bw()
  + theme(axis.title.x = element_text(vjust = 0,
                                      face= "bold",
                                      size=12,
                                      family = "serif"),
          axis.title.y = element_text(vjust = 0,
                                      face= "bold",
                                      size=12,
                                      family = "serif"),
          plot.title = element_text(vjust = 0,
                                    size=12,
                                    family="serif",
                                    hjust = 0.5),
          axis.text = element_text(vjust = 0,
                                   size=12,
                                   family="serif",
                                   colour = "black"))
)


##Boxplot de distribucion de ramas log-normal
(ggplot(log, aes(x=area, y=PD,fill=terminales))
  + geom_boxplot()
  + scale_fill_manual(values=c("#C6D876", "#DEAA8A", "#00B6FF"))
  + labs( x="Area", y= "PD", fill="Terminals")
  + theme_bw()
  + theme(axis.title.x = element_text(vjust = 0,
                                      face= "bold",
                                      size=12,
                                      family = "serif"),
          axis.title.y = element_text(vjust = 0,
                                      face= "bold",
                                      size=12,
                                      family = "serif"),
          plot.title = element_text(vjust = 0,
                                    size=12,
                                    family="serif",
                                    hjust = 0.5),
          axis.text = element_text(vjust = 0,
                                   size=12,
                                   family="serif",
                                   colour = "black"))
)

###Tabla con las medias de cada PD por terminales por tama?o de area####
unif_mean<-aggregate(PD~terminales*area*dist_ramas, FUN= mean,data=unif)
expo_mean<-aggregate(PD~terminales*area*dist_ramas, FUN= mean,data=expo)
log_mean<-aggregate(PD~terminales*area*dist_ramas, FUN= mean,data=log)

####Grafico de lineas#####
##Grafico de lineas de PD vs area por cada tama?o de terminales en las distirbuciones uniformes
(ggplot(unif_mean, aes(x=area, y=PD,group=terminales))
  + geom_line(aes(col=terminales), lwd=1.25)
  + geom_point(size=2)
  + scale_color_manual(values=c("#C6D876", "#DEAA8A", "#00B6FF"))
  + labs( x="Area", y= "PD", colour="Terminals")
  + theme_bw()
  + theme(axis.title.x = element_text(vjust = 0,
                                      face= "bold",
                                      size=12,
                                      family = "serif"),
          axis.title.y = element_text(vjust = 0,
                                      face= "bold",
                                      size=12,
                                      family = "serif"),
          plot.title = element_text(vjust = 0,
                                    size=12,
                                    family="serif",
                                    hjust = 0.5),
          axis.text = element_text(vjust = 0,
                                   size=12,
                                   family="serif",
                                   colour = "black"))
)


##Grafico de lineas de PD vs area por cada tama?o de terminales en las distirbuciones exponencial
(ggplot(expo_mean, aes(x=area, y=PD,group=terminales))
  + geom_line(aes(col=terminales), lwd=1.25)
  + geom_point(size=2)
  + scale_color_manual(values=c("#C6D876", "#DEAA8A", "#00B6FF"))
  + labs( x="Area", y= "PD", colour="Terminals")
  + theme_bw()
  + theme(axis.title.x = element_text(vjust = 0,
                                      face= "bold",
                                      size=12,
                                      family = "serif"),
          axis.title.y = element_text(vjust = 0,
                                      face= "bold",
                                      size=12,
                                      family = "serif"),
          plot.title = element_text(vjust = 0,
                                    size=12,
                                    family="serif",
                                    hjust = 0.5),
          axis.text = element_text(vjust = 0,
                                   size=12,
                                   family="serif",
                                   colour = "black"))
)


##Grafico de lineas de PD vs area por cada tama?o de terminales en las distirbuciones log-normal
(ggplot(log_mean, aes(x=area, y=PD,group=terminales))
  + geom_line(aes(col=terminales), lwd=1.25)
  + geom_point(size=2)
  + scale_color_manual(values=c("#C6D876", "#DEAA8A", "#00B6FF"))
  + labs( x="Area", y= "PD", colour="Terminals")
  + theme_bw()
  + theme(axis.title.x = element_text(vjust = 0,
                                      face= "bold",
                                      size=12,
                                      family = "serif"),
          axis.title.y = element_text(vjust = 0,
                                      face= "bold",
                                      size=12,
                                      family = "serif"),
          plot.title = element_text(vjust = 0,
                                    size=12,
                                    family="serif",
                                    hjust = 0.5),
          axis.text = element_text(vjust = 0,
                                   size=12,
                                   family="serif",
                                   colour = "black"))
)



####Tabla que contiene todos los valores de PD discriminados por tipo de distribucion del cual salieron las longitudes de rama, numero de terminales y tama?o de area, adicionalmente contiene las riquezas de especie
all<-rbind(unif,expo,log)
all<-all[,-4]

####Modelo de regresion lineal multiple con interaccion de las areas con los numeros de terminales y la distribucion de las longitudes de ramas####
modelo<-lm(PD~area*terminales*dist_ramas, data=all)
resu<-summary(modelo)

write.csv(resu$coefficients,"resultados_modelo_lineal_multiple.csv")
resultados<-data.frame(R_squared=resu$r.squared,
           Adjusted_R_squared=resu$adj.r.squared[1],
           f_statistic=resu$fstatistic[1], 
           df_numerador=resu$fstatistic[2],
           df_denominador=resu$fstatistic[3], 
           p_value=2.2e-16 )
write.csv(resultados, "resultados_modelo_lineal_multiple_2.csv")

###Normalidad de los residuos
library(nortest)
hist(modelo$residuals)
qqnorm(modelo$residuals)
qqline(modelo$residuals)

lillie.test(modelo$residuals[-c(44387,43857)])

(ggplot(data = data.frame(predict_values = predict(modelo),
                         residuos = residuals(modelo)),
       aes(x = predict_values, y = residuos)) +
  geom_point() +
  geom_smooth(color = "firebrick", se = FALSE) +
  geom_hline(yintercept = 0) +
  theme_bw())


##Hmocedasticidad de los residuos con el test de Breush-Pagan
library(lmtest)
bptest(modelo)


####Modelo de regresion lineal multiple ROBUSTA con interaccion de las areas con los numeros de terminales y la distribucion de las longitudes de ramas####

library(MASS)
modelo_rob <- rlm(all$PD ~ all$area*all$terminales*all$dist_ramas)
summary(modelo_rob)

## a partir de los coeficientes se calcula sus respectivos valores de p, usando una prueba de t-student
coeficientes <- as.data.frame(summary(modelo_rob)$coefficients)
coeficientes$p_value <- 2*(1 - pt(q = abs(coeficientes$`t value`),
                                  df = nrow(all) - 6))
coeficientes

