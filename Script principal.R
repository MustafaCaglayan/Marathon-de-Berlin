rm(list=ls())
path="C:/Users/Mustafa/Documents/Cours/DUT STID 1ère année/Semestre 2/Projet/Github"
setwd(path)
library(readr)
library(readxl)
library(ggplot2)
library(dplyr)
library(ggthemes)

#IMPORTATION ET NETTOYAGE DES DONNEES
{
  #BERLIN
  #Importation de la base
  Berlin <- read_delim(paste0(path,"/data/Berlin.csv"), 
                       ";", escape_double = FALSE, 
                       col_types = cols(Resultats = col_time(format = "%H:%M:%S")), 
                       trim_ws = TRUE)
  #Suppression des resultats < 6h15min reglementaire
  Berlin=Berlin[-which(Berlin$Resultats>6*3600+15*60),]
  #Suppression des colonnes inutiles
  bd_berlin=Berlin[,-c(7,3,1)]
  rm(Berlin)
  colnames(bd_berlin)=c("Annee","Sexe","Age","Resultats")
  #Création colonne Vitesse
  bd_berlin$Vitesse="NA"
  bd_berlin$Vitesse=ifelse(bd_berlin$Resultats!="NA",42/(as.numeric(bd_berlin$Resultats)/3600))
  #Suppression des valeurs aberrantes de l'âge
  bd_berlin=bd_berlin[-which(bd_berlin$Age<18),]
  #Nouvelle data frame où on enlève les valeurs manquantes de l'âge
  age_vitesse=data.frame(cbind(bd_berlin$Age,bd_berlin$Vitesse))
  colnames(age_vitesse)=c("Age","Vitesse")
  age_vitesse=age_vitesse %>% filter(!is.na(Age))
  
  #NATIONALITE
  liste=list()
  table_nationalite=data.frame()
  j=1
  for (i in 2005:2010){
    #Importation de chaques années dans une liste
    liste[[paste(i)]]=read.csv(paste(path,"/data/",i,".csv",collapse = "", sep = ""))
    #Suppression les colonnes inutiles
    liste[[paste(i)]]=liste[[j]][,-c(9,6,5,3,1)]
    #Creation colonne Annee
    liste[[paste(i)]]$Annee=i
    #Jointure de chaques années
    table_nationalite=rbind(table_nationalite,liste[[paste(i)]])
    j=j+1
  }
  rm(i,j,liste)

}


#GRAPHIQUE
#HISTOGRAMME DE LA REPARTITION DU RESULTAT
{
  ggplot(data = bd_berlin, aes(Resultats)) +
    geom_histogram(colour="black", fill="grey") +
    ylab("Nombre de coureurs")
}

#COULOIR DE PERFORMANCE DE L'AGE
{
  p=ggplot(age_vitesse, aes(Age,Vitesse)) +
    geom_quantile(aes(Age,Vitesse,colour = factor(..quantile..)), 
                  method="rqss",
                  quantiles=c(0.25,0.375,0.625,0.75), 
                  size=0.9) +
    scale_color_manual(name="Couloirs",
                       labels=c("Couloir 4", "Couloir 3", "Couloir 2" ,"Couloir 1"),
                       values=c("#00E1FF","#00AEFF","#0065FF","#0032FF")) +
    labs(title="Couloir de perfomance de l'age")
    p + theme_stata()
}

#Nuage de point / Modèle gam
{
  ggplot(age_vitesse, aes(Age,Vitesse)) +
    geom_point() +
    geom_smooth(span=0.1, aes(colour="Ajustement de la tendance")) +
    labs(title="Nuage de points de la vitesse en fonction de l'age") +
    scale_colour_manual(name="Légende",values = "blue")
}


#MODELE DE MOORE (fonctionne pas à cause de discontinuités dans l'age)
{
  x=age_vitesse$Age
  y=age_vitesse$Vitesse
  
  #Fonction à minimiser (moidre carré)
  modele_moore <- function(x,p){
    p[1]*exp(-p[2]*x) + p[3]*(1-exp(p[4]*x))
  }
  
  moindre_carre = function(p){
    sum((y-modele_moore(x,p))**2)
  }
  
  #Optimisation des paramètres
  for (i in 1:1000){
    start=c(max(y),runif(1,0,1),runif(1,0,1),runif(1,0,1))
    model <- nlminb(moindre_carre, start=start, lower = 0)
    
    a=model$par[1]
    b=model$par[2]
    c=model$par[3]
    d=model$par[4]
    yî=a*(1-exp(-b*x))+c*(1-exp(d*x))
    R2=var(yî)/var(y)
    
    print(R2)
    
    if (R2>0.5){
      break
    }
  }
  
  #Graphique de l'ajustement
  ajustement=data.frame(x,y,yî)
  ggplot(ajustement,aes(x,y,yî)) +
    geom_point(aes(x,y))+
    geom_point(aes(x,yî), color="red")
}



for (i in 1:nrow(Berlin_reduit)){
  
  if (Berlin_reduit$Annee[i]==2001){
    bd_alluvial$r_2001[i]=Berlin_reduit$Classement[i]
  }
  
  else if(Berlin_reduit$Annee[i]==2002) {
    bd_alluvial$r_2002[i]=Berlin_reduit$Classement[i]
  }
  
  else if(Berlin_reduit$Annee[i]==2003) {
    bd_alluvial$r_2003[i]=Berlin_reduit$Classement[i]
  }
  
  else if(Berlin_reduit$Annee[i]==2004) {
    bd_alluvial$r_2004[i]=Berlin_reduit$Classement[i]
  }
  
  else if(Berlin_reduit$Annee[i]==2005) {
    bd_alluvial$r_2005[i]=Berlin_reduit$Classement[i]
  }
  
  else if(Berlin_reduit$Annee[i]==2006) {
    bd_alluvial$r_2006[i]=Berlin_reduit$Classement[i]
  }
  
  else if(Berlin_reduit$Annee[i]==2007) {
    bd_alluvial$r_2007[i]=Berlin_reduit$Classement[i]
  }
  
  else if(Berlin_reduit$Annee[i]==2008) {
    bd_alluvial$r_2008[i]=Berlin_reduit$Classement[i]
  }
  
  else if(Berlin_reduit$Annee[i]==2009) {
    bd_alluvial$r_2009[i]=Berlin_reduit$Classement[i]
  }
  
  else if(Berlin_reduit$Annee[i]==2010) {
    bd_alluvial$r_2010[i]=Berlin_reduit$Classement[i]
  }
}


