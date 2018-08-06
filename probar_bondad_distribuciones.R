

#y=z$los_dias
#summary(z$los_dias)
#length(z$los_dias)
#type="realplus"
#i=1
#signif=0.05

#prueba<-contar_dist_acept(y=y$los_dias,type="realplus",signif=0.05)

contar_dist_acept<-function(y,type,signif)
{
  
 # order_fit<-fitDist(y,type=type)

  nom_dist<-c( "exGAUS",
               "GIG",
               "GG",
               "BCCGo",
               "BCPEo",
               "GA",
               "GB2",
               "BCTo",
               "WEI3" ,
               "LOGNO",
               "EXP",
               "PARETO2",
               "IG",
               "IGAMMA",
               "NO")
  
  n_dist<- length(nom_dist)
  resultado=NULL
  acepta=0
  
 
  
  for (i in 1:n_dist)
  { 
    
    fit<-gamlssML(y,family=nom_dist[i])

    n_param=length(fit$parameters)
    pt_dis<-eval(parse(text=paste("p",nom_dist[i],sep="")))
    #n_param
    #nom_dist[i]
    
    if (n_param==1)
    {
      
      ks_fit<- ks.test(x=y, y=pt_dis,
                       mu=fit$mu)
      
      # ad_fit<- ad.test(y,null=pt_dis,
      #  mu=fit$mu)
    } 
    
    if (n_param==2)
    {
      
      ks_fit<- ks.test(x=y, y=pt_dis,
                       mu=fit$mu,
                       sigma=fit$sigma)
      
      #ad_fit<- ad.test(y,null=pt_dis,
      # mu=fit$mu,
      # sigma=fit$sigma)
    }
    
    
    if (n_param==3)
    {
      
      ks_fit<- ks.test(x=y, y=pt_dis,
                       mu=fit$mu,
                       sigma=fit$sigma,
                       nu=fit$nu)
      
      #ad_fit<- ad.test(y,null=pt_dis,
      #                mu=fit$mu,
      #               sigma=fit$sigma,
      #              nu=fit$nu)
    }
    
    
    
    if (n_param==4)
    {
      ks_fit<- ks.test(x=y, y=pt_dis,
                       mu=fit$mu,
                       sigma=fit$sigma,
                       nu=fit$nu,
                       tau=fit$tau)
      
      # ad_fit<- ad.test(y,null=pt_dis,
      #                 mu=fit$mu,
      #                sigma=fit$sigma,
      #               nu=fit$nu,
      #              tau=fit$tau)
    }
    
    if (n_param>4)
    {
      
      Print("La distribución ajustada tiene más de 4 parámetros")
      
    }
    
    if(ks_fit$p.value>=signif){
      acepta=acepta+1
    
      
    }
    
    #,ad_fit$p.value   se quitó de la función
    resultado_add<-data.frame(nom_dist[i],ks_fit$p.value)
    resultado<-rbind(resultado,resultado_add)
    
    
    
  }
  #,"AD"
  nom_prueba<-c("Dist","KS")
  names(resultado)<-nom_prueba
  resultado$res_prueba_ks<-case_when(
    resultado$KS<=signif ~"Rechaza",
    resultado$KS>signif~"Acepta"
  )
  
  #resultado$res_prueba_ad<-case_when(
  # resultado$AD<=signif ~"Rechaza",
  #resultado$AD>signif~"Acepta")
return(acepta)
  

}


####Probar distribuciones
#y=nodo1[,1]
#type= "realplus"
#signif=0.05

probar_dist<-function(y,type,signif)
{
  
  order_fit<-fitDist(y,type=type,extra="NO")
  
  
  
  nom_dist<-names(order_fit$fits)
    
    
  n_dist<- length(nom_dist)
  resultado=NULL
  acepta=0
  
  #y=y$los_dias
 # i=2
  for (i in 1:n_dist)
  { 
    rm(ks_fit)
    rm(ad_fit)
    fit<-gamlssML(y,family=nom_dist[i])
  
    n_param=length(fit$parameters)
    pt_dis<-eval(parse(text=paste("p",nom_dist[i],sep="")))
    #n_param
    #nom_dist[i]
    
    if (n_param==1)
    {
      
      ks_fit<- tryCatch(ks.test(x=y, y=pt_dis,
                       mu=fit$mu),error=function(e) -1)
      
      ad_fit<- tryCatch(ad.test(y,null=pt_dis,
        mu=fit$mu),error=function(e) -1)
    } 
    
    if (n_param==2)
    {
      
      ks_fit<- tryCatch(ks.test(x=y, y=pt_dis,
                       mu=fit$mu,
                       sigma=fit$sigma),error=function(e) -1)
      
      ad_fit<- tryCatch(ad.test(y,null=pt_dis,
       mu=fit$mu,sigma=fit$sigma),error=function(e) -1)
    }
    
    
    if (n_param==3)
    {
      
      ks_fit<- tryCatch(ks.test(x=y, y=pt_dis,
                       mu=fit$mu,
                       sigma=fit$sigma,
                       nu=fit$nu),error=function(e) -1)
      
      ad_fit<- tryCatch(ad.test(y,null=pt_dis,
                      mu=fit$mu,
                     sigma=fit$sigma,
                    nu=fit$nu),error=function(e) -1)
    }
    
    
    
    if (n_param==4)
    {
      ks_fit<- tryCatch(ks.test(x=y, y=pt_dis,
                       mu=fit$mu,
                       sigma=fit$sigma,
                       nu=fit$nu,
                       tau=fit$tau),error=function(e) -1)
      
       ad_fit<- tryCatch(ad.test(y,null=pt_dis,
                       mu=fit$mu,
                      sigma=fit$sigma,
                     nu=fit$nu,
                    tau=fit$tau),error=function(e) -1)
    }
    
    if (n_param>4)
    {
      
      Print("La distribución ajustada tiene más de 4 parámetros")
      
    }
 
    if (typeof(ks_fit)!="list"){ks_fit$p.value=-1}
    if (typeof(ad_fit)!="list"){ad_fit$p.value=-1}
    
    
    
    #,ad_fit$p.value   se quitó de la función
    resultado_add<-data.frame(nom_dist[i],ks_fit$p.value,ad_fit$p.value,fit$sbc,fit$aic)
    resultado<-rbind(resultado,resultado_add)
    
    
    
  }
  #,"AD"
  nom_prueba<-c("Dist","KS", "AD","SBC","AIC")
  names(resultado)<-nom_prueba
  resultado$res_prueba_ks<-case_when(
    resultado$KS==-1 ~"No Aplica",
    resultado$KS<=signif ~"Rechaza",
    resultado$KS>signif~"Acepta"
  )
  
  resultado$res_prueba_ad<-case_when(
    resultado$AD==-1~"No Aplica",
    resultado$AD<=signif ~"Rechaza",
  resultado$AD>signif~"Acepta")
 
  return(resultado)
  
  
}





