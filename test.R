setwd('d://temp//datadiveLT')

Y2008=read.csv('kandidatasDaugiamandate2008_kandidatasitem.csv',fileEncoding ='UTF8',stringsAsFactors=FALSE)
d2008=read.csv('kandidatoDeklaracija2008_kandidatodeklaracija.csv',fileEncoding ='UTF8',stringsAsFactors=FALSE)

Y2008$kandidatas=gsub('\\s+',' ',tolower(Y2008$kandidatas))
d2008$kandidatas=gsub('\\s+',' ',tolower(d2008$kandidatas))

turtas=data.frame(k=as.character(d2008$kandidatas),turtas=(d2008$turtas+d2008$suteiktos_paskolos+d2008$gryni_pinigai+d2008$vertybiniai_popieriai+d2008$gautos_pajamos)-
  d2008$mokesciai-d2008$gautos_paskolos)

index=sapply(Y2008$kandidatas,function(x){  y=which(x==turtas$k);if(length(y)>0)as.integer(y[1])})
Y2008= data.frame(Y2008,turtas=turtas[index,2])

Y2004=read.csv('nariai2004.csv',fileEncoding ='UTF8',stringsAsFactors=FALSE,sep=';')

Y2004=sapply(Y2004,function(x){
  gsub('^ *','',tolower((gsub('\\*\\*\\*','',gsub('\\n','',gsub('\\"','',(x)))))))
})

Y2008=data.frame(Y2008,buves=sapply(Y2008$kandidatas,function(x){  any(x==Y2004)}   ))
             
             
Y2008_nariai=read.csv('nariai2008.csv',fileEncoding ='UTF8',stringsAsFactors=FALSE,sep=',')
Y2008_nariai=Y2008_nariai[which(Y2008_nariai$Daugiamandate!='Daugiamandate '),1]
Y2008_nariai=sapply(Y2008_nariai,function(x){
  gsub('\\s$','',gsub('^ *','',tolower((gsub('\\*\\*\\*','',gsub('\\n','',gsub('\\"','',(x))))))))
})


Y2008=data.frame(Y2008,narys=sapply(Y2008$kandidatas,function(x){  any(x==Y2008_nariai)}   ))

Y2008$gimimo_data=sapply(Y2008$gimimo_data,function(x){2008-as.integer(strsplit(x,'-')[[1]][1])})
Y2008$issilavinimas=sapply(Y2008$issilavinimas,function(x)length(strsplit(x,';')[[1]]))
Y2008$uzsienio_kalbos=sapply(Y2008$uzsienio_kalbos,function(x)length(strsplit(x,',')[[1]]))
Y2008$seimynine_padetis=as.factor(Y2008$seimynine_padetis)
Y2008$iskele=as.factor(Y2008$iskele)
Y2008$tautybe=as.factor(Y2008$tautybe)
Y2008$pripazintas_kaltu[which(Y2008$pripazintas_kaltu=='Taip ')]=TRUE
Y2008$pripazintas_kaltu[which(Y2008$pripazintas_kaltu=='Nenurode ')]=NA
Y2008$pripazintas_kaltu[which(Y2008$pripazintas_kaltu=='Ne ')]=FALSE
Y2008$pripazintas_kaltu=as.logical(Y2008$pripazintas_kaltu)

Y2008$sunkus_nusikaltimas[which(Y2008$sunkus_nusikaltimas=='Taip ')]=TRUE
Y2008$sunkus_nusikaltimas[which(Y2008$sunkus_nusikaltimas=='Nenurode ')]=NA
Y2008$sunkus_nusikaltimas[which(Y2008$sunkus_nusikaltimas=='Ne ')]=FALSE
Y2008$sunkus_nusikaltimas=as.logical(Y2008$sunkus_nusikaltimas)
Y2008=Y2008[!is.na(Y2008$sunkus_nusikaltimas),]

Y2008$neatlikta_bausme[which(Y2008$neatlikta_bausme=='Turiu ')]=TRUE
Y2008$neatlikta_bausme[which(Y2008$neatlikta_bausme=='Nenurode ')]=NA
Y2008$neatlikta_bausme[which(Y2008$neatlikta_bausme=='Neturiu ')]=FALSE
Y2008$neatlikta_bausme=as.logical(Y2008$neatlikta_bausme)
Y2008=Y2008[which((Y2008$apygarda!='Daugiamandate')),]
Y2008$narys=factor(Y2008$narys)
train=data.frame(metai=Y2008$gimimo_data,seimynine_padetis=Y2008$seimynine_padetis,
                 sunkus_nusikaltimas=Y2008$sunkus_nusikaltimas,
                 issilavinimas=Y2008$issilavinimas,tautybe=Y2008$tautybe,
                 uzsienio_kalbos=Y2008$uzsienio_kalbos,pripazintas_kaltu=Y2008$pripazintas_kaltu,neatlikta_bausme=Y2008$neatlikta_bausme,
                 buves=Y2008$buves,turtas=Y2008$turtas,narys=Y2008$narys)

require(randomForest)

#test=train[501:775,]
#train=train[1:500,]
forest=randomForest(narys~.,train,ntree=100)
varImpPlot(forest)

require('ggplot2')
rez=data.frame(names=(rownames((forest$importance))),values=as.numeric(forest$importance))
levels(rez$names)=c('Buvęs Seimo narys','Išsilavinimas','Amžius','Neatlikta bausmė','Pripažintas kaltu','Šeimyninė padėtis','Sunkus nusikaltimas','Tautybė','Turtas','Užsienio kalbos')
rez=rez[order(rez$values),]
rez=data.frame(order=order(rez$values),names=rez$names,values=rez$values)

#rez=rez[order(rez$values),]
ggplot(rez,aes(values,paste(order-1,'. ',names,sep='')))+geom_point()+ylab('Savybės')+xlab('Reikšmės')
