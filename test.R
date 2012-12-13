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
    issilavinimas=Y2008$issilavinimas,tautybe=Y2008$tautybe,
    uzsienio_kalbos=Y2008$uzsienio_kalbos,
    buves=Y2008$buves,turtas=Y2008$turtas,narys=Y2008$narys
    #,sunkus_nusikaltimas=Y2008$sunkus_nusikaltimas,pripazintas_kaltu=Y2008$pripazintas_kaltu,neatlikta_bausme=Y2008$neatlikta_bausme
                 )

require(randomForest)

forest=randomForest(narys~.,train,ntree=100)
varImpPlot(forest)

require('ggplot2')
rez=data.frame(names=(rownames((forest$importance))),values=as.numeric(forest$importance))
#levels(rez$names)=c('Buvęs Seimo narys','Išsilavinimas','Amžius','Neatlikta bausmė','Pripažintas kaltu','Šeimyninė padėtis','Sunkus nusikaltimas','Tautybė','Turtas','Užsienio kalbos')
rez=rez[order(rez$values),]
rez=data.frame(order=order(rez$values),names=rez$names,values=rez$values)

#rez=rez[order(rez$values),]
ggplot(rez,aes(values,paste(order-1,'. ',names,sep='')))+geom_point()+ylab('Savybės')+xlab('Reikšmės')

test=train[501:774,]
train=train[1:500,]

tmp=list()
for(i in 1:5)
{
  sample=train[-((100*i-99):(100*i)),]
  outcome= train[((100*i-99):(100*i)),]
  forest=randomForest(narys~.,sample,ntree=100)
  rez=predict(forest, outcome,type='prob')
  x=(as.numeric(outcome$narys)-1)-rez[,2]
  print(sqrt(sum(x^2)/length(x)))
  tmp[[i]]=forest
}
ord=(1+(randomForest::importance(tmp[[1]]))/sum(randomForest::importance(tmp[[1]]))/5)*
(1+(randomForest::importance(tmp[[2]]))/sum(randomForest::importance(tmp[[2]]))/5)*
(1+(randomForest::importance(tmp[[3]]))/sum(randomForest::importance(tmp[[3]]))/5)*
(1+(randomForest::importance(tmp[[4]]))/sum(randomForest::importance(tmp[[4]]))/5)*
(1+(randomForest::importance(tmp[[5]]))/sum(randomForest::importance(tmp[[5]]))/5)


train=subset(train,select=c(tail(rownames(ord)[order(ord)],5),'narys'))

forest=randomForest(narys~.,train,ntree=100)
rez=predict(forest, test,type='prob')


x=(as.numeric(test$narys)-1)-rez[,2]
sqrt(sum(x^2)/length(x))

library(ROCR)
#library(e1071)
pred=prediction((rez)[,2],(as.numeric(test$narys)-1))
plot(performance(pred,"cost", cost.fp=4, cost.fn=1))
rez=ifelse(rez[,2]>.75,1,0)
sum(ifelse(as.logical(rez)!=as.logical(test$narys),1,0))/nrow(test)
#.65

#2012
train=data.frame(metai=Y2008$gimimo_data,seimynine_padetis=Y2008$seimynine_padetis,
                 issilavinimas=Y2008$issilavinimas,tautybe=Y2008$tautybe,
                 uzsienio_kalbos=Y2008$uzsienio_kalbos,
                 buves=Y2008$buves,turtas=Y2008$turtas,narys=Y2008$narys
                 #,sunkus_nusikaltimas=Y2008$sunkus_nusikaltimas,pripazintas_kaltu=Y2008$pripazintas_kaltu,neatlikta_bausme=Y2008$neatlikta_bausme
)

require(randomForest)

forest=randomForest(narys~.,train,ntree=100)


Y2012=read.csv('kandidatasDaugiamandate2012_kandidatasitem.csv',fileEncoding ='UTF8',stringsAsFactors=FALSE)
d2012=read.csv('kandidatoDeklaracija2012_kandidatodeklaracija.csv',fileEncoding ='UTF8',stringsAsFactors=FALSE)

Y2012$kandidatas=gsub('\\s+',' ',tolower(Y2012$kandidatas))
d2012$kandidatas=gsub('\\s+',' ',tolower(d2012$kandidatas))

turtas=data.frame(k=as.character(d2012$kandidatas),turtas=(d2012$turtas+d2012$suteiktos_paskolos+d2012$gryni_pinigai+d2012$vertybiniai_popieriai+d2012$gautos_pajamos)-
                    d2012$mokesciai-d2012$gautos_paskolos)
cpi=c(0.013,.038,.034,.033)
turtas[,2]=turtas[,2]*(2-exp(sum(log(cpi+1))))
index=sapply(Y2012$kandidatas,function(x){  y=which(x==turtas$k);if(length(y)>0)as.integer(y[1])})
Y2012= data.frame(Y2012,turtas=turtas[index,2])
Y2012=data.frame(Y2012,buves=sapply(Y2012$kandidatas,function(x){  any(x==Y2008_nariai)}   ))

Y2012_nariai=read.csv('nariai2012.csv',fileEncoding ='UTF8',stringsAsFactors=FALSE,sep=',')
Y2012_nariai=Y2012_nariai[which(Y2012_nariai$Daugiamandate!='Daugiamandate '),1]
Y2012_nariai=sapply(Y2012_nariai,function(x){
  gsub('\\s$','',gsub('^ *','',tolower((gsub('\\*\\*\\*','',gsub('\\n','',gsub('\\"','',(x))))))))
})


Y2012=data.frame(Y2012,narys=sapply(Y2012$kandidatas,function(x){  any(x==Y2012_nariai)}   ))

Y2012$gimimo_data=sapply(Y2012$gimimo_data,function(x){2008-as.integer(strsplit(x,'-')[[1]][1])})
Y2012$issilavinimas=sapply(Y2012$issilavinimas,function(x)length(strsplit(x,';')[[1]]))
Y2012$uzsienio_kalbos=sapply(Y2012$uzsienio_kalbos,function(x)length(strsplit(x,',')[[1]]))
Y2012$seimynine_padetis=as.factor(Y2012$seimynine_padetis)
Y2012$iskele=as.factor(Y2012$iskele)
Y2012$tautybe=as.factor(Y2012$tautybe)
Y2012$pripazintas_kaltu[which(Y2012$pripazintas_kaltu=='Taip ')]=TRUE
Y2012$pripazintas_kaltu[which(Y2012$pripazintas_kaltu=='Nenurode ')]=NA
Y2012$pripazintas_kaltu[which(Y2012$pripazintas_kaltu=='Ne ')]=FALSE
Y2012$pripazintas_kaltu=as.logical(Y2012$pripazintas_kaltu)

Y2012$sunkus_nusikaltimas[which(Y2012$sunkus_nusikaltimas=='Taip ')]=TRUE
Y2012$sunkus_nusikaltimas[which(Y2012$sunkus_nusikaltimas=='Nenurode ')]=NA
Y2012$sunkus_nusikaltimas[which(Y2012$sunkus_nusikaltimas=='Ne ')]=FALSE
Y2012$sunkus_nusikaltimas=as.logical(Y2012$sunkus_nusikaltimas)
Y2012=Y2012[!is.na(Y2012$sunkus_nusikaltimas),]

Y2012$neatlikta_bausme[which(Y2012$neatlikta_bausme=='Turiu ')]=TRUE
Y2012$neatlikta_bausme[which(Y2012$neatlikta_bausme=='Nenurode ')]=NA
Y2012$neatlikta_bausme[which(Y2012$neatlikta_bausme=='Neturiu ')]=FALSE
Y2012$neatlikta_bausme=as.logical(Y2012$neatlikta_bausme)
Y2012=Y2012[which((Y2012$apygarda!='Daugiamandate')),]
Y2012$narys=factor(Y2012$narys)

test=data.frame(metai=Y2012$gimimo_data,seimynine_padetis=Y2012$seimynine_padetis,
                 issilavinimas=Y2012$issilavinimas,tautybe=Y2012$tautybe,
                 uzsienio_kalbos=Y2012$uzsienio_kalbos,
                 buves=Y2012$buves,turtas=Y2012$turtas,narys=Y2012$narys
                 #,sunkus_nusikaltimas=Y2012$sunkus_nusikaltimas,pripazintas_kaltu=Y2012$pripazintas_kaltu,neatlikta_bausme=Y2012$neatlikta_bausme
)