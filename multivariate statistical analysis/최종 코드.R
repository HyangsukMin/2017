ha<-read.csv("BLI 2016.csv")
library(ggplot2)
library(RColorBrewer)
ha<-ha[-29,c(2,11)]
p<-data.frame(LOCATION="TOTAL_MEAN",Life.Satisfaction=mean(ha$Life.Satisfaction))
ha<-rbind(ha,p)
a<-ha
a$col<-factor(ifelse(a$LOCATION=="KOR","대한민국",
              ifelse(a$LOCATION=="TOTAL_MEAN","평균","그외")),levels=c("대한민국","평균","그외"))
a$LOCATION<-factor(a$LOCATION,levels=a[order(a$Life.Satisfaction),]$LOCATION)
windows()
ggplot(a,aes(x=LOCATION,y=Life.Satisfaction,group=col,fill=col))+
  geom_bar(stat="identity")+scale_fill_brewer(palette = "Pastel1",name="국가")+
  ggtitle("OECD국가 삶의 만족도 순위")+ylab("삶의 만족도")+xlab("국가")+
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=20),axis.title = element_text(face="bold", size=15),axis.text.x = element_text(angle = 90, hjust = 1))


ha<-read.csv("BLI 2016.csv")
bli<-read.csv("betterlife_na.csv")
hab<-merge(ha,bli[,c(1,28:34)],by="LOCATION")
row.names(hab)<-hab[,1]
hab<-hab[,-1:-2]
head(hab)
library(psych)
pairs.panels(hab)


#높을수록 좋은 것으로 바꾸기 (corruption ,gini)
hab$corruption=with(hab, max(corruption)-corruption)
hab$gini=with(hab, max(gini)-gini)

#1. 요인분석
hab_s<-scale(hab)
hab_s<-as.data.frame(hab_s)
fa<-factanal(hab_s[,-9],2)
print(fa,digits = 2,sort=T)
fa<-factanal(hab_s[,-9],3)
print(fa,digits = 2,sort=T)
fa<-factanal(hab_s[,-9],4)## p-value가 0.05를 넘어감. 3개로 결정!
print(fa,digits = 2,sort=T)

##인자 개수 결정 --> 3개
scree(fa$correlation,factors=TRUE,pc=FALSE)

library(GPArotation)
library(psych)
fa2<-fa(hab_s[,-9],3,rotate="varimax")
fa.diagram(fa2)

print(fa2,digits = 2,sort=T)

1-fa2$unique
#4개의 요인에 의해서 각 변수의 변동량은 다음과 같은 수준으로 설명됨.

hab_efa<-fa2$scores
pairs.panels(hab_efa)

#	인자간 상관성이 거의 없음.
hab_efa<-cbind(hab_s[,9],hab_efa)
hab_efa<-as.data.frame(hab_efa)
colnames(hab_efa)[1]<-c("life_satisfaction")
head(hab_efa)

##2. 군집분석
###군집 개수 결정1
ess=c()
for (k in 1:10){
  km=kmeans(hab_efa, k, nstart=10)
  ess[k]=km$tot.withinss}
plot(ess,type='l')+title("적절한 군집 개수는?")
points(ess,pch=19) 

##군집 개수 결정2 ##
library(NbClust)
nc <- NbClust(hab_efa, min.nc=2, max.nc=10, method="kmeans")
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")

#	군집 수는 3개로 결정
library(factoextra)
library(cluster)
set.seed(12)
km <- kmeans(hab_efa,3, nstart=10)
clusplot(hab_efa, km$cluster,  main = "Cluster plot",
         color=TRUE, labels = 2, lines = 0)

fviz_cluster(km,hab_efa)

hab_efa$cluster<-km$cluster
tapply(hab_efa$life_satisfaction,hab_efa$cluster,mean)

##군집수가 3개 이상일 때, 일원배치 분산분석
##군집수가 3개 이상일 때, 일원배치 분산분석
##군집수가 3개 이상일 때, 일원배치 분산분석
bartlett.test(life_satisfaction~cluster,data=hab_efa) #오차의 등분산성 가정 만족.
a<-aov(life_satisfaction~cluster,data=hab_efa)
summary(a)

##그래프 그려보기
hab_efa$cluster<-km$cluster
hab_efa$col<-ifelse(hab_efa$cluster==1,"red",
                    ifelse(hab_efa$cluster==2,"green", "blue"))

a.g<-ggplot(hab_efa, aes(x=MR1,y=life_satisfaction,label=rownames(hab_efa)))+geom_point(aes(color=col))+ylab("삶의만족도")+xlab("국민특성")+
  ggtitle("삶의 만족도와 국민특성과의 관계")+
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=20),axis.title = element_text(face="bold", size=12))+
  scale_color_discrete(name="군집",labels=c("불행한나라들","덜행복한나라들","행복한나라들"))+geom_text(aes(color=col),hjust=0, vjust=0)

b.g<-ggplot(hab_efa, aes(x=MR2,y=life_satisfaction,label=rownames(hab_efa)))+geom_point(aes(color=col))+ylab("삶의만족도")+xlab("복지")+
  ggtitle("삶의 만족도와 복지와의 관계")+
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=20),axis.title = element_text(face="bold", size=12))+
  scale_color_discrete(name="군집",labels=c("불행한나라들","덜행복한나라들","행복한나라들"))+geom_text(aes(color=col),hjust=0, vjust=0)

c.g<-ggplot(hab_efa, aes(x=MR3,y=life_satisfaction,label=rownames(hab_efa)))+geom_point(aes(color=col))+ylab("삶의만족도")+xlab("사회적분위기")+
  ggtitle("삶의 만족도와 사회적분위기와의 관계")+
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=20),axis.title = element_text(face="bold", size=12))+
  scale_color_discrete(name="군집",labels=c("불행한나라들","덜행복한나라들","행복한나라들"))+geom_text(aes(color=col),hjust=0, vjust=0)

a.g;b.g;c.g
#3.회귀분석 군집분석에서 요인 MR1에 속한 변수와 삶의 만족도 간의 관계를 파악하고자 함.
k1<-row.names(hab_efa[hab_efa$cluster==1,])
head(k1)
k2<-row.names(hab_efa[hab_efa$cluster==3,])
head(k2)
ha<-read.csv("BLI 2016.csv")
head(ha)
bli<-read.csv("betterlife_na.csv")
hab<-merge(ha,bli[,c(1,28:34)],by="LOCATION")
hab<-hab[hab$LOCATION %in% c(k1,k2),]
row.names(hab)<-hab[,1]
hab<-hab[,-1:-2]
head(hab)
hab<-hab[row.names(hab)!="TUR",]
#####회귀분석 시작######################
hab<-hab[,c("Life.Satisfaction","Health","Generosity","corruption","Housing","Income","confingov","Jobs","Community","Civic.engagement")]
vars=colnames(hab)[c(-1,-2)]
var<-"Life.Satisfaction~Health"
for (i in vars[-1]){var<-paste(var,"+",i,sep="")}
var
fit2<-step(lm(Life.Satisfaction~1,data=hab),scope=var,direction="both")
summary(fit2)

다른 변수가 동일할 때에, 
Corruption이 0.1단위 올라가면 삶의 만족도가 -0.32798단위 감소하고,
Health가 1단위 올라가면 삶의 만족도가 1.0480단위 상승하고,
Jobs가 1단위 올라가면 삶의 만족도가 0.7669단위 상승하고,
Housing이 1단위 올라가면 삶의 만족도가 -0.4358단위 감소한다.  여기서는 해석이 애매해서 고려 안함.
par(mfrow=c(2,2))
plot(fit2)

	잔차분석 그래프가 약간 굽어 있긴 하지만 심하진 않음.
	Cook’s distance를 넘는 이상치는 없음. 
	설명변수에 비해서 데이터의 양이 적은 것이 문제임. 
plot(fit2$residuals)
library(car)
vif(fit2) 

Vif 값 5 기준으로 변수 간 다중 공선성 없음.
###각 설명 변수에 대해서 그래프 그리기 우리나라 위치 파악
hab$cluster<-ifelse(row.names(hab) %in% k1,"행복한나라들","불행한나라들")
hab$cluster<-ifelse(hab$LOCATION=="KOR","대한민국",hab$cluster)
hab$cluster<-factor(hab$cluster)
hab$LOCATION<-row.names(hab)
hab$cluster<-factor(hab$cluster,levels=c("대한민국","행복한나라들","불행한나라들"))

hab$LOCATION<-factor(hab$LOCATION,levels = hab[order(hab$corruption),]$LOCATION)
library(ggplot2)
ggplot(hab, aes(x=LOCATION,y=corruption,label=LOCATION))+geom_bar(stat="identity",aes(fill=cluster))+ylab("정치부패인식")+xlab("OECD국가")+
  ggtitle("정치부패")+theme(plot.title = element_text(hjust = 0.5,face="bold",size=20),axis.title = element_text(face="bold", size=15),axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_brewer(palette = "Pastel1",name="국가")

	우리나라는 정치부패인식이 다른 나라에 비해서 높게 나타남.
hab$LOCATION<-factor(hab$LOCATION,levels = hab[order(hab$Health),]$LOCATION)
library(ggplot2)
ggplot(hab, aes(x=LOCATION,y=Health,label=LOCATION))+geom_bar(stat="identity",aes(fill=cluster))+ylab("건강")+xlab("OECD국가")+
  ggtitle("건강")+theme(plot.title = element_text(hjust = 0.5,face="bold",size=20),axis.title = element_text(face="bold", size=15),axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_brewer(palette = "Pastel1",name="국가")

	우리나라는 다른 국가에 비해 건강에 대한 개인의 인식이 나쁨.
hab$LOCATION<-factor(hab$LOCATION,levels = hab[order(hab$Jobs),]$LOCATION)
library(ggplot2)
ggplot(hab, aes(x=LOCATION,y=Jobs,label=LOCATION))+geom_bar(stat="identity",aes(fill=cluster))+ylab("일자리")+xlab("OECD국가")+
  ggtitle("일자리")+
  theme(plot.title = element_text(hjust = 0.5,face="bold",size=20),axis.title = element_text(face="bold", size=15),axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_brewer(palette = "Pastel1",name="국가")
	우리나라는 일자리 부분은 불행한 나라 중에서는 상위권 이지만 행복한 나라에 비해서는 여전히 낮은 수준임.



