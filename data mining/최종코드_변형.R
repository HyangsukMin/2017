library(dplyr)
library(data.table)
library(caret)
library(wordVectors)
rm(list=ls())
setwd("C:\\Users\\Hyangsuk_Min\\Desktop\\데마팀플\\원데이터")
cs.dt <- fread("train_profiles.csv")
tr.dt <- fread("train_clickstreams.tab"); tr.dt[,CUS_ID:= as.numeric(CUS_ID)]

#####0. 새로운 NEW_NM생성####
###TRAIN
tr.dt$NEW_NM<-tr.dt$ACT_NM
##
tr.dt$NEW_NM<-ifelse(tr.dt$ACT_NM =="포털지식검색",tr.dt$SITE_NM,tr.dt$NEW_NM)
tr.dt$NEW_NM<-ifelse(tr.dt$SITE_NM %in% c("zum","네이버","네이트","다음","야후!",
                                          "검색zum","네이버 검색","네이트 검색","다음 검색","야후! 검색",
                                          "구글 Gmail","네이버 메일","네이트 메일","다음 메일","코리아닷컴 메일","드림위즈 메일","야후! 메일",
                                          "구글 Maps","네이버 지도","네이트 지도","다음 지도",
                                          "Bing","구글","지식로그","큐로보 - 로봇검색"),tr.dt$SITE_NM,tr.dt$NEW_NM)
##
tr.dt$NEW_NM<-ifelse(tr.dt$SITE_NM %in% c("네이버 카페","다음 카페","싸이월드 클럽",
                                          "네이버 블로그","다음 블로그","야후! 블로그",
                                          "페이스북","싸이월드 미니홈피","트위터","구글 Plus","미투데이",
                                          "네이버 오픈캐스트","다음 뷰",
                                          "뽐뿌",
                                          "싸이월드","순팽이닷컴"),tr.dt$SITE_NM,tr.dt$NEW_NM)

##
tr.dt$NEW_NM<-ifelse(tr.dt$SITE_NM %in% c("G마켓","네이버 샵N","11번가","옥션","인터파크","옥션 중고장터",
                                          "오가게","미아마스번","11am","톰앤래빗","다크빅토리","립합","다홍","소녀감성","쥬엔","도드리","프롬비기닝",
                                          "롯데닷컴","CJmall","롯데i몰","현대Hmall","신세계몰","GS SHOP","AK몰","NS Mall","엘롯데","디앤샵",
                                          "쿠팡","위메프","티몬","그루폰 코리아","CJ오클랏","원어데이",
                                          "에누리닷컴","다나와","어바웃 비비",
                                          "하프클럽","아이스타일24","LG패션샵","패션플러스",
                                          "멋남","지니프","조군샵","붐스타일","키작은남자"),tr.dt$SITE_NM,tr.dt$NEW_NM)

##
tr.dt$NEW_NM<-ifelse(tr.dt$SITE_NM %in% c("미디어다음","네이버 뉴스","네이트 뉴스","야후! 미디어","뉴스zum","조인스 뉴스",
                                          "중앙일보","조선닷컴","서울신문","한겨레","세계닷컴","문화일보","동아닷컴","경향닷컴",
                                          "스포츠조선","OSEN","스포츠동아","일간스포츠","스포츠서울닷컴","스포츠투데이",
                                          "아시아경제","서울경제신문","조선비즈닷컴","한국경제","이데일리","머니투데이",
                                          "마이데일리","노컷뉴스","오마이뉴스","뉴데일리","미디어오늘","데일리안",
                                          "MBN","YTN","머니투데이 방송","SBS CNBC",
                                          "디지털타임스","ZDNet Korea","전자신문",
                                          "뉴스엔","티브이데일리","TV리포트","머니투데이 스타뉴스"),tr.dt$SITE_NM,tr.dt$NEW_NM)


##
tr.dt$NEW_NM<-ifelse(tr.dt$SITE_NM  %in% c("YouTube","아프리카TV","판도라TV","pooq",
                                           "네이버 만화","다음 만화속세상","네이트 만화·툰도시",
                                           "네이버 영화","다음 영화",
                                           "다음 TV팟","네이버 TV캐스트",
                                           "네이버 뮤직","다음 뮤직","싸이월드 싸이BGM",
                                           "일베저장소","오늘의 유머","멜론","엠넷","벅스","올레뮤직"),tr.dt$SITE_NM,tr.dt$NEW_NM)

##
tr.dt$NEW_NM<-ifelse(tr.dt$SITE_NM %in% c("잡코리아","사람인","고용정보 워크넷","커리어"),tr.dt$SITE_NM,tr.dt$NEW_NM)

##
tr.dt$NEW_NM<-ifelse(tr.dt$NEW_NM =="시중은행",tr.dt$SITE_NM,tr.dt$NEW_NM)
tr.dt$NEW_NM<-ifelse(tr.dt$SITE_NM %in% c("신한카드","현대카드","삼성카드","롯데카드","KB국민카드","비씨카드","하나SK카드","외환카드"),tr.dt$SITE_NM,tr.dt$NEW_NM)

##
tr.dt$NEW_NM<-ifelse(tr.dt$SITE_NM %in% c("민원24","국세청 홈텍스","국세청 연말정산간소화서비스",
                                          "대법원 인터넷등기소","대법원","Warning",
                                          "기상청","국세청",
                                          "국세청 전자세금계산서 e세로",
                                          "코레일 한국철도공사","한국토지주택공사"),tr.dt$SITE_NM,tr.dt$NEW_NM)
##
tr.dt$NEW_NM<-ifelse(tr.dt$SITE_NM %in% c("EBSi","메가스터디","이투스",
                                          "에듀윌","이그잼"),tr.dt$SITE_NM,tr.dt$NEW_NM)
##
tr.dt$NEW_NM<-ifelse(tr.dt$SITE_NM %in% c("피망","넷마블","한게임","넥슨","엠게임",
                                          "던전앤파이터","리니지","메이플스토리",
                                          "네이버 야구9단","피파 온라인 3",
                                          "인벤","루리웹닷컴"),tr.dt$SITE_NM,tr.dt$NEW_NM)


write.csv(tr.dt,"train_clickstreams(more_detailed).csv",row.names = F)

####TEST
library(dplyr)
library(data.table)
library(caret)
library(wordVectors)
rm(list=ls())
setwd("C:\\Users\\Hyangsuk_Min\\Desktop\\데마팀플\\원데이터")
tr.dt <- fread("test_clickstreams.tab"); tr.dt[,CUS_ID:= as.numeric(CUS_ID)]

tr.dt$NEW_NM<-tr.dt$ACT_NM
##
tr.dt$NEW_NM<-ifelse(tr.dt$ACT_NM =="포털지식검색",tr.dt$SITE_NM,tr.dt$NEW_NM)
tr.dt$NEW_NM<-ifelse(tr.dt$SITE_NM %in% c("zum","네이버","네이트","다음","야후!",
                                          "검색zum","네이버 검색","네이트 검색","다음 검색","야후! 검색",
                                          "구글 Gmail","네이버 메일","네이트 메일","다음 메일","코리아닷컴 메일","드림위즈 메일","야후! 메일",
                                          "구글 Maps","네이버 지도","네이트 지도","다음 지도",
                                          "Bing","구글","지식로그","큐로보 - 로봇검색"),tr.dt$SITE_NM,tr.dt$NEW_NM)
##
tr.dt$NEW_NM<-ifelse(tr.dt$SITE_NM %in% c("네이버 카페","다음 카페","싸이월드 클럽",
                                          "네이버 블로그","다음 블로그","야후! 블로그",
                                          "페이스북","싸이월드 미니홈피","트위터","구글 Plus","미투데이",
                                          "네이버 오픈캐스트","다음 뷰",
                                          "뽐뿌",
                                          "싸이월드","순팽이닷컴"),tr.dt$SITE_NM,tr.dt$NEW_NM)

##
tr.dt$NEW_NM<-ifelse(tr.dt$SITE_NM %in% c("G마켓","네이버 샵N","11번가","옥션","인터파크","옥션 중고장터",
                                          "오가게","미아마스번","11am","톰앤래빗","다크빅토리","립합","다홍","소녀감성","쥬엔","도드리","프롬비기닝",
                                          "롯데닷컴","CJmall","롯데i몰","현대Hmall","신세계몰","GS SHOP","AK몰","NS Mall","엘롯데","디앤샵",
                                          "쿠팡","위메프","티몬","그루폰 코리아","CJ오클랏","원어데이",
                                          "에누리닷컴","다나와","어바웃 비비",
                                          "하프클럽","아이스타일24","LG패션샵","패션플러스",
                                          "멋남","지니프","조군샵","붐스타일","키작은남자"),tr.dt$SITE_NM,tr.dt$NEW_NM)

##
tr.dt$NEW_NM<-ifelse(tr.dt$SITE_NM %in% c("미디어다음","네이버 뉴스","네이트 뉴스","야후! 미디어","뉴스zum","조인스 뉴스",
                                          "중앙일보","조선닷컴","서울신문","한겨레","세계닷컴","문화일보","동아닷컴","경향닷컴",
                                          "스포츠조선","OSEN","스포츠동아","일간스포츠","스포츠서울닷컴","스포츠투데이",
                                          "아시아경제","서울경제신문","조선비즈닷컴","한국경제","이데일리","머니투데이",
                                          "마이데일리","노컷뉴스","오마이뉴스","뉴데일리","미디어오늘","데일리안",
                                          "MBN","YTN","머니투데이 방송","SBS CNBC",
                                          "디지털타임스","ZDNet Korea","전자신문",
                                          "뉴스엔","티브이데일리","TV리포트","머니투데이 스타뉴스"),tr.dt$SITE_NM,tr.dt$NEW_NM)


##
tr.dt$NEW_NM<-ifelse(tr.dt$SITE_NM  %in% c("YouTube","아프리카TV","판도라TV","pooq",
                                           "네이버 만화","다음 만화속세상","네이트 만화·툰도시",
                                           "네이버 영화","다음 영화",
                                           "다음 TV팟","네이버 TV캐스트",
                                           "네이버 뮤직","다음 뮤직","싸이월드 싸이BGM",
                                           "일베저장소","오늘의 유머","멜론","엠넷","벅스","올레뮤직"),tr.dt$SITE_NM,tr.dt$NEW_NM)

##
tr.dt$NEW_NM<-ifelse(tr.dt$SITE_NM %in% c("잡코리아","사람인","고용정보 워크넷","커리어"),tr.dt$SITE_NM,tr.dt$NEW_NM)

##
tr.dt$NEW_NM<-ifelse(tr.dt$NEW_NM =="시중은행",tr.dt$SITE_NM,tr.dt$NEW_NM)
tr.dt$NEW_NM<-ifelse(tr.dt$SITE_NM %in% c("신한카드","현대카드","삼성카드","롯데카드","KB국민카드","비씨카드","하나SK카드","외환카드"),tr.dt$SITE_NM,tr.dt$NEW_NM)

##
tr.dt$NEW_NM<-ifelse(tr.dt$SITE_NM %in% c("민원24","국세청 홈텍스","국세청 연말정산간소화서비스",
                                          "대법원 인터넷등기소","대법원","Warning",
                                          "기상청","국세청",
                                          "국세청 전자세금계산서 e세로",
                                          "코레일 한국철도공사","한국토지주택공사"),tr.dt$SITE_NM,tr.dt$NEW_NM)
##
tr.dt$NEW_NM<-ifelse(tr.dt$SITE_NM %in% c("EBSi","메가스터디","이투스",
                                          "에듀윌","이그잼"),tr.dt$SITE_NM,tr.dt$NEW_NM)
##
tr.dt$NEW_NM<-ifelse(tr.dt$SITE_NM %in% c("피망","넷마블","한게임","넥슨","엠게임",
                                          "던전앤파이터","리니지","메이플스토리",
                                          "네이버 야구9단","피파 온라인 3",
                                          "인벤","루리웹닷컴"),tr.dt$SITE_NM,tr.dt$NEW_NM)


write.csv(tr.dt,"test_clickstreams(more_detailed).csv",row.names = F)



#####1.-1 변수생성TRAIN######
rm(list=ls())
library(lubridate)
library(dplyr)
library(data.table)
setwd("C:\\Users\\Hyangsuk_Min\\Desktop\\데마팀플\\원데이터")

tc<-fread("train_clickstreams(more_detailed).csv",stringsAsFactors = F)
tc2<-fread("test_clickstreams(more_detailed).csv",stringsAsFactors = F)
tc2<-tc2[tc2$NEW_NM %in% unique(tc$NEW_NM),]
tc<-tc[tc$NEW_NM %in% unique(tc2$NEW_NM),]

cs.dt <- fread("train_profiles.csv")
tr<-tc
sk<-fread("train_searchkeywords.tab")
tr<-na.omit(tr)
tr$TIME_ID <-ymd_h(tr$TIME_ID)
tr$date <- date(tr$TIME_ID)
tr$year <- year(tr$TIME_ID)
tr$month <- month(tr$TIME_ID)
tr$day <- day(tr$TIME_ID)
tr$wkday <- wday(tr$TIME_ID)
tr$TIME<-hour(tr$TIME_ID)
tr$TIME2<-ifelse(tr$TIME<=5 & tr$TIME>=2,"0205",
                 ifelse(tr$TIME<=9 & tr$TIME>=6,"0609",
                        ifelse(tr$TIME>=10 & tr$TIME<=13,"1013",
                               ifelse(tr$TIME>=14 & tr$TIME<=17,"1417",
                                      ifelse(tr$TIME>=18&tr$TIME<=21,"1821",
                                             ifelse(tr$TIME>=22 &tr$TIME<=23,"2201",
                                                    ifelse(tr$TIME==0 | tr$TIME==1,"2201","?")))))))
tr$TIME2<-factor(tr$TIME2)
library(reshape)
detach(package:plyr)
cs.t1<-cast(tr,CUS_ID~NEW_NM,sum,value="SITE_CNT")
#cs.t1<-cs.t1[,-300]
cs.t12<-cs.t1[,-1]/rowSums(cs.t1[,-1])*100
cs.t12<-cbind("CUS_ID"=1:2500,cs.t12)
#names(cs.t12)[-1]<-paste("CP",1:(dim(cs.t12)[2]-1),sep="")
cs.t13<-tr %>% group_by(CUS_ID,NEW_NM) %>% summarise(Total=sum(SITE_CNT)) %>% group_by(CUS_ID) %>% summarise(CCOV=sd(Total)/mean(Total))

cs.t2<-cast(tr,CUS_ID~NEW_NM,sum,value="ST_TIME")
#cs.t2<-cs.t2[,-300]
cs.t22<-cs.t2[,-1]/rowSums(cs.t2[,-1])*100
cs.t22<-cbind("CUS_ID"=1:2500,cs.t22)
#names(cs.t22)[-1]<-paste("CD",1:(dim(cs.t22)[2]-1),sep="")
cs.t23<-tr %>% group_by(CUS_ID,NEW_NM) %>% summarise(Total=sum(ST_TIME)) %>% group_by(CUS_ID) %>% summarise(CCOV2=sd(Total)/mean(Total))

cs.t3<-tr %>% group_by(CUS_ID) %>% summarise(DWELLTIME=sum(ST_TIME))
cs.t4<-tr %>% group_by(CUS_ID) %>% summarise(PAGEVIEW=sum(SITE_CNT))

cs.t5 <- tr %>% group_by(CUS_ID) %>% select(CUS_ID, SITE) %>% summarize(reppagenum=n())
cs.t6 <- tr %>% group_by(CUS_ID) %>% select(CUS_ID, SITE) %>% distinct(SITE) %>% summarize(pagenum=n())

cs.t7 <- cast(tr,CUS_ID~TIME,sum,value = "SITE_CNT")
cs.t72<-cs.t7[,-1]/rowSums(cs.t7[,-1])*100
names(cs.t72)<-paste("Tp",0:23,sep="")
cs.t72<-cbind("CUS_ID"=1:2500,cs.t72)
cs.t73<-tr %>% group_by(CUS_ID,TIME) %>% summarise(Total=sum(SITE_CNT)) %>% group_by(CUS_ID) %>% summarise(TCOV=sd(Total)/mean(Total))

cs.t8 <- cast(tr,CUS_ID~TIME,sum,value = "ST_TIME")
cs.t82<-cs.t8[,-1]/rowSums(cs.t8[,-1])*100
names(cs.t82)<-paste("Td",0:23,sep="")
cs.t82<-cbind("CUS_ID"=1:2500,cs.t82)
cs.t83<-tr %>% group_by(CUS_ID,TIME) %>% summarise(Total=sum(ST_TIME)) %>% group_by(CUS_ID) %>% summarise(TCOV2=sd(Total)/mean(Total))

cs.t9 <- cast(tr,CUS_ID~TIME2,sum,value = "SITE_CNT")
cs.t92<-cs.t9[,-1]/rowSums(cs.t9[,-1])*100
names(cs.t92)<-paste("T2p",names(cs.t92),sep="")
cs.t92<-cbind("CUS_ID"=1:2500,cs.t92)
cs.t93<-tr %>% group_by(CUS_ID,TIME2) %>% summarise(Total=sum(SITE_CNT)) %>% group_by(CUS_ID) %>% summarise(T2COV=sd(Total)/mean(Total))

cs.t10 <- cast(tr,CUS_ID~TIME2,sum,value = "ST_TIME")
cs.t102<-cs.t10[,-1]/rowSums(cs.t10[,-1])*100
names(cs.t102)<-paste("T2d",names(cs.t102),sep="")
cs.t102<-cbind("CUS_ID"=1:2500,cs.t102)
cs.t103<-tr %>% group_by(CUS_ID,TIME2) %>% summarise(Total=sum(ST_TIME)) %>% group_by(CUS_ID) %>% summarise(T2COV2=sd(Total)/mean(Total))

cs.tf11 <- tr %>% group_by(CUS_ID, date) %>% summarize(N = n()) %>% group_by(CUS_ID) %>% summarize(VDAYS= n())

cs.tf12 <- cast(tr,CUS_ID~wkday,sum,value="SITE_CNT")
cs.tf122<-cs.tf12[,-1]/rowSums(cs.tf12[,-1])*100
names(cs.tf122)<-c("psun", "pmon", "ptue", "pwed", "pthu", "pfri", "psat")
cs.tf122<-cbind("CUS_ID"=1:2500,cs.tf122)
cs.tf123<-tr %>% group_by(CUS_ID,wkday) %>% summarise(Total=sum(SITE_CNT)) %>% group_by(CUS_ID) %>% summarise(WCOV=sd(Total)/mean(Total))

cs.tf13 <- cast(tr,CUS_ID~wkday,sum,value="ST_TIME")
cs.tf132<-cs.tf13[,-1]/rowSums(cs.tf13[,-1])*100
names(cs.tf132)<-c("dsun","dmon", "dtue", "dwed", "dthu", "dfri", "dsat")
cs.tf132<-cbind("CUS_ID"=1:2500,cs.tf132)
cs.tf133<-tr %>% group_by(CUS_ID,wkday) %>% summarise(Total=sum(ST_TIME)) %>% group_by(CUS_ID) %>% summarise(WCOV2=sd(Total)/mean(Total))

cs.tf14<-sk %>% group_by(CUS_ID) %>% summarise(STOT=n())
cs.tf143<-sk%>% group_by(CUS_ID) %>% summarise(SCOV=sd(QRY_CNT)/mean(QRY_CNT))

#cs.t12,cs.t13,cs.t22,cs.t23,cs.t3,cs.t4,cs.t5,cs.t6,cs.t72,cs.t73,cs.t82,cs.t83,cs.t92,cs.t93,cs.t102,cs.t103,cs.tf11,cs.tf122,cs.tf123,cs.tf132,cs.tf133
train_f<-cs.dt %>% 
  left_join(cs.t12,by="CUS_ID") %>% 
  left_join(cs.t13,by="CUS_ID") %>%
  left_join(cs.t22,by="CUS_ID") %>%
  left_join(cs.t23,by="CUS_ID") %>%
  left_join(cs.t3,by="CUS_ID") %>%
  left_join(cs.t4,by="CUS_ID") %>%
  left_join(cs.t5,by="CUS_ID") %>%
  left_join(cs.t6,by="CUS_ID") %>%
  left_join(cs.t72,by="CUS_ID") %>%
  left_join(cs.t73,by="CUS_ID") %>%
  left_join(cs.t82,by="CUS_ID") %>%
  left_join(cs.t83,by="CUS_ID") %>%
  left_join(cs.t92,by="CUS_ID") %>%
  left_join(cs.t93,by="CUS_ID") %>%
  left_join(cs.t102,by="CUS_ID") %>%
  left_join(cs.t103,by="CUS_ID") %>%
  left_join(cs.tf11,by="CUS_ID") %>%
  left_join(cs.tf122,by="CUS_ID") %>%
  left_join(cs.tf123,by="CUS_ID") %>%
  left_join(cs.tf132,by="CUS_ID") %>%
  left_join(cs.tf133,by="CUS_ID") %>%
  left_join(cs.tf14,by="CUS_ID") %>%
  left_join(cs.tf143,by="CUS_ID")


train_f[is.na(train_f)]<-0
sum(is.na(train_f))

write.csv(train_f,"train_fN_170609-1.csv",row.names = F)


#####1.-2 변수생성TEST######
library(lubridate)
library(dplyr)
library(data.table)
setwd("C:\\Users\\Hyangsuk_Min\\Desktop\\데마팀플\\원데이터")
tr<-tc2
sk<-fread("test_searchkeywords.tab")
tr<-na.omit(tr)
tr$TIME_ID <-ymd_h(tr$TIME_ID)
tr$date <- date(tr$TIME_ID)
tr$year <- year(tr$TIME_ID)
tr$month <- month(tr$TIME_ID)
tr$day <- day(tr$TIME_ID)
tr$wkday <- wday(tr$TIME_ID)
tr$TIME<-hour(tr$TIME_ID)
tr$TIME2<-ifelse(tr$TIME<=5 & tr$TIME>=2,"0205",
                 ifelse(tr$TIME<=9 & tr$TIME>=6,"0609",
                        ifelse(tr$TIME>=10 & tr$TIME<=13,"1013",
                               ifelse(tr$TIME>=14 & tr$TIME<=17,"1417",
                                      ifelse(tr$TIME>=18&tr$TIME<=21,"1821",
                                             ifelse(tr$TIME>=22 &tr$TIME<=23,"2201",
                                                    ifelse(tr$TIME==0 | tr$TIME==1,"2201","?")))))))
tr$TIME2<-factor(tr$TIME2)
library(reshape)
detach(package:plyr)
cs.t1<-cast(tr,CUS_ID~NEW_NM,sum,value="SITE_CNT")
#cs.t1<-cs.t1[,-300]
cs.t12<-cs.t1[,-1]/rowSums(cs.t1[,-1])*100
cs.t12<-cbind("CUS_ID"=2501:5000,cs.t12)
rm(cs.t1)
#names(cs.t12)[-1]<-paste("CP",1:(dim(cs.t12)[2]-1),sep="")
cs.t13<-tr %>% group_by(CUS_ID,NEW_NM) %>% summarise(Total=sum(SITE_CNT)) %>% group_by(CUS_ID) %>% summarise(CCOV=sd(Total)/mean(Total))

cs.t2<-cast(tr,CUS_ID~NEW_NM,sum,value="ST_TIME")
#cs.t2<-cs.t2[,-300]
cs.t22<-cs.t2[,-1]/rowSums(cs.t2[,-1])*100
cs.t22<-cbind("CUS_ID"=2501:5000,cs.t22)
rm(cs.t2)
#names(cs.t22)[-1]<-paste("CD",1:(dim(cs.t22)[2]-1),sep="")
cs.t23<-tr %>% group_by(CUS_ID,NEW_NM) %>% summarise(Total=sum(ST_TIME)) %>% group_by(CUS_ID) %>% summarise(CCOV2=sd(Total)/mean(Total))

cs.t3<-tr %>% group_by(CUS_ID) %>% summarise(DWELLTIME=sum(ST_TIME))
cs.t4<-tr %>% group_by(CUS_ID) %>% summarise(PAGEVIEW=sum(SITE_CNT))

cs.t5 <- tr %>% group_by(CUS_ID) %>% select(CUS_ID, SITE) %>% summarize(reppagenum=n())
cs.t6 <- tr %>% group_by(CUS_ID) %>% select(CUS_ID, SITE) %>% distinct(SITE) %>% summarize(pagenum=n())

cs.t7 <- cast(tr,CUS_ID~TIME,sum,value = "SITE_CNT")
cs.t72<-cs.t7[,-1]/rowSums(cs.t7[,-1])*100
names(cs.t72)<-paste("Tp",0:23,sep="")
cs.t72<-cbind("CUS_ID"=2501:5000,cs.t72)
cs.t73<-tr %>% group_by(CUS_ID,TIME) %>% summarise(Total=sum(SITE_CNT)) %>% group_by(CUS_ID) %>% summarise(TCOV=sd(Total)/mean(Total))

cs.t8 <- cast(tr,CUS_ID~TIME,sum,value = "ST_TIME")
cs.t82<-cs.t8[,-1]/rowSums(cs.t8[,-1])*100
names(cs.t82)<-paste("Td",0:23,sep="")
cs.t82<-cbind("CUS_ID"=2501:5000,cs.t82)
cs.t83<-tr %>% group_by(CUS_ID,TIME) %>% summarise(Total=sum(ST_TIME)) %>% group_by(CUS_ID) %>% summarise(TCOV2=sd(Total)/mean(Total))

cs.t9 <- cast(tr,CUS_ID~TIME2,sum,value = "SITE_CNT")
cs.t92<-cs.t9[,-1]/rowSums(cs.t9[,-1])*100
names(cs.t92)<-paste("T2p",names(cs.t92),sep="")
cs.t92<-cbind("CUS_ID"=2501:5000,cs.t92)
cs.t93<-tr %>% group_by(CUS_ID,TIME2) %>% summarise(Total=sum(SITE_CNT)) %>% group_by(CUS_ID) %>% summarise(T2COV=sd(Total)/mean(Total))

cs.t10 <- cast(tr,CUS_ID~TIME2,sum,value = "ST_TIME")
cs.t102<-cs.t10[,-1]/rowSums(cs.t10[,-1])*100
names(cs.t102)<-paste("T2d",names(cs.t102),sep="")
cs.t102<-cbind("CUS_ID"=2501:5000,cs.t102)
cs.t103<-tr %>% group_by(CUS_ID,TIME2) %>% summarise(Total=sum(ST_TIME)) %>% group_by(CUS_ID) %>% summarise(T2COV2=sd(Total)/mean(Total))

cs.tf11 <- tr %>% group_by(CUS_ID, date) %>% summarize(N = n()) %>% group_by(CUS_ID) %>% summarize(VDAYS= n())

cs.tf12 <- cast(tr,CUS_ID~wkday,sum,value="SITE_CNT")
cs.tf122<-cs.tf12[,-1]/rowSums(cs.tf12[,-1])*100
names(cs.tf122)<-c("psun", "pmon", "ptue", "pwed", "pthu", "pfri", "psat")
cs.tf122<-cbind("CUS_ID"=2501:5000,cs.tf122)
cs.tf123<-tr %>% group_by(CUS_ID,wkday) %>% summarise(Total=sum(SITE_CNT)) %>% group_by(CUS_ID) %>% summarise(WCOV=sd(Total)/mean(Total))

cs.tf13 <- cast(tr,CUS_ID~wkday,sum,value="ST_TIME")
cs.tf132<-cs.tf13[,-1]/rowSums(cs.tf13[,-1])*100
names(cs.tf132)<-c("dsun","dmon", "dtue", "dwed", "dthu", "dfri", "dsat")
cs.tf132<-cbind("CUS_ID"=2501:5000,cs.tf132)
cs.tf133<-tr %>% group_by(CUS_ID,wkday) %>% summarise(Total=sum(ST_TIME)) %>% group_by(CUS_ID) %>% summarise(WCOV2=sd(Total)/mean(Total))

cs.tf14<-sk %>% group_by(CUS_ID) %>% summarise(STOT=n())
cs.tf143<-sk %>% group_by(CUS_ID) %>% summarise(SCOV=sd(QRY_CNT)/mean(QRY_CNT))
#cs.t12,cs.t13,cs.t22,cs.t23,cs.t3,cs.t4,cs.t5,cs.t6,cs.t72,cs.t73,cs.t82,cs.t83,cs.t92,cs.t93,cs.t102,cs.t103,cs.tf11,cs.tf122,cs.tf123,cs.tf132,cs.tf133
test_f<-cs.t12 %>%
  left_join(cs.t13,by="CUS_ID") %>%
  left_join(cs.t22,by="CUS_ID") %>%
  left_join(cs.t23,by="CUS_ID") %>%
  left_join(cs.t3,by="CUS_ID") %>%
  left_join(cs.t4,by="CUS_ID") %>%
  left_join(cs.t5,by="CUS_ID") %>%
  left_join(cs.t6,by="CUS_ID") %>%
  left_join(cs.t72,by="CUS_ID") %>%
  left_join(cs.t73,by="CUS_ID") %>%
  left_join(cs.t82,by="CUS_ID") %>%
  left_join(cs.t83,by="CUS_ID") %>%
  left_join(cs.t92,by="CUS_ID") %>%
  left_join(cs.t93,by="CUS_ID") %>%
  left_join(cs.t102,by="CUS_ID") %>%
  left_join(cs.t103,by="CUS_ID") %>%
  left_join(cs.tf11,by="CUS_ID") %>%
  left_join(cs.tf122,by="CUS_ID") %>%
  left_join(cs.tf123,by="CUS_ID") %>%
  left_join(cs.tf132,by="CUS_ID") %>%
  left_join(cs.tf133,by="CUS_ID") %>%
  left_join(cs.tf14,by="CUS_ID") %>%
  left_join(cs.tf143,by="CUS_ID")


test_f[is.na(test_f)]<-0
sum(is.na(test_f))

write.csv(test_f,"test_fN_170609-1.csv",row.names = F)

#####1.-3 변수 고르기####
library(mlbench)
library(caret)
library(plyr)
library(dplyr)
library(data.table)
library(FSelector)
##### Data read
train <- read.csv("train_fN_170609-1.csv")
test <- read.csv("test_fN_170609-1.csv")
##### Modeling 
train$GROUP<-factor(train$GROUP,labels =c("F20","F30","F40","M20","M30","M40"))

## another method 
weights <- chi.squared(GROUP~., train[,-1])
print(weights)

train_var = train[,-c(1,2)]
selvar = train_var[,weights$attr_importance !=0]

train_sel = cbind(train[,1:2], selvar)
train_sel$GROUP<-factor(train_sel$GROUP,labels = c("F20-","F30","F40+","M20-","M30","M40+"))

write.csv(train_sel, file = "train_fN_sel_170609.csv", row.names = F)

test_var = test[,-1]
selvar_test = test_var[,weights$attr_importance !=0]

test_sel = cbind(test[,1], selvar_test)
names(test_sel)[1]<-"CUS_ID"
write.csv(test_sel, file = "test_fN_sel_170609.csv", row.names = F)


#####1.-4 모델 돌리기~~~전에 3503개 만들기####
library(data.table)
train1<-read.csv("train_fN_sel_170609.csv")
test1<-read.csv("test_fN_sel_170609.csv")
cs.dt2<-fread("test_public.csv")
cs.dt2$GROUP<-ifelse(cs.dt2$`F20-`==1,"F20-",
                     ifelse(cs.dt2$F30==1,"F30",
                            ifelse(cs.dt2$`F40+`,"F40+",
                                   ifelse(cs.dt2$`M20-`,"M20-",
                                          ifelse(cs.dt2$M30,"M30","M40+")))))
test2<-merge(cs.dt2[,c(1,8)],test1,by="CUS_ID")
train<-rbind(train1,test2)
test<-test1[!(test1$CUS_ID %in% cs.dt2$CUS_ID),]

write.csv(train,"train_fN_sel_3500.csv",row.names=F)
write.csv(test,"test_fN_sel_3500.csv",row.names = F)

#####2. 워드투백 만들기~~ ####
## 3500개 만들기
rm(list=ls())
setwd("C:\\Users\\Hyangsuk_Min\\Desktop\\데마팀플\\원데이터")
# Install & load word2vec package
if(!require(devtools)) install.packages("devtools"); library(devtools)
if(!require(wordVectors)) install_github("bmschmidt/wordVectors"); library(wordVectors)

# Install & load data.table package
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(randomForest)) install.packages("randomForest"); library(randomForest)
if(!require(caret)) install.packages("caret"); library(caret)

cs.dt <- fread("train_profiles.csv")
tr.dt <- fread("train_clickstreams(more_detailed).csv"); tr.dt[,CUS_ID:= as.numeric(CUS_ID)]
cs.dt2<-fread("test_public.csv")
cs.dt2$GROUP<-ifelse(cs.dt2$`F20-`==1,"F20-",
                     ifelse(cs.dt2$F30==1,"F30",
                            ifelse(cs.dt2$`F40+`,"F40+",
                                   ifelse(cs.dt2$`M20-`,"M20-",
                                          ifelse(cs.dt2$M30,"M30","M40+")))))

tr.dt2 <-fread("test_clickstreams(more_detailed).csv"); tr.dt2[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt, CUS_ID); setkey(tr.dt, CUS_ID);setkey(cs.dt2, CUS_ID); setkey(tr.dt2, CUS_ID) 

md.dt1<-merge(cs.dt,tr.dt,by="CUS_ID")
md.dt2<-merge(cs.dt2[,c(1,8)],tr.dt2,by="CUS_ID",all.x=T)
md.dt <- rbind(md.dt1,md.dt2)

###### Make sites sentences 
f <- function(x, t) {
  grp <- md.dt[CUS_ID==x, GROUP][1]
  itemfreq <- table(md.dt[CUS_ID==x,  NEW_NM])
  fitems <- itemfreq[itemfreq >= t]
  act <- names(fitems)
  #  
  sapply(act, function(x) gsub(" ", "_", x))
  set.seed(1)
  #
  as.vector((sapply(1:20, function(x) c(grp, sample(act)))))
}

items <- unlist(sapply(unique(md.dt$CUS_ID), f, 2))
items<-gsub(" ","_",items)

write.table(items, "items-new7(act+site)-3500.txt", eol = " ", quote = F, row.names = F, col.names = F)

model = train_word2vec("items-new7(act+site)-3500.txt","vec-new7(act+site)-3500.bin",vectors=300,threads=1,window=5,cbow=1,iter=5,negative_samples=10, force = T)
#model <- read.binary.vectors("vec-new7(act+site)-3500.bin") # reload the model. 

df<-data.frame()
for (i in unique(md.dt$CUS_ID) ){
  itemfreq <- table(md.dt[CUS_ID==i, NEW_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,model[[names(fitems), average=T]])
  df<-rbind(df,sim)
}

cs.dt1<-rbind(cs.dt,cs.dt2[,c(1,8)])
df<-merge(cs.dt1,df,by="CUS_ID")
write.csv(df,"n2v300_train-3500.csv",row.names = F)


##TEST
md.dt<-tr.dt2[!(tr.dt2$CUS_ID %in% cs.dt2$CUS_ID),]

df<-data.frame()
for (i in unique(md.dt$CUS_ID)){
  itemfreq <- table(md.dt[CUS_ID==i, NEW_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,model[[names(fitems), average=T]])
  df<-rbind(df,sim)
}

write.csv(df,"n2v300_test-3500.csv",row.names = F)


#####1.-5 코사인 유사도 만들어서 추가하기####
rm(list=ls())
cs.dt <- fread("train_profiles.csv")
tr.dt <- fread("train_clickstreams(more_detailed).csv"); tr.dt[,CUS_ID:= as.numeric(CUS_ID)]
cs.dt2<-fread("test_public.csv")
cs.dt2$GROUP<-ifelse(cs.dt2$`F20-`==1,"F20-",
                     ifelse(cs.dt2$F30==1,"F30",
                            ifelse(cs.dt2$`F40+`,"F40+",
                                   ifelse(cs.dt2$`M20-`,"M20-",
                                          ifelse(cs.dt2$M30,"M30","M40+")))))

tr.dt2 <-fread("test_clickstreams(more_detailed).csv"); tr.dt2[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt, CUS_ID); setkey(tr.dt, CUS_ID);setkey(cs.dt2, CUS_ID); setkey(tr.dt2, CUS_ID) 

md.dt1<-merge(cs.dt,tr.dt,by="CUS_ID")
md.dt2<-merge(cs.dt2[,c(1,8)],tr.dt2,by="CUS_ID",all.x=T)
md.dt <- rbind(md.dt1,md.dt2)
model <- read.binary.vectors("vec-new7(act+site).bin") # reload the model. 
df<-data.frame()
cus<-unique(md.dt$CUS_ID)
for (i in cus ){
  itemfreq <- table(md.dt[CUS_ID==i, NEW_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df<-rbind(df,sim1)
}
colnames(df)<-c("CUS_ID","NF20","NF30","NF40","NM20","NM30","NM40")
train<-read.csv("train_fN_sel_3500.csv")
train<-merge(train,df,by="CUS_ID")
write.csv(train,"train_fN_sel+n_3500.csv",row.names = F)


tr2<-tr.dt2[!(tr.dt2$CUS_ID %in% cs.dt2$CUS_ID),]
df<-data.frame()
cus<-unique(tr2$CUS_ID)
for (i in cus ){
  itemfreq <- table(tr2[CUS_ID==i, NEW_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,cosineSimilarity(model[[names(fitems), average=T]], model[[c("F20-","F30","F40+","M20-","M30","M40+"), average=F]]))
  sim1 <- sim[,c("CUS_ID","F20.","F30","F40.","M20.","M30","M40.")]
  df<-rbind(df,sim1)
}
colnames(df)<-c("CUS_ID","NF20","NF30","NF40","NM20","NM30","NM40")
test<-read.csv("test_fN_sel_3500.csv")
test<-merge(test,df,by="CUS_ID")
write.csv(test,"test_fN_sel+n_3500.csv",row.names = F)

#####1.-6 진짜로 모델 돌리기~~~ ####
library(caret)
rm(list = ls())
train<-read.csv("train_fN_sel+n_3500.csv")
test<-read.csv("test_fN_sel+n_3500.csv")
train$GROUP<-make.names(train$GROUP)
control <- trainControl(method="repeatedcv", number=5, repeats=3, classProbs=TRUE, summaryFunction=mnLogLoss)

# Used models
methods <- c("pda","gbm", "nnet","xgbTree","rf") # add methods such as xgbTree, rf, svmRadious, etc.

models <- list()
for (i in 1:length(methods)) {
  set.seed(123)
  print(methods[i])
  model <- train(GROUP ~ .,
                 data = subset(train, select=-CUS_ID),
                 method = methods[i],
                 preProcess="scale",
                 metric = "logLoss",
                 trControl = control)
  summary(model$results$logLoss)
  models[[i]] <- model
}
names(models) <- methods
# Saving and loading models: 
saveRDS(models, "fN_sel_3500_170613.rds") 
# models <- readRDS("models.rds")

# Model comparison
results <- resamples(models)
summary(results)
modelCor(results)

# prediction & submission
for (i in 1:length(methods)) {
  pred <- predict(models[i], test, type="prob")
  fname <- paste("FINSUB_fN_sel+n_3500-", methods[i], ".csv", sep="")
  write.csv(cbind(CUS_ID=test$CUS_ID,pred[[1]]), fname, row.names = F)
}

#####1.-7 앙상블하기####
EN1<-read.csv("FINSUB_fN_sel+n_3500-gbm.csv")
EN2<-read.csv("FINSUB_fN_sel+n_3500-rf.csv")
EN3<-read.csv("FINSUB_fN_sel+n_3500-xgbTree.csv")
pred<-merge(EN1,merge(EN2,EN3,by="CUS_ID"),by="CUS_ID")
nc <- ncol(pred)
nf <- 3
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"FIN-NEW7-3500ENSEMBLE.csv", row.names = F)



#####2.-2모델 돌리기####
train<-read.csv("n2v300_train-3500.csv")
test<-read.csv("n2v300_test-3500.csv")
train$GROUP<-make.names(train$GROUP)
control <- trainControl(method="repeatedcv", number=5, repeats=3, classProbs=TRUE, summaryFunction=mnLogLoss)
# Used models
methods <- c("pda","nnet","svmRadial","xgbTree","gbm") # add methods such as xgbTree, rf, svmRadious, etc.

models <- list()
for (i in 1:length(methods)) {
  set.seed(123)
  print(methods[i])
  model <- train(GROUP ~ .,
                 data = subset(train, select=-CUS_ID),
                 method = methods[i],
                 preProcess="scale",
                 metric = "logLoss",
                 trControl = control)
  summary(model$results$logLoss)
  models[[i]] <- model
}
names(models) <- methods

# Model comparison
results <- resamples(models)
summary(results)
modelCor(results)

# prediction & submission
for (i in 1:length(methods)) {
  pred <- predict(models[i], test, type="prob")
  fname <- paste("FIN-n2v-", methods[i], ".csv", sep="")
  write.csv(cbind(CUS_ID=test$CUS_ID,pred[[1]]), fname, row.names = F)
}
#####2.-3 앙상블하기####
EN1<-read.csv("FIN-n2v-pda.csv")
EN2<-read.csv("FIN-n2v-nnet.csv")
EN3<-read.csv("FIN-n2v-svmRadial.csv")
EN4<-read.csv("FIN-n2v-xgbTree.csv")
EN5<-read.csv("FIN-n2v-gbm.csv")
#modelCor(results)
#              pda       nnet    svmRadial     xgbTree    gbm
#pda       1.00000000  0.03275334 0.5118511  0.81715779  0.7942068
#nnet      0.03275334  1.00000000 0.1549532 -0.05777527 -0.2016708
#svmRadial 0.51185115  0.15495317 1.0000000  0.76723923  0.6999015
#xgbTree   0.81715779 -0.05777527 0.7672392  1.00000000  0.8669835
#gbm       0.79420679 -0.20167079 0.6999015  0.86698352  1.0000000
##pda+nnet
pred<-merge(EN1,EN2,by="CUS_ID")
nc <- ncol(pred)
nf <- 2
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"FIN-n2v-ensemble1.csv", row.names = F)
n2ven1<-read.csv("FIN-n2v-ensemble1.csv")
##ensemble1+svm
pred<-merge(n2ven1,EN3,by="CUS_ID")
nc <- ncol(pred)
nf <- 2
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"FIN-n2v-ensemble2.csv", row.names = F)
n2ven2<-read.csv("FIN-n2v-ensemble2.csv")
##ensemble2+gbm+xgbTree
pred<-merge(n2ven2,merge(EN4,EN5,by="CUS_ID"),by="CUS_ID")
nc <- ncol(pred)
nf <- 3
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"FIN-n2v-ensemble3.csv", row.names = F)


#####3.-1 Master1 TRAIN####
setwd("C:\\Users\\Hyangsuk_Min\\Desktop\\데마팀플\\원데이터")
rm(list=ls())
#train
library(dplyr)
library(lubridate)
library(ggplot2)
library(reshape)
library(matrixStats)
if(!require(data.table)) install.packages("data.table"); library(data.table)

#data
cs.dt.main <- fread("train_profiles.csv")
tr.dt.main <- fread("train_clickstreams.tab"); tr.dt.main[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt.main, CUS_ID); setkey(tr.dt.main, CUS_ID)
md.dt.main <- merge(cs.dt.main, tr.dt.main)

## train_sub
cs.dt.sub <- fread("test_public.csv")
cs.dt.sub$GROUP<-ifelse(cs.dt.sub$`F20-`==1,"F20-",
                        ifelse(cs.dt.sub$F30==1,"F30",
                               ifelse(cs.dt.sub$`F40+`==1,"F40+",
                                      ifelse(cs.dt.sub$`M20-`,"M20-",
                                             ifelse(cs.dt.sub$M30,"M30","M40+")))))
tr.dt.sub <- fread("test_clickstreams.tab"); tr.dt.sub[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt.sub, CUS_ID); setkey(tr.dt.sub, CUS_ID) 
md.dt.sub <- merge(cs.dt.sub[,c(1,8)], tr.dt.sub)

##newtrain
md.dt=rbind(md.dt.main,md.dt.sub)

cstr <- md.dt %>% select(-GROUP)
prof <- md.dt %>% select(CUS_ID,GROUP) %>% distinct(CUS_ID, .keep_all=T)

#save memory
write.csv(cstr, "cstr.csv",row.names=F)
write.csv(prof, "prof.csv",row.names=F)
#rm(list=ls())

cstr = fread("cstr.csv")
#cstr = cstr[,-1]
prof = fread("prof.csv")
#prof = prof[,-1]


cs.t1 = cstr %>% melt(id.vars=c("CUS_ID","ACT_NM"), measure.vars=c("ST_TIME")) %>%
  cast(CUS_ID~ACT_NM, sum, subset=variable=="ST_TIME")
cs.t2 = cs.t1
cs.t2[,-1] = 100*(cs.t2[,-1]/rowSums(cs.t2[,-1]))

cs.c1 = cstr %>% melt(id.vars=c("CUS_ID","ACT_NM"), measure.vars=c("SITE_CNT")) %>%
  cast(CUS_ID~ACT_NM, sum, subset=variable=="SITE_CNT")
cs.c2 = cs.c1
cs.c2[,-1] = 100*(cs.c2[,-1]/rowSums(cs.c2))

pf = prof %>% 
  mutate(gr=ifelse(GROUP=="M20-",0,
                   ifelse(GROUP=="M30",1,
                          ifelse(GROUP=="M40+",2, 
                                 ifelse(GROUP=="F20-",3,
                                        ifelse(GROUP=="F30",4,5)))))) %>% select(-GROUP)

cs.v1= pf %>% left_join(cs.t1)%>% left_join(cs.c1, by="CUS_ID", suffix=c(".t1",".c1"))
cs.v2= pf %>% left_join(cs.t2) %>% left_join(cs.c2, by="CUS_ID", suffix=c(".t2",".c2"))

custsig1 = pf %>% left_join(cs.v1) %>% left_join(cs.v2)
rm(cs.t1);rm(cs.t2);rm(cs.v1);rm(cs.v2);rm(cs.c1);rm(cs.c2)


cs.tc = cstr %>% group_by(CUS_ID) %>% select(CUS_ID, SITE_CNT) %>% summarize(total_cnt=sum(SITE_CNT))
cs.tt = cstr %>% group_by(CUS_ID) %>% select(CUS_ID, ST_TIME) %>% summarize(total_time=sum(ST_TIME))


cs.npr = cstr %>% group_by(CUS_ID) %>% select(CUS_ID, SITE) %>% summarize(reppagenum=n())
cs.np = cstr %>% group_by(CUS_ID) %>% select(CUS_ID, SITE) %>% distinct(SITE) %>% summarize(pagenum=n())


cs.tm = cstr %>% select(CUS_ID,TIME_ID,SITE_CNT,ST_TIME)
cs.tm[,TIME_ID:= as.numeric(TIME_ID)]
cs.tm$TIME_ID <- cs.tm$TIME_ID %% 100
cs.tm.1= cs.tm %>% melt(id.vars=c("CUS_ID","TIME_ID"), measure.vars=c("SITE_CNT")) %>%
  cast(CUS_ID~TIME_ID, sum, subset=variable=="SITE_CNT")
colnames(cs.tm.1) <- c("CUS_ID","tm_1.0","tm_1.1","tm_1.2","tm_1.3","tm_1.4","tm_1.5","tm_1.6",
                       "tm_1.7","tm_1.8","tm_1.9","tm_1.10","tm_1.11","tm_1.12","tm_1.13",
                       "tm_1.14","tm_1.15","tm_1.16","tm_1.17","tm_1.18","tm_1.19",
                       "tm_1.20","tm_1.21","tm_1.22","tm_1.23")

cs.time.1=cs.tm.1 %>% 
  mutate(tm1.1_8=tm_1.0+tm_1.1+tm_1.2+tm_1.3+tm_1.4+tm_1.5+tm_1.6+tm_1.7+tm_1.8)%>%
  mutate(tm1.10_17=tm_1.10+tm_1.11+tm_1.13+tm_1.14+tm_1.15+tm_1.16+tm_1.17) %>%
  mutate(tm1.22_2=tm_1.22+tm_1.23+tm_1.0+tm_1.1+tm_1.2) %>%
  mutate(tm1.5_8=tm_1.5+tm_1.6+tm_1.7+tm_1.8) %>% mutate(tm_1.13_14=tm_1.13+tm_1.14) 
cs.tm.2 = cs.tm.1
cs.tm.2[,-1] = 100*(cs.tm.2[,-1]/rowSums(cs.tm.2))
colnames(cs.tm.2) <- c("CUS_ID","tm_2.0","tm_2.1","tm_2.2","tm_2.3","tm_2.4","tm_2.5","tm_2.6",
                       "tm_2.7","tm_2.8","tm_2.9","tm_2.10","tm_2.11","tm_2.12","tm_2.13",
                       "tm_2.14","tm_2.15","tm_2.16","tm_2.17","tm_2.18","tm_2.19",
                       "tm_2.20","tm_2.21","tm_2.22","tm_2.23")
cs.time.2=cs.tm.2 %>%
  mutate(tm2.1_8=tm_2.0+tm_2.1+tm_2.2+tm_2.3+tm_2.4+tm_2.5+tm_2.6+tm_2.7+tm_2.8)%>%
  mutate(tm2.10_17=tm_2.10+tm_2.11+tm_2.13+tm_2.14+tm_2.15+tm_2.16+tm_2.17) %>%
  mutate(tm2.22_2=tm_2.22+tm_2.23+tm_2.0+tm_2.1+tm_2.2) %>%
  mutate(tm2.5_8=tm_2.5+tm_2.6+tm_2.7+tm_2.8) %>% mutate(tm_2.13_14=tm_2.13+tm_2.14) 


cs.tm.3= cs.tm %>% melt(id.vars=c("CUS_ID","TIME_ID"), measure.vars=c("ST_TIME")) %>%
  cast(CUS_ID~TIME_ID, sum, subset=variable=="ST_TIME")
cs.tm.4 = cs.tm.3
colnames(cs.tm.3) <- c("CUS_ID","tm_3.0","tm_3.1","tm_3.2","tm_3.3","tm_3.4","tm_3.5","tm_3.6",
                       "tm_3.7","tm_3.8","tm_3.9","tm_3.10","tm_3.11","tm_3.12","tm_3.13",
                       "tm_3.14","tm_3.15","tm_3.16","tm_3.17","tm_3.18","tm_3.19",
                       "tm_3.20","tm_3.21","tm_3.22","tm_3.23")
cs.time.3=cs.tm.3 %>%
  mutate(tm3.1_8=tm_3.0+tm_3.1+tm_3.2+tm_3.3+tm_3.4+tm_3.5+tm_3.6+tm_3.7+tm_3.8)%>%
  mutate(tm3.10_17=tm_3.10+tm_3.11+tm_3.13+tm_3.14+tm_3.15+tm_3.16+tm_3.17) %>%
  mutate(tm3.22_2=tm_3.22+tm_3.23+tm_3.0+tm_3.1+tm_3.2) %>%
  mutate(tm3.5_8=tm_3.5+tm_3.6+tm_3.7+tm_3.8) %>% mutate(tm_3.13_14=tm_3.13+tm_3.14) 
cs.tm.4[,-1] = 100*(cs.tm.4[,-1]/rowSums(cs.tm.4))
cs.tm.4[is.na(cs.tm.4)] <- 0
colnames(cs.tm.4) <- c("CUS_ID","tm_4.0","tm_4.1","tm_4.2","tm_4.3","tm_4.4","tm_4.5","tm_4.6",
                       "tm_4.7","tm_4.8","tm_4.9","tm_4.10","tm_4.11","tm_4.12","tm_4.13",
                       "tm_4.14","tm_4.15","tm_4.16","tm_4.17","tm_4.18","tm_4.19",
                       "tm_4.20","tm_4.21","tm_4.22","tm_4.23")
cs.time.4=cs.tm.4 %>%
  mutate(tm4.1_8=tm_4.0+tm_4.1+tm_4.2+tm_4.3+tm_4.4+tm_4.5+tm_4.6+tm_4.7+tm_4.8)%>%
  mutate(tm4.10_17=tm_4.10+tm_4.11+tm_4.13+tm_4.14+tm_4.15+tm_4.16+tm_4.17) %>%
  mutate(tm4.22_2=tm_4.22+tm_4.23+tm_4.0+tm_4.1+tm_4.2) %>%
  mutate(tm4.5_8=tm_4.5+tm_4.6+tm_4.7+tm_4.8) %>% mutate(tm_4.13_14=tm_4.13+tm_4.14) 


cstr$TIME_ID <- ymd_h(cstr$TIME_ID)
cstr$date <- date(cstr$TIME_ID)
cstr$year <- year(cstr$TIME_ID)
cstr$month <- month(cstr$TIME_ID)
cstr$day <- day(cstr$TIME_ID)
cstr$time <- hour(cstr$TIME_ID)
cstr$wkday <- wday(cstr$TIME_ID)

cs.V1 <- cstr %>% 
  group_by(CUS_ID) %>%
  summarize(ttl_pv = sum(SITE_CNT))


cs.V4 <- cstr %>%
  group_by(CUS_ID, date) %>%
  summarize(cstr = n()) %>%
  group_by(CUS_ID) %>%
  summarize(ttl_vis_day = n())


cs.V5.day <- cstr %>%
  group_by(CUS_ID, wkday) %>%
  summarize(pv_day = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ wkday, sum) %>%
  mutate(ttl_pv = cs.V1$ttl_pv)

cs.V5.day[,2:8] <- cs.V5.day[,2:8]/cs.V5.day[,9]
cs.V5.day <- cs.V5.day[,-9]
names(cs.V5.day) <- c("CUS_ID", "sun", "mon", "tue", "wed", "thu", "fri", "sat")


cs.V6.day <- cstr %>%
  group_by(CUS_ID, wkday) %>%
  summarize(pv_day = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ wkday, sum) %>%
  mutate(avg_day = rowMeans(.[2:8]), sd_day = rowSds(as.matrix(.[2:8]))) %>%
  mutate(coef_var_day = sd_day/avg_day) %>%
  select(CUS_ID, coef_var_day)

#custsig
custsig <- pf %>% 
  left_join(custsig1) %>%
  left_join(cs.tc) %>%
  left_join(cs.tt) %>%
  left_join(cs.npr) %>%
  left_join(cs.np) %>%
  left_join(cs.time.1) %>%
  left_join(cs.time.2) %>%
  left_join(cs.V1) %>%
  left_join(cs.V4) %>%
  left_join(cs.V5.day) %>%
  left_join(cs.V6.day)


names(custsig) <- gsub(" ", "", names(custsig))
names(custsig) <- gsub("/", "", names(custsig))

custsig[is.na(custsig)] <- 0
write.csv(custsig,"custsig.csv")
#change column names & NA to 0

#####3.-2 Master1 TEST####
rm(list=ls())

#data
## train_sub
tr.dt.sub <- fread("test_clickstreams.tab"); tr.dt.sub[,CUS_ID:= as.numeric(CUS_ID)]
setkey(tr.dt.sub, CUS_ID) 

profsub =read.csv("test_public.csv", stringsAsFactors=F)

profsubtest= profsub %>%
  mutate(GROUP=ifelse(M20.==1,"M20-",
                      ifelse(M30==1,"M30",
                             ifelse(M40.==1,"M40+", 
                                    ifelse(F20.==1,"F20-",
                                           ifelse(F30==1,"F30",
                                                  ifelse(F40.==1,"F40+",NA))))))) %>% select(-F20.:-M40.)

# test data
tr.t.dt <- tr.dt.sub
tr.t.dt <- tr.t.dt[!(tr.t.dt$CUS_ID %in% profsubtest$CUS_ID)]
setkey(tr.t.dt, CUS_ID)

cstr <- tr.t.dt
rm(tr.dt.sub);rm(tr.t.dt);rm(profsub);rm(profsubtest)


cs.t1 = cstr %>% melt(id.vars=c("CUS_ID","ACT_NM"), measure.vars=c("ST_TIME")) %>%
  cast(CUS_ID~ACT_NM, sum, subset=variable=="ST_TIME")
cs.t2 = cs.t1
cs.t2[,-1] = 100*(cs.t2[,-1]/rowSums(cs.t2[,-1]))

cs.c1 = cstr %>% melt(id.vars=c("CUS_ID","ACT_NM"), measure.vars=c("SITE_CNT")) %>%
  cast(CUS_ID~ACT_NM, sum, subset=variable=="SITE_CNT")
cs.c2 = cs.c1
cs.c2[,-1] = 100*(cs.c2[,-1]/rowSums(cs.c2))

cs.v1= cs.t1 %>% left_join(cs.c1, by="CUS_ID", suffix=c(".t1",".c1"))
cs.v2= cs.t2 %>% left_join(cs.c2, by="CUS_ID", suffix=c(".t2",".c2"))

custsig1 = cs.v1 %>% left_join(cs.v2)
rm(cs.t1);rm(cs.t2);rm(cs.v1);rm(cs.v2);rm(cs.c1);rm(cs.c2)


cs.tc = cstr %>% group_by(CUS_ID) %>% select(CUS_ID, SITE_CNT) %>% summarize(total_cnt=sum(SITE_CNT))
cs.tt = cstr %>% group_by(CUS_ID) %>% select(CUS_ID, ST_TIME) %>% summarize(total_time=sum(ST_TIME))


cs.npr = cstr %>% group_by(CUS_ID) %>% select(CUS_ID, SITE) %>% summarize(reppagenum=n())
cs.np = cstr %>% group_by(CUS_ID) %>% select(CUS_ID, SITE) %>% distinct(SITE) %>% summarize(pagenum=n())


cs.tm = cstr %>% select(CUS_ID,TIME_ID,SITE_CNT,ST_TIME)
cs.tm[,TIME_ID:= as.numeric(TIME_ID)]
cs.tm$TIME_ID <- cs.tm$TIME_ID %% 100
cs.tm.1= cs.tm %>% melt(id.vars=c("CUS_ID","TIME_ID"), measure.vars=c("SITE_CNT")) %>%
  cast(CUS_ID~TIME_ID, sum, subset=variable=="SITE_CNT")
colnames(cs.tm.1) <- c("CUS_ID","tm_1.0","tm_1.1","tm_1.2","tm_1.3","tm_1.4","tm_1.5","tm_1.6",
                       "tm_1.7","tm_1.8","tm_1.9","tm_1.10","tm_1.11","tm_1.12","tm_1.13",
                       "tm_1.14","tm_1.15","tm_1.16","tm_1.17","tm_1.18","tm_1.19",
                       "tm_1.20","tm_1.21","tm_1.22","tm_1.23")

cs.time.1=cs.tm.1 %>% 
  mutate(tm1.1_8=tm_1.0+tm_1.1+tm_1.2+tm_1.3+tm_1.4+tm_1.5+tm_1.6+tm_1.7+tm_1.8)%>%
  mutate(tm1.10_17=tm_1.10+tm_1.11+tm_1.13+tm_1.14+tm_1.15+tm_1.16+tm_1.17) %>%
  mutate(tm1.22_2=tm_1.22+tm_1.23+tm_1.0+tm_1.1+tm_1.2) %>%
  mutate(tm1.5_8=tm_1.5+tm_1.6+tm_1.7+tm_1.8) %>% mutate(tm_1.13_14=tm_1.13+tm_1.14) 
cs.tm.2 = cs.tm.1
cs.tm.2[,-1] = 100*(cs.tm.2[,-1]/rowSums(cs.tm.2))
colnames(cs.tm.2) <- c("CUS_ID","tm_2.0","tm_2.1","tm_2.2","tm_2.3","tm_2.4","tm_2.5","tm_2.6",
                       "tm_2.7","tm_2.8","tm_2.9","tm_2.10","tm_2.11","tm_2.12","tm_2.13",
                       "tm_2.14","tm_2.15","tm_2.16","tm_2.17","tm_2.18","tm_2.19",
                       "tm_2.20","tm_2.21","tm_2.22","tm_2.23")
cs.time.2=cs.tm.2 %>%
  mutate(tm2.1_8=tm_2.0+tm_2.1+tm_2.2+tm_2.3+tm_2.4+tm_2.5+tm_2.6+tm_2.7+tm_2.8)%>%
  mutate(tm2.10_17=tm_2.10+tm_2.11+tm_2.13+tm_2.14+tm_2.15+tm_2.16+tm_2.17) %>%
  mutate(tm2.22_2=tm_2.22+tm_2.23+tm_2.0+tm_2.1+tm_2.2) %>%
  mutate(tm2.5_8=tm_2.5+tm_2.6+tm_2.7+tm_2.8) %>% mutate(tm_2.13_14=tm_2.13+tm_2.14) 


cs.tm.3= cs.tm %>% melt(id.vars=c("CUS_ID","TIME_ID"), measure.vars=c("ST_TIME")) %>%
  cast(CUS_ID~TIME_ID, sum, subset=variable=="ST_TIME")
cs.tm.4 = cs.tm.3
colnames(cs.tm.3) <- c("CUS_ID","tm_3.0","tm_3.1","tm_3.2","tm_3.3","tm_3.4","tm_3.5","tm_3.6",
                       "tm_3.7","tm_3.8","tm_3.9","tm_3.10","tm_3.11","tm_3.12","tm_3.13",
                       "tm_3.14","tm_3.15","tm_3.16","tm_3.17","tm_3.18","tm_3.19",
                       "tm_3.20","tm_3.21","tm_3.22","tm_3.23")
cs.time.3=cs.tm.3 %>%
  mutate(tm3.1_8=tm_3.0+tm_3.1+tm_3.2+tm_3.3+tm_3.4+tm_3.5+tm_3.6+tm_3.7+tm_3.8)%>%
  mutate(tm3.10_17=tm_3.10+tm_3.11+tm_3.13+tm_3.14+tm_3.15+tm_3.16+tm_3.17) %>%
  mutate(tm3.22_2=tm_3.22+tm_3.23+tm_3.0+tm_3.1+tm_3.2) %>%
  mutate(tm3.5_8=tm_3.5+tm_3.6+tm_3.7+tm_3.8) %>% mutate(tm_3.13_14=tm_3.13+tm_3.14) 
cs.tm.4[,-1] = 100*(cs.tm.4[,-1]/rowSums(cs.tm.4))
cs.tm.4[is.na(cs.tm.4)] <- 0
colnames(cs.tm.4) <- c("CUS_ID","tm_4.0","tm_4.1","tm_4.2","tm_4.3","tm_4.4","tm_4.5","tm_4.6",
                       "tm_4.7","tm_4.8","tm_4.9","tm_4.10","tm_4.11","tm_4.12","tm_4.13",
                       "tm_4.14","tm_4.15","tm_4.16","tm_4.17","tm_4.18","tm_4.19",
                       "tm_4.20","tm_4.21","tm_4.22","tm_4.23")
cs.time.4=cs.tm.4 %>%
  mutate(tm4.1_8=tm_4.0+tm_4.1+tm_4.2+tm_4.3+tm_4.4+tm_4.5+tm_4.6+tm_4.7+tm_4.8)%>%
  mutate(tm4.10_17=tm_4.10+tm_4.11+tm_4.13+tm_4.14+tm_4.15+tm_4.16+tm_4.17) %>%
  mutate(tm4.22_2=tm_4.22+tm_4.23+tm_4.0+tm_4.1+tm_4.2) %>%
  mutate(tm4.5_8=tm_4.5+tm_4.6+tm_4.7+tm_4.8) %>% mutate(tm_4.13_14=tm_4.13+tm_4.14) 


cstr$TIME_ID <- ymd_h(cstr$TIME_ID)
cstr$date <- date(cstr$TIME_ID)
cstr$year <- year(cstr$TIME_ID)
cstr$month <- month(cstr$TIME_ID)
cstr$day <- day(cstr$TIME_ID)
cstr$time <- hour(cstr$TIME_ID)
cstr$wkday <- wday(cstr$TIME_ID)

cs.V1 <- cstr %>% 
  group_by(CUS_ID) %>%
  summarize(ttl_pv = sum(SITE_CNT))


cs.V4 <- cstr %>%
  group_by(CUS_ID, date) %>%
  summarize(cstr = n()) %>%
  group_by(CUS_ID) %>%
  summarize(ttl_vis_day = n())


cs.V5.day <- cstr %>%
  group_by(CUS_ID, wkday) %>%
  summarize(pv_day = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ wkday, sum) %>%
  mutate(ttl_pv = cs.V1$ttl_pv)

cs.V5.day[,2:8] <- cs.V5.day[,2:8]/cs.V5.day[,9]
cs.V5.day <- cs.V5.day[,-9]
names(cs.V5.day) <- c("CUS_ID", "sun", "mon", "tue", "wed", "thu", "fri", "sat")


cs.V6.day <- cstr %>%
  group_by(CUS_ID, wkday) %>%
  summarize(pv_day = sum(SITE_CNT)) %>%
  cast(CUS_ID ~ wkday, sum) %>%
  mutate(avg_day = rowMeans(.[2:8]), sd_day = rowSds(as.matrix(.[2:8]))) %>%
  mutate(coef_var_day = sd_day/avg_day) %>%
  select(CUS_ID, coef_var_day)

#custsig
custsig <- custsig1 %>%
  left_join(cs.tc) %>%
  left_join(cs.tt) %>%
  left_join(cs.npr) %>%
  left_join(cs.np) %>%
  left_join(cs.time.1) %>%
  left_join(cs.time.2) %>%
  left_join(cs.V1) %>%
  left_join(cs.V4) %>%
  left_join(cs.V5.day) %>%
  left_join(cs.V6.day)


names(custsig) <- gsub(" ", "", names(custsig))
names(custsig) <- gsub("/", "", names(custsig))

custsig[is.na(custsig)] <- 0
write.csv(custsig,"custsig_test.csv")

#####3.-3 feature selection####
rm(list=ls())
library(caret)
library(dplyr)
library(xgboost)
library(plyr)
library(randomForest)

sig <- read.csv("custsig.csv", stringsAsFactors=F)
sig = sig %>% mutate(weekend=sun+sat)
sig = sig[,-1]
sigsubmit <- read.csv("custsig_test.csv", stringsAsFactors=F)
sigsubmit = sigsubmit %>% mutate(weekend=sun+sat)
sigsubmit = sigsubmit[,-1]

cb.train=sig
cb.test= sigsubmit

#model train
cb.train$gr <- factor(cb.train$gr)
cb.train$gr = paste("C",cb.train$gr,sep="")

cb3.train = cb.train %>%
  select(CUS_ID,gr,#sex
         IT뉴스.c2,자동차정보.t2,컨텐츠공유.P2P..c2,자동차제조.c2,외국신문잡지.c2,
         자동차제조.t2,내비게이션GPS.c2,tm2.5_8,종합인터넷신문.t2,weekend,종합일간지.t2,
         종합가격비교.t2,방송뉴스.t2,경제신문.c2,tm2.1_8,tm_2.7,자동차브랜드.c2,대통령선거.c2,
         중고차쇼핑몰.c2,포털뉴스.t2,게임웹진.t2,기계장비B2B.c2,
         여성의류쇼핑몰.c2,tm_2.13_14,연예인의류쇼핑몰.c2,여성화전문몰.c2,커뮤니티포털.t2,
         포털블로그.t2,종합쇼핑몰.t2,coef_var_day,tm2.10_17,브랜드여성의류쇼핑몰.c2,
         화장품브랜드쇼핑몰.t2,종합의류쇼핑몰.c2,출산육아정보.t2,화장품브랜드업체.t2,온라인사진인화.c2,
         소셜허브.t2,커피전문점.c2,포털게시판.t2,패션의류전문지.t2,tue,패밀리레스토랑.t2,유아용품제조.t2,
         종합일간지.t1,방송뉴스.t1,스포츠신문.c1,대통령선거.c1,자동차브랜드.t1,컴퓨터장치주변기기.c1,
         경제신문.c1,게임웹진.t1,기계장비B2B.c1,자동차전문지.c1,출산육아정보.t1,의약제조.c1,패밀리레스토랑.c1,
         홈인테리어용품쇼핑몰.t1,
         #age 20_30
         여성의류쇼핑몰.t1,장학재단.c1,군대.t1,SNS.c1,남성의류쇼핑몰.c1,캐주얼웨어브랜드.c1,
         연예인의류쇼핑몰.c1,속옷쇼핑몰.c1,스포츠의류쇼핑몰.c1,선글라스쇼핑몰.t1,콘텍트렌즈.c1,
         고시자격증학원.c1,콘텍트렌즈.t1,메신저.c1,선물디자인소품쇼핑몰.c1,속옷제조.t1,쇼핑몰솔루션.c1,
         화장품브랜드업체.t1,시험자격증교육.종합..c1,행정민원.c1,시중은행.t1,인터넷납부.c1,
         대형마트쇼핑몰.c1,포털쇼핑.t1,전기가스석유유통.t1,생명보험.c1,포털어린이.c1,공공질서안전.t1,
         고객지원센터.c1,자동차보험.c1,관광농원휴양지.t1,전자결제전자화폐.c1,홈인테리어용품쇼핑몰.t1,
         SNS.c2,군대.c2,남성의류쇼핑몰.c2,장학재단.c2,포털만화.t2,고시자격증학원.t2,
         메신저.c2,캐주얼웨어브랜드.c2,패션몰링크검색.t2,영화평리뷰.c2,액션슈팅.c2,종합대학교.국내..c2,
         종합신발쇼핑몰.c2,시험자격증교육.종합..c2,속옷쇼핑몰.t2,오픈마켓.c2,시중은행.c2,행정민원.c2,
         대형마트쇼핑몰.t2,인터넷납부.c2,신용카드.c2,자동차보험.t2,포털어린이.c2,종합포털.t2,
         전자결제전자화폐.c2,공단.c2,tm1.5_8,포털지도지역정보.c2,아동복쇼핑몰.c2,
         포털쇼핑.t2,포털부동산.c2,대형마트SSM.t2,
         #age20_40
         종합신발쇼핑몰.c1,커뮤니티포털.t1,브랜드여성의류쇼핑몰.t1,포털검색.c1,종합의류쇼핑몰.c1,
         여성화전문몰.c1,배달음식쇼핑몰.t1,미용실.t1,게임커뮤니티.c1,SPA브랜드.c1,
         헤어미용쇼핑몰.c1,빅사이즈의류쇼핑몰.t1,폰트서체.t1,광고대행사.c1,
         웹디자인그래픽.t1,패션미용정보.t1,기타패션잡화쇼핑몰.c1,비만클리닉.c1,
         화장품브랜드쇼핑몰.t1,아이스크림프랜차이즈.c1,그룹사기업채용.c1,명품쇼핑몰.c1,모바일게임.t1,
         여성의류브랜드.c1,패션쇼핑타운.c1,대기업그룹사.c1,스키스노우보드.c1,웹에이전시.c1,성형외과.t1,
         커피음료프랜차이즈.c1,포털블로그.c1,영화평리뷰.c1,시험자격증교육.단과..c1,포털게시판.t1,
         이미지클립아트.t1,인터넷광고대행사미디어랩.c1,피자스파게티.t1,패스트푸드프랜차이즈.c1,커플쇼핑몰.t1,
         게임인터넷방송.t1,브랜드남성의류쇼핑몰.t1,로그분석카운터.t1,섬유패션B2B.t1,포털만화.t1,
         공연정보.c1,SPA브랜드쇼핑몰.c1,체형피부관리.c1,기타교육기관.c1,수능대학입시.t1,
         모바일솔루션개발.c1,해외쇼핑대행.t1,쇼핑몰구축.c1,음악감상.c1,패션의류전문지.t1,
         브랜드청바지쇼핑몰.c1,화장품제조.t1,여행용품쇼핑몰.t1,외국신문잡지.c1,종합인터넷신문.t1,
         포털뉴스.t1,종합포털.t1,내비게이션GPS.t1,종합가격비교.c1,IT뉴스.c1,공단.t1,
         사법기관.c1,언론사블로그.t1,광역단체.t1,포털부동산.c1,대형마트쇼핑몰.t1,
         AS센터.t1,부동산종합정보.t1,부동산경매.t1,지역뉴스.t1,전문부동산정보.c1,pagenum,
         포털검색.c2,배달음식쇼핑몰.t2,스포츠의류쇼핑몰.c2,기능화장품쇼핑몰.t2,소셜커머스.t2,
         선글라스쇼핑몰.t2,선물디자인소품쇼핑몰.c2,미용실.c2,게임커뮤니티.t2,헤어미용쇼핑몰.c2,
         쇼핑몰솔루션.c2,tm2.22_2,빅사이즈의류쇼핑몰.t2,SPA브랜드.c2,아이템거래.t2,
         그룹사기업채용.c2,명품쇼핑몰.c2,로그분석카운터.t2,패션미용정보.c2,인터넷광고대행사미디어랩.t2,
         상품권쇼핑몰.t2,토렌트정보.t2,tm_2.2,SPA브랜드쇼핑몰.t2,수능대학입시.c2,면세점.t2,MMORPG.t2,
         coef_var_day,브랜드남성의류쇼핑몰.c2,패스트푸드프랜차이즈.c2,아이스크림프랜차이즈.c2,
         기타교육기관.c2,안과.c2,이미지클립아트.t2,게임인터넷방송.t2,기타쇼핑몰.t2,쇼핑몰구축.c2,
         스키스노우보드.t2,음식점프랜차이즈.c2,호텔.c2,폰트서체.t2,광고대행사.t2,
         패션쇼핑타운.c2,피자스파게티.t2,섬유패션B2B.t2,휴대폰악세사리쇼핑몰.c1,
         여행용품쇼핑몰.t2,게임정보.t2,방송뉴스.c2,언론사블로그.t2,위성케이블채널.c2,스포츠신문.c2,
         ttl_vis_day,사법기관.c2,부동산경매.c2,생명보험.t2,전기가스석유유통.t2,기초단체.t2,지역뉴스.t2,통신사.t2,
         #age 30_40
         오픈마켓.c1,종합신발쇼핑몰.t1,임부복쇼핑몰.c1,소셜커머스.c1,우유유제품제조.c1,백화점.c1,
         온라인사진인화.t1,가방쇼핑몰.c1,신용카드.c1,사무용품쇼핑몰.c1,유아용품쇼핑몰.t1,전자결제솔루션.t1,
         유아용품제조.t1,해외배송대행.c1,사진관스튜디오.c1,치킨프랜차이즈.t1,커피전문점.c1,
         기능화장품쇼핑몰.t1,파티용품쇼핑몰.c1,통신사.t1)

cb3.test = cb.test %>%
  select(CUS_ID, #sex
         IT뉴스.c2,자동차정보.t2,컨텐츠공유.P2P..c2,자동차제조.c2,외국신문잡지.c2,
         자동차제조.t2,내비게이션GPS.c2,tm2.5_8,종합인터넷신문.t2,weekend,종합일간지.t2,
         종합가격비교.t2,방송뉴스.t2,경제신문.c2,tm2.1_8,tm_2.7,자동차브랜드.c2,대통령선거.c2,
         중고차쇼핑몰.c2,포털뉴스.t2,게임웹진.t2,기계장비B2B.c2,
         여성의류쇼핑몰.c2,tm_2.13_14,연예인의류쇼핑몰.c2,여성화전문몰.c2,커뮤니티포털.t2,
         포털블로그.t2,종합쇼핑몰.t2,coef_var_day,tm2.10_17,브랜드여성의류쇼핑몰.c2,
         화장품브랜드쇼핑몰.t2,종합의류쇼핑몰.c2,출산육아정보.t2,화장품브랜드업체.t2,온라인사진인화.c2,
         소셜허브.t2,커피전문점.c2,포털게시판.t2,패션의류전문지.t2,tue,패밀리레스토랑.t2,유아용품제조.t2,
         종합일간지.t1,방송뉴스.t1,스포츠신문.c1,대통령선거.c1,자동차브랜드.t1,컴퓨터장치주변기기.c1,
         경제신문.c1,게임웹진.t1,기계장비B2B.c1,자동차전문지.c1,출산육아정보.t1,의약제조.c1,패밀리레스토랑.c1,
         홈인테리어용품쇼핑몰.t1,
         #age 20_30
         여성의류쇼핑몰.t1,장학재단.c1,군대.t1,SNS.c1,남성의류쇼핑몰.c1,캐주얼웨어브랜드.c1,
         연예인의류쇼핑몰.c1,속옷쇼핑몰.c1,스포츠의류쇼핑몰.c1,선글라스쇼핑몰.t1,콘텍트렌즈.c1,
         고시자격증학원.c1,콘텍트렌즈.t1,메신저.c1,선물디자인소품쇼핑몰.c1,속옷제조.t1,쇼핑몰솔루션.c1,
         화장품브랜드업체.t1,시험자격증교육.종합..c1,행정민원.c1,시중은행.t1,인터넷납부.c1,
         대형마트쇼핑몰.c1,포털쇼핑.t1,전기가스석유유통.t1,생명보험.c1,포털어린이.c1,공공질서안전.t1,
         고객지원센터.c1,자동차보험.c1,관광농원휴양지.t1,전자결제전자화폐.c1,홈인테리어용품쇼핑몰.t1,
         SNS.c2,군대.c2,남성의류쇼핑몰.c2,장학재단.c2,포털만화.t2,고시자격증학원.t2,
         메신저.c2,캐주얼웨어브랜드.c2,패션몰링크검색.t2,영화평리뷰.c2,액션슈팅.c2,종합대학교.국내..c2,
         종합신발쇼핑몰.c2,시험자격증교육.종합..c2,속옷쇼핑몰.t2,오픈마켓.c2,시중은행.c2,행정민원.c2,
         대형마트쇼핑몰.t2,인터넷납부.c2,신용카드.c2,자동차보험.t2,포털어린이.c2,종합포털.t2,
         전자결제전자화폐.c2,공단.c2,tm1.5_8,포털지도지역정보.c2,아동복쇼핑몰.c2,
         포털쇼핑.t2,포털부동산.c2,대형마트SSM.t2,
         #age20_40
         종합신발쇼핑몰.c1,커뮤니티포털.t1,브랜드여성의류쇼핑몰.t1,포털검색.c1,종합의류쇼핑몰.c1,
         여성화전문몰.c1,배달음식쇼핑몰.t1,미용실.t1,게임커뮤니티.c1,SPA브랜드.c1,
         헤어미용쇼핑몰.c1,빅사이즈의류쇼핑몰.t1,폰트서체.t1,광고대행사.c1,
         웹디자인그래픽.t1,패션미용정보.t1,기타패션잡화쇼핑몰.c1,비만클리닉.c1,
         화장품브랜드쇼핑몰.t1,아이스크림프랜차이즈.c1,그룹사기업채용.c1,명품쇼핑몰.c1,모바일게임.t1,
         여성의류브랜드.c1,패션쇼핑타운.c1,대기업그룹사.c1,스키스노우보드.c1,웹에이전시.c1,성형외과.t1,
         커피음료프랜차이즈.c1,포털블로그.c1,영화평리뷰.c1,시험자격증교육.단과..c1,포털게시판.t1,
         이미지클립아트.t1,인터넷광고대행사미디어랩.c1,피자스파게티.t1,패스트푸드프랜차이즈.c1,커플쇼핑몰.t1,
         게임인터넷방송.t1,브랜드남성의류쇼핑몰.t1,로그분석카운터.t1,섬유패션B2B.t1,포털만화.t1,
         공연정보.c1,SPA브랜드쇼핑몰.c1,체형피부관리.c1,기타교육기관.c1,수능대학입시.t1,
         모바일솔루션개발.c1,해외쇼핑대행.t1,쇼핑몰구축.c1,음악감상.c1,패션의류전문지.t1,
         브랜드청바지쇼핑몰.c1,화장품제조.t1,여행용품쇼핑몰.t1,외국신문잡지.c1,종합인터넷신문.t1,
         포털뉴스.t1,종합포털.t1,내비게이션GPS.t1,종합가격비교.c1,IT뉴스.c1,공단.t1,
         사법기관.c1,언론사블로그.t1,광역단체.t1,포털부동산.c1,대형마트쇼핑몰.t1,
         AS센터.t1,부동산종합정보.t1,부동산경매.t1,지역뉴스.t1,전문부동산정보.c1,pagenum,
         포털검색.c2,배달음식쇼핑몰.t2,스포츠의류쇼핑몰.c2,기능화장품쇼핑몰.t2,소셜커머스.t2,
         선글라스쇼핑몰.t2,선물디자인소품쇼핑몰.c2,미용실.c2,게임커뮤니티.t2,헤어미용쇼핑몰.c2,
         쇼핑몰솔루션.c2,tm2.22_2,빅사이즈의류쇼핑몰.t2,SPA브랜드.c2,아이템거래.t2,
         그룹사기업채용.c2,명품쇼핑몰.c2,로그분석카운터.t2,패션미용정보.c2,인터넷광고대행사미디어랩.t2,
         상품권쇼핑몰.t2,토렌트정보.t2,tm_2.2,SPA브랜드쇼핑몰.t2,수능대학입시.c2,면세점.t2,MMORPG.t2,
         coef_var_day,브랜드남성의류쇼핑몰.c2,패스트푸드프랜차이즈.c2,아이스크림프랜차이즈.c2,
         기타교육기관.c2,안과.c2,이미지클립아트.t2,게임인터넷방송.t2,기타쇼핑몰.t2,쇼핑몰구축.c2,
         스키스노우보드.t2,음식점프랜차이즈.c2,호텔.c2,폰트서체.t2,광고대행사.t2,
         패션쇼핑타운.c2,피자스파게티.t2,섬유패션B2B.t2,휴대폰악세사리쇼핑몰.c1,
         여행용품쇼핑몰.t2,게임정보.t2,방송뉴스.c2,언론사블로그.t2,위성케이블채널.c2,스포츠신문.c2,
         ttl_vis_day,사법기관.c2,부동산경매.c2,생명보험.t2,전기가스석유유통.t2,기초단체.t2,지역뉴스.t2,통신사.t2,
         #age 30_40
         오픈마켓.c1,종합신발쇼핑몰.t1,임부복쇼핑몰.c1,소셜커머스.c1,우유유제품제조.c1,백화점.c1,
         온라인사진인화.t1,가방쇼핑몰.c1,신용카드.c1,사무용품쇼핑몰.c1,유아용품쇼핑몰.t1,전자결제솔루션.t1,
         유아용품제조.t1,해외배송대행.c1,사진관스튜디오.c1,치킨프랜차이즈.t1,커피전문점.c1,
         기능화장품쇼핑몰.t1,파티용품쇼핑몰.c1,통신사.t1)
names(cb3.train)
group<-fread("train_profiles.csv")
test_pulic<-fread("test_public.csv")
test_pulic$GROUP<-ifelse(test_pulic$`F20-`==1,"F20-",
                         ifelse(test_pulic$F30==1,"F30",
                                ifelse(test_pulic$`F40+`==1,"F40+",
                                       ifelse(test_pulic$`M20-`,"M20-",
                                              ifelse(test_pulic$M30,"M30","M40+")))))
group<-rbind(group,test_pulic[,c(1,8)])
cb3.train<-merge(group,cb3.train,by="CUS_ID")
cb3.train<-cb3.train[,-3]
write.csv(cb3.train,"master1_train.csv",row.names = F)
write.csv(cb3.test,"maste1_test.csv",row.names = F)

#####3.-3(2) 변수 추가####
train<-read.csv("master1_train.csv")
test<-read.csv("maste1_test.csv")
tf<-read.csv("train_fN_sel+n_3500.csv")
te<-read.csv("test_fN_sel+n_3500.csv")
train<-merge(train,tf[,c(1,525:530)],by="CUS_ID")
test<-merge(test,te[,c(1,524:529)],by="CUS_ID")
write.csv(train,"m1+n_train.csv",row.names = F)
write.csv(test,"m1+n_test.csv",row.names = F)


#####3.-4 Modeling####
rm(list=ls())
train<-read.csv("m1+n_train.csv")
test<-read.csv("m1+n_test.csv")
train$GROUP<-make.names(train$GROUP)
control <- trainControl(method="repeatedcv", number=5, repeats=3, classProbs=TRUE, summaryFunction=mnLogLoss)

# Used models
methods <- c("pda","gbm", "nnet","xgbTree") # add methods such as xgbTree, rf, svmRadious, etc.

models <- list()
for (i in 1:length(methods)) {
  set.seed(123)
  print(methods[i])
  model <- train(GROUP ~ .,
                 data = subset(train, select=-CUS_ID),
                 method = methods[i],
                 preProcess="scale",
                 metric = "logLoss",
                 trControl = control)
  summary(model$results$logLoss)
  models[[i]] <- model
}
names(models) <- methods
# Saving and loading models: 
#saveRDS(models, "fN_sel_3500_170613.rds") 
# models <- readRDS("models.rds")

# Model comparison
results1 <- resamples(models)
summary(results1)
modelCor(results1)

# prediction & submission
for (i in 1:length(methods)) {
  pred <- predict(models[i], test, type="prob")
  fname <- paste("FIN-m1+n-", methods[i], ".csv", sep="")
  write.csv(cbind(CUS_ID=test$CUS_ID,pred[[1]]), fname, row.names = F)
}

#####3.-5 앙상블하기####
EN1<-read.csv("FIN-m1+n-gbm.csv")
EN2<-read.csv("FIN-m1+n-nnet.csv")
EN3<-read.csv("FIN-m1+n-xgbTree.csv")
pred<-merge(EN1,merge(EN2,EN3,by="CUS_ID"),by="CUS_ID")
nc <- ncol(pred)
nf <- 3
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"FIN-m1+n-3500ENSEMBLE.csv", row.names = F)

#####4.워드투백 만들기####
setwd("C:\\Users\\Hyangsuk_Min\\Desktop\\데마팀플\\원데이터")
rm(list=ls())
# Install & load word2vec package
if(!require(devtools)) install.packages("devtools"); library(devtools)
if(!require(wordVectors)) install_github("bmschmidt/wordVectors"); library(wordVectors)

# Install & load data.table package
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(randomForest)) install.packages("randomForest"); library(randomForest)
if(!require(caret)) install.packages("caret"); library(caret)

# list objects in word2vec package
ls("package:wordVectors")


#####4.-1 소분류 워투백####
####트레인
###### Fast reading and combining several files using data.table (with fread): 5 times faster than read.csv()
cs.dt <- fread("train_profiles.csv")
tr.dt <- fread("train_clickstreams.tab"); tr.dt[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt, CUS_ID); setkey(tr.dt, CUS_ID) 
md.dt <- merge(cs.dt, tr.dt)

test_pulic<-fread("test_public.csv")
test_pulic$GROUP<-ifelse(test_pulic$`F20-`==1,"F20-",
                         ifelse(test_pulic$F30==1,"F30",
                                ifelse(test_pulic$`F40+`==1,"F40+",
                                       ifelse(test_pulic$`M20-`,"M20-",
                                              ifelse(test_pulic$M30,"M30","M40+")))))
tr.dt2<-fread("test_clickstreams.tab"); tr.dt2[,CUS_ID:=as.numeric(CUS_ID)]
setkey(test_pulic,CUS_ID);setkey(tr.dt2,CUS_ID)
md.dt2<-merge(test_pulic[,c(1,8)],tr.dt2)
md.dt<-rbind(md.dt,md.dt2)
cus<-unique(md.dt$CUS_ID)

###### Make sites sentences ## 방문한 소분류를 모음
f <- function(x, t) {
  grp <- md.dt[CUS_ID==x, GROUP][1]
  itemfreq <- table(md.dt[CUS_ID==x,  ACT_NM])
  fitems <- itemfreq[itemfreq >= t]
  act <- names(fitems)
  #  
  sapply(act, function(x) gsub(" ", "_", x))
  set.seed(1)
  #
  as.vector((sapply(1:20, function(x) c(grp, sample(act)))))  ##방문한 사이트를 20번 샘플링 --> 5만개의 문장을 만들어서 학습시킴.
}
items <- unlist(sapply(cus, f, 2))

write.table(items, "items-act_nm.txt", eol = " ", quote = F, row.names = F, col.names = F)

##### Train site2vec model
model = train_word2vec("items-act_nm.txt","vec-act_nm.bin",vectors=300,threads=1,window=5,cbow=1,iter=5,negative_samples=10, force = T)
#model <- read.binary.vectors("vec-act_nm.bin") 
df<-data.frame()
for (i in cus ){
  itemfreq <- table(md.dt[CUS_ID==i, ACT_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,model[[names(fitems), average=T]])
  df<-rbind(df,sim)
}
cus_gender<-rbind(cs.dt,test_pulic[,c(1,8)])
w2v_act<-df
w2v_act_train<-merge(cus_gender,by="CUS_ID",w2v_act)

write.csv(w2v_act_train,"w2v_act_mean_train.csv",row.names = F)

#####테스트적용
tr.dt2<-tr.dt2[!(tr.dt2$CUS_ID %in% cus),]
cus2<-unique(tr.dt2$CUS_ID)
df<-data.frame()
for (i in cus2 ){
  itemfreq <- table(tr.dt2[CUS_ID==i, ACT_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,model[[names(fitems), average=T]])
  df<-rbind(df,sim)
}

w2v_act_test<-df

write.csv(w2v_act_test,"w2v_act_mean_test.csv",row.names = F)

#####4.-2사이트 워투백####
setwd("C:\\Users\\Hyangsuk_Min\\Desktop\\데마팀플\\원데이터")
rm(list=ls())
# Install & load word2vec package
if(!require(devtools)) install.packages("devtools"); library(devtools)
if(!require(wordVectors)) install_github("bmschmidt/wordVectors"); library(wordVectors)

# Install & load data.table package
if(!require(data.table)) install.packages("data.table"); library(data.table)
if(!require(randomForest)) install.packages("randomForest"); library(randomForest)
if(!require(caret)) install.packages("caret"); library(caret)

# list objects in word2vec package
ls("package:wordVectors")

####트레인
###### Fast reading and combining several files using data.table (with fread): 5 times faster than read.csv()
cs.dt <- fread("train_profiles.csv")
tr.dt <- fread("train_clickstreams.tab"); tr.dt[,CUS_ID:= as.numeric(CUS_ID)]
setkey(cs.dt, CUS_ID); setkey(tr.dt, CUS_ID) 
md.dt <- merge(cs.dt, tr.dt)

test_pulic<-fread("test_public.csv")
test_pulic$GROUP<-ifelse(test_pulic$`F20-`==1,"F20-",
                         ifelse(test_pulic$F30==1,"F30",
                                ifelse(test_pulic$`F40+`==1,"F40+",
                                       ifelse(test_pulic$`M20-`,"M20-",
                                              ifelse(test_pulic$M30,"M30","M40+")))))
tr.dt2<-fread("test_clickstreams.tab"); tr.dt2[,CUS_ID:=as.numeric(CUS_ID)]
setkey(test_pulic,CUS_ID);setkey(tr.dt2,CUS_ID)
md.dt2<-merge(test_pulic[,c(1,8)],tr.dt2)
md.dt<-rbind(md.dt,md.dt2)

cus<-unique(md.dt$CUS_ID)
####
f <- function(x, t) {
  grp <- md.dt[CUS_ID==x, GROUP][1]
  itemfreq <- table(md.dt[CUS_ID==x,  SITE_NM])
  fitems <- itemfreq[itemfreq >= t]
  act <- names(fitems)
  #  
  sapply(act, function(x) gsub(" ", "_", x))
  set.seed(1)
  #
  as.vector((sapply(1:20, function(x) c(grp, sample(act)))))  ##방문한 사이트를 20번 샘플링 --> 5만개의 문장을 만들어서 학습시킴.
}
items <- unlist(sapply(cus, f, 2))
write.table(items, "items-site_nm.txt", eol = " ", quote = F, row.names = F, col.names = F)


##### Train site2vec model
model = train_word2vec("items-site_nm.txt","vec-site_nm.bin",vectors=300,threads=1,window=5,cbow=1,iter=5,negative_samples=10, force = T)
# model <- read.binary.vectors("vec-site_nm.bin") # reload the model. 
####벡터생성
model <- read.binary.vectors("vec-site_nm.bin") # reload the model. 
df<-data.frame()
for (i in cus ){
  itemfreq <- table(md.dt[CUS_ID==i, SITE_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,model[[names(fitems), average=T]])
  df<-rbind(df,sim)
}
cus_gender<-rbind(cs.dt,test_pulic[,c(1,8)])

w2v_site<-df

w2v_site_train<-merge(cus_gender,by="CUS_ID",w2v_site)

write.csv(w2v_site_train,"w2v_site_mean_train.csv",row.names = F)

####테스트적용
tr.dt2<-tr.dt2[!(tr.dt2$CUS_ID %in% cus),]
cus2<-unique(tr.dt2$CUS_ID)
df<-data.frame()
for (i in cus2 ){
  itemfreq <- table(tr.dt2[CUS_ID==i, SITE_NM])
  fitems <- itemfreq[itemfreq >= 1]
  sim <- data.frame(CUS_ID=i,model[[names(fitems), average=T]])
  df<-rbind(df,sim)
}

w2v_site_test<-df

write.csv(w2v_site_test,"w2v_site_mean_test.csv",row.names = F)

trf1<-read.csv("w2v_act_mean_train.csv")
trf2<-read.csv("w2v_site_mean_train.csv")
names(trf2)[-1:-2]<-paste("X",301:600,sep="")
tef1<-read.csv("w2v_act_mean_test.csv")
tef2<-read.csv("w2v_site_mean_test.csv")
names(tef2)[-1]<-paste("X",301:600,sep="")

trf<-merge(trf1,trf2,by="CUS_ID")
tef<-merge(tef1,tef2,by="CUS_ID")
trf[,303]<-NULL
names(trf)[2]<-"GROUP"
write.csv(trf,"train_final_w2v_all.csv",row.names = F)
write.csv(tef,"test_final_w2v_all.csv",row.names = F)

#####4.-3Training & Prediction ####
rm(list=ls())
train<-read.csv("train_final_w2v_all.csv")
test<-read.csv("test_final_w2v_all.csv")
train$GROUP<-make.names(train$GROUP)
control <- trainControl(method="repeatedcv", number=5, repeats=2, classProbs=TRUE, summaryFunction=mnLogLoss)

# Used models
methods <- c("svmRadial","gbm", "nnet","xgbTree") # add methods such as xgbTree, rf, svmRadious, etc.

models <- list()
for (i in 1:length(methods)) {
  print(methods[[i]])
  set.seed(123)
  model <- train(GROUP ~ .,
                 data = subset(train, select=-CUS_ID),
                 method = methods[i],
                 preProcess = NULL,
                 metric = "logLoss",
                 trControl = control)
  models[[i]] <- model
  summary(models[[i]]$results$logLoss)
}
names(models) <- methods
# Saving and loading models: 
saveRDS(models, "models-W2v-170612.rds") 
# models <- readRDS("models.rds")
results <- resamples(models)
summary(results)

modelCor(results)

# prediction & submission
for (i in 1:length(methods)) {
  pred <- predict(models[i], test, type="prob")
  fname <- paste("SUB-W2V3500-", methods[i], ".csv", sep="")
  write.csv(cbind(CUS_ID=test$CUS_ID,pred[[1]]), fname, row.names = F)
}
w1<-read.csv("SUB-W2V3500-nnet.csv")
w2<-read.csv("SUB-W2V3500-svmRadial.csv")
w3<-read.csv("SUB-W2V3500-gbm.csv")
w4<-read.csv("SUB-W2V3500-xgbTree.csv")

##SVM+NNET
pred<-merge(w1,w2,by="CUS_ID")
nc <- ncol(pred)
nf <- 2
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"SUB-W2V1.csv", row.names = F)
EN1<-read.csv("SUB-W2V1.csv")
##EN1+GBM
pred<-merge(EN1,w3,by="CUS_ID")
nc <- ncol(pred)
nf <- 2
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"SUB-W2V2.csv", row.names = F)
EN2<-read.csv("SUB-W2V2.csv")
##EN2+XGB
pred<-merge(EN2,w4,by="CUS_ID")
nc <- ncol(pred)
nf <- 2
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"SUB-W2V3.csv", row.names = F)

#####5.Ensemble####
te<-read.csv("test_public.csv")
#
en1<-read.csv("7조.csv")
names(en1)[-1]<-paste("en1_",names(en1)[-1],sep="")
en1<-en1[!(en1$CUS_ID %in% te$CUS_ID),]
#
en2<-read.csv("3조.csv")
names(en2)[-1]<-paste("en2_",names(en2)[-1],sep="")
en2<-en2[!(en2$CUS_ID %in% te$CUS_ID),]
#
en3<-read.csv("9조.csv")
names(en3)[-1]<-paste("en3_",names(en3)[-1],sep="")
en3<-en3[!(en3$CUS_ID %in% te$CUS_ID),]
#
en4<-read.csv("5차_1등_7조_submission.csv")
names(en4)[-1]<-paste("en4_",names(en4)[-1],sep="")
en4<-en4[!(en4$CUS_ID %in% te$CUS_ID),]
#
en5<-read.csv("5차_2등_8조_submission.csv")
names(en5)[-1]<-paste("en5_",names(en5)[-1],sep="")
en5<-en5[!(en5$CUS_ID %in% te$CUS_ID),]
#
en6<-read.csv("5차_3등_1조_submission.csv")
names(en6)[-1]<-paste("en6_",names(en6)[-1],sep="")
en6<-en6[!(en6$CUS_ID %in% te$CUS_ID),]
#
en7<-read.csv("FIN-m1+n-3500ENSEMBLE.csv")
names(en7)[-1]<-paste("en7_",names(en7)[-1],sep="")
en7<-en7[!(en7$CUS_ID %in% te$CUS_ID),]
#
en8<-read.csv("SUB-W2V3.csv")
names(en8)[-1]<-paste("en8_",names(en8)[-1],sep="")
en8<-en8[!(en8$CUS_ID %in% te$CUS_ID),]
#
en9<-read.csv("NEW7-3500ENSEMBLE.csv")
names(en9)[-1]<-paste("en9_",names(en9)[-1],sep="")
en9<-en9[!(en9$CUS_ID %in% te$CUS_ID),]
#
en10<-read.csv("FIN-n2v-ensemble3.csv")
names(en10)[-1]<-paste("en10_",names(en10)[-1],sep="")
en10<-en10[!(en10$CUS_ID %in% te$CUS_ID),]

###Ensemble1-en1+en2+en3
pred<-merge(en1,merge(en2,en3,by="CUS_ID"),by="CUS_ID")
nc <- ncol(pred)
nf <- 3
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"ensemble1.csv", row.names = F)

ensemble1<-read.csv("ensemble1.csv")

##Ensemble2-ensemble1+en5+en6
pred<-merge(ensemble1,merge(en5,en6,by="CUS_ID"),by="CUS_ID")
nc <- ncol(pred)
nf <- 3
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"ensemble2.csv", row.names = F)

ensemble2<-read.csv("ensemble2.csv")

##Ensemble3-ensemble2+en4
pred<-merge(ensemble2,en4,by="CUS_ID")
nc <- ncol(pred)
nf <- 2
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"ensemble3.csv", row.names = F)


ensemble3<-read.csv("ensemble3.csv")

##Ensemble4-en7+en8+en10
pred<-merge(en7,merge(en8,en10,by="CUS_ID"),by="CUS_ID")
nc <- ncol(pred)
nf <- 3
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"ensemble4.csv", row.names = F)

ensemble4<-read.csv("ensemble4.csv")

##Ensemble5-en9+en10
pred<-merge(en9,en10,by="CUS_ID")
nc <- ncol(pred)
nf <- 2
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"ensemble5.csv", row.names = F)

ensemble5<-read.csv("ensemble5.csv")


##ensemble6-ensemble4+ensemble5
pred<-merge(ensemble4,ensemble5,by="CUS_ID")
nc <- ncol(pred)
nf <- 2
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"ensemble6.csv", row.names = F)

ensemble6<-read.csv("ensemble6.csv")

##ensemble7-ensemble3+ensemble6
pred<-merge(ensemble4,ensemble6,by="CUS_ID")
nc <- ncol(pred)
nf <- 2
for (i in 1:6) {
  x <- 1
  for (j in 0:(nf-1)) x <- x * pred[,i+j*6+1]
  pred[nc+i] <- x^(1/nf)
}
pred[2:(ncol(pred)-6)] <- NULL
pred <- cbind(pred[1], pred[-1] / rowSums(pred[-1]))

names(pred) <- c("CUS_ID", "F20-", "F30", "F40+", "M20-", "M30", "M40+")
write.csv(pred,"7조-최종.csv", row.names = F)

final<-read.csv("7조-최종.csv")


####6.AR--안함####
rm(list=ls())
setwd("C:\\Users\\Hyangsuk_Min\\Desktop\\데마팀플\\원데이터")

library(data.table)
if(!require(arules)) install.packages("arules"); library(arules)
if(!require(arulesViz)) install.packages("arulesViz"); library(arulesViz)
if(!require(arulesCBA)) install.packages("arulesCBA"); library(arulesCBA)
if(!require(arc)) install.packages("arc") ; library(arc)

tp<-fread("train_profiles.csv");tp[,CUS_ID:= as.numeric(CUS_ID)]
trc<-fread("train_clickstreams(more_detailed).csv");trc[,CUS_ID:= as.numeric(CUS_ID)]
tec<-fread("test_clickstreams(more_detailed).csv");tec[,CUS_ID:=as.numeric(CUS_ID)]
test_pulic<-fread("test_public.csv")
test_pulic$GROUP<-test_pulic$GROUP<-ifelse(test_pulic$`F20-`==1,"F20-",
                                           ifelse(test_pulic$F30==1,"F30",
                                                  ifelse(test_pulic$`F40+`==1,"F40+",
                                                         ifelse(test_pulic$`M20-`,"M20-",
                                                                ifelse(test_pulic$M30,"M30","M40+")))))
tr<-rbind(trc,tec[tec$CUS_ID %in% test_pulic$CUS_ID,])
tr.filter2<-tr %>% distinct(CUS_ID,NEW_NM)
tp2<-rbind(tp,test_pulic[,c(1,8)])
names(tp2)[2]<-"NEW_NM"
tr.filter2<-rbind(tr.filter2,tp2)
tr.filter2<-tr.filter2[order(tr.filter2$CUS_ID),]
trans<-as(split(tr.filter$NEW_NM,tr.filter$CUS_ID),"transactions")

rules <- apriori(trans, parameter=list(support=0.005, confidence=0.7,minlen=1))

rules.target <- subset(rules, rhs %in% c("F20-","F30","F40+","M20-","M30","M40+") & lift > 1)

inspect(sort(rules.target,by="confidence"))
inspect(sort(rules.target,by=c("confidence","support","lift")))

ru<-as(rules.target,"data.frame")
ru$rules<-as.character(ru$rules)
ru1<-data.frame(strsplit(ru$rules,"=>"))
ru1<-t(ru1)
ru$GROUP<-ru1[,2]
write.csv(ru,"association-rules.csv",row.names = F)


####
tec<-fread("test_clickstreams(more_detailed).csv");tec[,CUS_ID:=as.numeric(CUS_ID)]
test_pulic<-fread("test_public.csv")

tec2<-tec[!(tec$CUS_ID %in% test_pulic$CUS_ID),]
te.f<-tec2 %>% group_by(CUS_ID,NEW_NM) %>% summarise(tot=n())

a<-te.f[te.f$NEW_NM %in% c("골프 회원권 거래소","포털금융"),]
a<-te.f[te.f$NEW_NM %in% c("IT뉴스","골프 회원권 거래소"),]
a<-te.f[te.f$NEW_NM %in% c("{골프 회원권 거래소","오마이뉴스"),]
a<-te.f[te.f$NEW_NM %in% c("골프 회원권 거래소","오마이뉴스"),]
a<-te.f[te.f$NEW_NM %in% c("골프 회원권 거래소","디지털타임스"),]
a<-te.f[te.f$NEW_NM %in% c("URL단순화","방송기타"),]
a<-te.f[te.f$NEW_NM %in% c("데일리안","방송기타"),]
a<-te.f[te.f$NEW_NM %in% c("기능대학","중고차쇼핑몰"),]
a<-te.f[te.f$NEW_NM %in% c("오마이뉴스","지역인터넷방송"),]
a<-te.f[te.f$NEW_NM %in% c("뉴데일리","지역인터넷방송"),]
a<-te.f[te.f$NEW_NM %in% c("고무/합성수지","로또정보"),]
a<-te.f[te.f$NEW_NM %in% c("이동통신브랜드","천주교"),]
a<-te.f[te.f$NEW_NM %in% c("다음 뷰","야후! 메일"),]
a<-te.f[te.f$NEW_NM %in% c("야후! 메일","포털금융"),]
a<-te.f[te.f$NEW_NM %in% c("디지털타임스","야후! 메일"),]
a<-te.f[te.f$NEW_NM %in% c("문화일보","야후! 메일"),]
a<-te.f[te.f$NEW_NM %in% c("YTN","야후! 메일"),]
a<-te.f[te.f$NEW_NM %in% c("검색엔진","사회/정치뉴스"),]
a<-te.f[te.f$NEW_NM %in% c("OSEN","사회/정치뉴스"),]
a<-te.f[te.f$NEW_NM %in% c("다음 지도","자동차쇼핑몰"),]
a<-te.f[te.f$NEW_NM %in% c("기독교인터넷방송","대통령선거"),]
a<-te.f[te.f$NEW_NM %in% c("기독교인터넷방송","증권/투자정보"),]
a<-te.f[te.f$NEW_NM %in% c("교육지원청","증권/투자정보"),]
a<-te.f[te.f$NEW_NM %in% c("인터넷납부","지역개발"),]
a<-te.f[te.f$NEW_NM %in% c("전자결제/전자화폐","지역개발"),]
a<-te.f[te.f$NEW_NM %in% c("대통령선거","바둑/장기"),]
a<-te.f[te.f$NEW_NM %in% c("동아닷컴","바둑/장기"),]
a<-te.f[te.f$NEW_NM %in% c("바둑/장기","자동차전문지"),]
a<-te.f[te.f$NEW_NM %in% c("바둑/장기","비뇨기과"),]
a<-te.f[te.f$NEW_NM %in% c("바둑/장기","종합블로그"),]
a<-te.f[te.f$NEW_NM %in% c("바둑/장기","캘린더/일정관리"),]
a<-te.f[te.f$NEW_NM %in% c("바둑/장기","다나와"),]
a<-te.f[te.f$NEW_NM %in% c("바둑/장기","SNS"),]
a<-te.f[te.f$NEW_NM %in% c("바둑/장기","다음 뮤직"),]
a<-te.f[te.f$NEW_NM %in% c("바둑/장기"),]
a<-te.f[te.f$CUS_ID %in% a$CUS_ID,]
a<-te.f[te.f$NEW_NM %in% c("다음 지식검색","바둑/장기"),]
a<-te.f[te.f$NEW_NM %in% c("정치인","정치인 팬클럽"),]
a<-te.f[te.f$NEW_NM %in% c("Warning","지역/상가포털"),]
a<-te.f[te.f$NEW_NM %in% c("데스크탑가젯","스포츠서울닷컴"),]
a<-te.f[te.f$NEW_NM %in% c("야구정보"),]
a<-te.f[te.f$CUS_ID %in% a$CUS_ID,]
a<-te.f[te.f$NEW_NM %in% c("기숙학원"),]
a<-te.f[te.f$CUS_ID %in% a$CUS_ID,]
a<-te.f[te.f$NEW_NM %in% c("정치포럼/단체"),]
a<-te.f[te.f$CUS_ID %in% a$CUS_ID,]
a<-te.f[te.f$NEW_NM %in% c("과학전문지","여행신문"),]
a<-te.f[te.f$NEW_NM %in% c("악기쇼핑몰","자료실"),]
a<-te.f[te.f$NEW_NM %in% c("야후! 미디어","주유정보"),]
a<-te.f[te.f$NEW_NM %in% c("의료기기제조"),]
a<-te.f[te.f$CUS_ID %in% a$CUS_ID,]
a<-te.f[te.f$NEW_NM %in% c("사이버가정학습"),]
a<-te.f[te.f$CUS_ID %in% a$CUS_ID,]
a<-te.f[te.f$NEW_NM %in% c("기타자연과학","대통령선거"),]
a<-te.f[te.f$NEW_NM %in% c("야후! 미디어","엠게임"),]
a<-te.f[te.f$NEW_NM %in% c("내비게이션/GPS","메일계정"),]
a<-te.f[te.f$NEW_NM %in% c("골프장"),]
a<-te.f[te.f$CUS_ID %in% a$CUS_ID,]
a<-te.f[te.f$NEW_NM %in% c("생활정보신문","컴퓨터 유지/보수"),]
a<-te.f[te.f$NEW_NM %in% c("컴퓨터 유지/보수","포털증권"),]
a<-te.f[te.f$NEW_NM %in% c("성인정보","의료기기쇼핑몰"),]
a<-te.f[te.f$NEW_NM %in% c("고속도로정보"),]
a<-te.f[te.f$CUS_ID %in% a$CUS_ID,]
a<-te.f[te.f$NEW_NM %in% c("컴퓨터장치/주변기기","포털뉴스"),]
a<-te.f[te.f$NEW_NM %in% c("생활정보신문","포털뉴스"),]
a<-te.f[te.f$NEW_NM %in% c("자동차 커뮤니티","철물/공구쇼핑몰"),]
a<-te.f[te.f$NEW_NM %in% c("Warning","철물/공구쇼핑몰"),]
a<-te.f[te.f$NEW_NM %in% c("철물/공구쇼핑몰","한의원/한방병원"),]
a<-te.f[te.f$NEW_NM %in% c("아케이드/고전게임","조인스 뉴스"),]
a<-te.f[te.f$NEW_NM %in% c("스포츠/취미전문지"),]
a<-te.f[te.f$CUS_ID %in% a$CUS_ID,]
a<-te.f[te.f$NEW_NM %in% c("영화전문지","컴퓨터정보전문지"),]

a<-te.f[te.f$NEW_NM %in% c("골프쇼핑몰"),]
a<-te.f[te.f$CUS_ID %in% a$CUS_ID,]
a<-te.f[te.f$NEW_NM %in% c("날씨정보"),]
a<-te.f[te.f$CUS_ID %in% a$CUS_ID,]
a<-te.f[te.f$NEW_NM %in% c("Warning","온라인다이어트정보"),]
a<-te.f[te.f$NEW_NM %in% c("대통령선거","온라인다이어트정보"),]
a<-te.f[te.f$NEW_NM %in% c("넥슨","영상/음향기기쇼핑몰"),]
a<-te.f[te.f$NEW_NM %in% c("군대 커뮤니티","전자책(eBook)"),]
a<-te.f[te.f$NEW_NM %in% c("식품제조","유아용품제조"),]
a<-te.f[te.f$NEW_NM %in% c("가계부","종합화장품쇼핑몰"),]
a<-te.f[te.f$NEW_NM %in% c("가계부","웹에이전시"),]
a<-te.f[te.f$NEW_NM %in% c("통합마일리지","학생교육업체"),]
a<-te.f[te.f$NEW_NM %in% c("온라인사진인화","학생교육업체"),]
a<-te.f[te.f$NEW_NM %in% c("식품제조","유아용품제조"),]
a<-te.f[te.f$NEW_NM %in% c("유아용품제조","학생교육업체"),]
a<-te.f[te.f$NEW_NM %in% c("위생용품제조","학생교육업체"),]
a<-te.f[te.f$NEW_NM %in% c("게임쇼핑몰"),]
a<-te.f[te.f$CUS_ID %in% a$CUS_ID,]
a<-te.f[te.f$NEW_NM %in% c("사이트중개/매매","신한카드"),]
a<-te.f[te.f$NEW_NM %in% c("PMP/내비게이션 커뮤니티","유아용품쇼핑몰"),]
a<-te.f[te.f$NEW_NM %in% c("공공기업/경제","로그분석/카운터"),]
a<-te.f[te.f$NEW_NM %in% c("사법기관","산업기계/장비"),]
a<-te.f[te.f$NEW_NM %in% c("기상청","비즈플랫폼"),]
a<-te.f[te.f$NEW_NM %in% c("게임 커뮤니티","원격/화상/교육솔루션"),]
a<-te.f[te.f$NEW_NM %in% c("무료게시판","자동차 커뮤니티"),]
a<-te.f[te.f$NEW_NM %in% c("애완동물","자동차용품쇼핑몰"),]
a<-te.f[te.f$NEW_NM %in% c("공공기업/경제","로그분석/카운터"),]
a<-te.f[te.f$NEW_NM %in% c("사법기관","산업기계/장비"),]
a<-te.f[te.f$NEW_NM %in% c("인터넷팩스","전략시뮬레이션"),]
a<-te.f[te.f$NEW_NM %in% c("광고/마케팅관리","자동차 커뮤니티"),]
a<-te.f[te.f$NEW_NM %in% c("국내프로야구단","자동차브랜드"),]
a<-te.f[te.f$NEW_NM %in% c("기타교육기관","자동차 커뮤니티"),]
a<-te.f[te.f$NEW_NM %in% c("공공복지","휴대폰 커뮤니티"),]

##AR 이용
en<-final
--d#M40+
en[en$CUS_ID==2777,-1]<-c(0,0,0,0,0,1)
en[en$CUS_ID==3025,-1]<-c(0,0,0,0,0,1)
en[en$CUS_ID==2830,-1]<-c(0,0,0,0,0,1)
en[en$CUS_ID==3993,-1]<-c(0,0,0,0,0,1)
en[en$CUS_ID==3218,-1]<-c(0,0,0,0,0,1)
en[en$CUS_ID==3713,-1]<-c(0,0,0,0,0,1)
en[en$CUS_ID==4704,-1]<-c(0,0,0,0,0,1)
#F30
en[en$CUS_ID==4550,-1]<-c(0,1,0,0,0,0)
en[en$CUS_ID==4029,-1]<-c(0,1,0,0,0,0)
en[en$CUS_ID==2734,-1]<-c(0,1,0,0,0,0)
en[en$CUS_ID==4172,-1]<-c(0,1,0,0,0,0)
#M30
en[en$CUS_ID==2592,-1]<-c(0,0,0,0,1,0)
write.csv(en,"7조-최종-AR.csv",row.names=F)
