library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)

data<-read_excel("tms_raw.xlsx", sheet = "raw_hourly") #NA나 이상값들은 모두 공백처리
#어째서 transmute가 제대로 작동 안하는지 모르겠음 그래서 mutate후에 data에 덮어씌우는 형태로 진행

data<-mutate(data, date=as.POSIXct(as.character(data$date), tz="Asia/Seoul", origin = "1970-01-01", format = "%Y%m%d%H"))

#summarise에 계속 NA가 나타나는 현상이 발생, NA가 하나도 없어야 함수가 작동함, NA를 빼고 처리하는 방법은 아직 알지못해서 rawdata의 NA를 전부 삭제

#용인, 남양주, 안양, 평택은 도로변이 없음
Sw_mobile<-filter(data, city=="수원", type=="이동차")
Sw_city<-filter(data, city=="수원", type=="도시대기")
Sw_road<-filter(data, city=="수원", type=="도로변")

Yi_mobile<-filter(data, city=="용인", type=="이동차")
Yi_city<-filter(data, city=="용인", type=="도시대기")

Nyj_mobile<-filter(data, city=="남양주", type=="이동차")
Nyj_city<-filter(data, city=="남양주", type=="도시대기")

Sn_mobile<-filter(data, city=="성남", type=="이동차")
Sn_city<-filter(data, city=="성남", type=="도시대기")
Sn_road<-filter(data, city=="성남", type=="도로변")

Ay_mobile<-filter(data, city=="안양", type=="이동차")
Ay_city<-filter(data, city=="안양", type=="도시대기")
Ay_road<-filter(data, city=="안양", type=="도로변")

Bc_mobile<-filter(data, city=="부천", type=="이동차")
Bc_city<-filter(data, city=="부천", type=="도시대기")
Bc_road<-filter(data, city=="부천", type=="도로변")

Gy_mobile<-filter(data, city=="고양", type=="이동차")
Gy_city<-filter(data, city=="고양", type=="도시대기")
Gy_road<-filter(data, city=="고양", type=="도로변")

Pt_mobile<-filter(data, city=="평택", type=="이동차")
Pt_city<-filter(data, city=="평택", type=="도시대기")
Pt_road<-filter(data, city=="평택", type=="도로변")

#WD를 위한 최빈값 구하는 함수
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#아래와 같이 실행했지만 각 시간별 데이터는 뜨지 않음
sum_Sw_city<-Sw_city %>% group_by(date) %>% summarise(city=first(city), type=first(type), site_name=first(site_name), site_number=first(site_number), SO2=mean(SO2), 
                                         NO=mean(NO), NO2=mean(NO2), Nox=mean(Nox), CO=mean(CO), O3=mean(O3), PM10=mean(PM10), PM25=mean(PM25), WD=getmode(WD),
                                         ws=mean(WS), temp=mean(temp), RH=mean(RH))

#다만 이렇게 실행하면 각 시간별 자료가 모두 뜸 but 평균을 낼 수 없는 자료들은 NA처리됨 warning 발생
sumeach_Sw_city<-Sw_city %>% group_by(date) %>% summarise_each(funs(mean))
