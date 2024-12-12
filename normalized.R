library(data.table)
library(dplyr)
library(ggplot2)
dt <- fread("/Users/acelynnlin/Documents/113-1/Data_Science/Project/Result.csv")
# 改變欄位名稱
setnames(dt, old = "中國國民黨平均分數", new = "value")

# 查看結果

dt[, normalized_value := (value - mean(value)) / sd(value), by = Type]


dt[, Final := ifelse(normalized_value >= 0, "國民黨","民進黨")]

after <- fread('/Users/acelynnlin/Documents/113-1/Data_Science/Project/政治人物名單 - Sheet5-2.csv')
after <- rbind(after, data.table(name = "柯文哲", party = "台灣民眾黨"))
dt<- dt %>%
  left_join(after,by = "name")

dt <- dt %>%
  mutate("party" = ifelse(國民黨標竿人物== 1, "國民黨",ifelse(民進黨標竿人物== 1, "民進黨",party)))

summary <- dt%>%
  filter(Final != party) %>%
  group_by(Type,party,Final)%>%
  summarise(count = n())

  
dt %>%
  filter(Final != party)%>%
  filter(party == "國民黨")%>%
  select(name, Type)

dt %>%
  filter(name == "柯文哲")

fwrite(dt,"/Users/acelynnlin/Documents/113-1/Data_Science/Project/Normalized.csv" )
