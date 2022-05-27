library("tabulizer")
library(stringr)

marvel_dc<- tabulizer::extract_tables("110_Fernanda Pereira da Silva.pdf")

marvel_dc_data<- marvel_dc[[8]]

marvel_dc_data_2<- marvel_dc[[9]]

marvel_dc_data_3<- marvel_dc[[10]]


marvel_dc_characters<-
  tibble(marv)


marvel_dc_characters<- as.data.frame(marvel_dc_data[,3:6])

names(marvel_dc_characters)<- c("nome", "ano", "empresa", "genero")

marvel_dc_characters_work<-marvel_dc_characters%>%
  filter(!(nome==""& ano=="" & empresa=="" & genero==""))

marvel_dc_characters_work<- marvel_dc_characters_work[-c(1:7),]

for (linha in 1:NROW(marvel_dc_characters_work)) {
  if marvel_dc_characters_work[linha,2]==""(
    marvel_dc_characters_work[linha,1]<- paste(marvel_dc_characters_work[linha,1],
                                               marvel_dc_characters_work[linha+2,1])
  )

}

marvel_dc_characters_work<- edit(marvel_dc_characters_work)

marvel_dc_characters_work<- bind_rows(marvel_dc_characters[13,],marvel_dc_characters_work)

marvel_dc_characters_work<-
marvel_dc_characters_work %>%
  filter(ano!="")



marvel_dc_characters_work_2<-
bind_rows(as.data.frame(marvel_dc_data_2[1:8,3:6]), as.data.frame(marvel_dc_data_2[9:33,2:5]))

names(marvel_dc_characters_work_2) <- names(marvel_dc_characters_work)


marvel_dc_characters_work<-
  marvel_dc_characters_work %>%
  bind_rows(marvel_dc_characters_work_2)


marvel_dc_characters_work_3<- as.data.frame(marvel_dc_data_3[,2:5])
names(marvel_dc_characters_work_3) <- names(marvel_dc_characters_work)

marvel_dc_characters_work_3 <- edit(marvel_dc_characters_work_3)

marvel_dc_characters_work<-
marvel_dc_characters_work %>%
  bind_rows(
    marvel_dc_characters_work_3 %>%
      filter(ano!="")
  )


marvel_dc_characters_final<-
marvel_dc_characters_work %>%
  mutate(ano =as.numeric(str_sub(ano,1,4) ),
    era= case_when(
      ano <= 1937 ~ "Era de Platina dos Quadrinhos",
      ano >1937 & ano<=1955 ~"Era de Ouro dos Quadrinhos",
      ano >1955 & ano<= 1969 ~"Era de Prata dos Quadrinhos",
      ano >1969 & ano<= 1979 ~ "Era de Bronze dos Quadrinhos",
      ano> 1979 ~ "Era Moderna dos Quadrinhos"

    )
  )

marvel_dc_characters_final %>%
  readr::write_csv(file="super_herois_negros_negras.csv")
