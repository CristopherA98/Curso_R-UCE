library(haven)
personas <- read_sav("Datasets/SPSS_enemdu_201912/enemdu_persona_201912.sav")
hogar <- read_sav("Datasets/SPSS_enemdu_201912/enemdu_viv_hog_201912.sav")

names(personas)
names(hogar)

glimpse(personas)
glimpse(hogar)

join <- merge(personas, hogar, by= c("area","ciudad","conglomerado",
                                     "panelm","vivienda","hogar","estrato",
                                     "fexp","upm","id_vivienda","id_hogar",
                                     "periodo"),
              all.x = TRUE)

names(join)


# DIMENSION 1

dim1 <- hogar %>%
  filter(vi02 %in% 1:8, #print_labels(hogar$vi02)
         #1.1. Material predominante de las paredes 
         (vi05a %in% 5:7 | #print_labels(hogar$vi05a);print_labels(hogar$vi04a)
            #1.2. Material predominante de las piso 
            vi04a %in% 6:7)) %>%
  select(area,ciudad,conglomerado,
         panelm,vivienda,hogar,estrato,
         fexp,upm,id_vivienda,id_hogar, vi02,vi05a,vi04a) %>%
  mutate(dim1 = 1)
                                