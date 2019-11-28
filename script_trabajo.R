#----Librerías----
library(rmdformats)
library(rvest)
library(stringr)
library(tidytext)
library(dplyr)
library(ggplot2)
library(wordcloud2)
library(reshape2)
library(tidyr)
library(stopwords)
library(htmlwidgets)
library(kableExtra)
library(RColorBrewer)

#---- Recursos----
#Palabras más recurrentes en español.
stop_words_esp <- stopwords(language = "es", source = "stopwords-iso")
stop_words_esp <- as.data.frame(stop_words_esp)
colnames(stop_words_esp) <- "text"
stop_words_esp$text <- as.character(stop_words_esp$text)

#Generamos una paleta de colores específica para el número de palabras a representar
colores_azules <- colorRampPalette(brewer.pal(9,"Blues"))
colores_rojos <- colorRampPalette(brewer.pal(9,"Reds"))
colores_naranjas <- colorRampPalette(brewer.pal(9,"Oranges"))
colores_verdes <- colorRampPalette(brewer.pal(9,"Greens"))

#---- Web scrapping----

# URL con la misma noticia seleccionada.
el_pais_url <- "https://elpais.com/ccaa/2019/11/18/catalunya/1574062344_940943.html"
el_mundo_url <- "https://www.elmundo.es/cataluna/2019/11/18/5dd1a7ba21efa0b6248b4606.html"
ok_diario_url <- "https://okdiario.com/espana/torra-dice-que-desobedecio-porque-junta-electoral-no-esta-encima-cataluna-4828375"
el_periodico_url <- "https://www.elperiodico.com/es/politica/20191118/declaracion-quim-torra-juicio-tsjc-desobediencia-pancarta-lazos-amarillos-7740469"

# Accedemos a los elementos que me interesan. En este caso los contenidos de los párrafos.
# 1- El Mundo.
tmp <- read_html(el_mundo_url)
tmp <- html_nodes(tmp, "article")
tmp <- repair_encoding(html_text(tmp[1]))

# Dividimos toda la cadena de texto en bloques
bloque_el_mundo <- substring(tmp,1516,nchar(tmp))
bloque1_mundo <- substring(bloque_el_mundo,1,2905)
bloque2_mundo <- substring(bloque_el_mundo,3577,nchar(bloque_el_mundo))
bloque2_mundo <- substring(bloque2_mundo,1,(nchar(bloque2_mundo)-426))
# y lo juntamos.
bloque_el_mundo <- paste(bloque1_mundo," ",bloque2_mundo)

# 2- Ok Diario.

tmp2 <- read_html(ok_diario_url)
tmp2 <- html_nodes(tmp2, "section")
tmp2 <- tmp2[1]
tmp2 <- repair_encoding(html_text(tmp2)) #reparamos todo el codigo para que sea UTF-8


bloque_ok_diario <- substring(tmp2,930,nchar(tmp2))
bloque_ok_diario <- substring(bloque_ok_diario,1,2209) #Conseguido! Ahora falta formatearlo.
# 3- El Periodico.

tmp3 <- read_html(el_periodico_url)
tmp3 <- html_nodes(tmp3, "section")
tmp3 <- tmp3[3]
tmp3 <- html_text(tmp3)
tmp3 <- repair_encoding(tmp3) #reparamos todo el codigo para que sea UTF-8

bloque_el_periodico <- substring(tmp3,1051,nchar(tmp3))
bloque_el_periodico_1 <- substring(bloque_el_periodico,1,695) #Conseguido! Ahora falta formatearlo.
bloque_el_periodico_2 <- substring(bloque_el_periodico,928,nchar(bloque_el_periodico)-3514)
bloque_el_periodico <- paste(bloque_el_periodico_1," ",bloque_el_periodico_2)

# 4 - El País.

tmp4 <- read_html(el_pais_url)
tmp4 <- html_nodes(tmp4, "article")
tmp4 <- tmp4[1]
tmp4 <- html_text(tmp4)
tmp4 <- repair_encoding(tmp4) #reparamos todo el codigo para que sea UTF-8

bloque_el_pais <- substring(tmp4,3223,nchar(tmp4))
bloque_el_pais_1 <- substring(bloque_el_pais,1,642)
bloque_el_pais_2 <- substring(bloque_el_pais,953,1794)
bloque_el_pais_3 <- substring(bloque_el_pais,3779,4714)
bloque_el_pais_4 <- substring(bloque_el_pais,4737,5071)
bloque_el_pais_5 <- substring(bloque_el_pais,7090,10443)
bloque_el_pais <- paste(bloque_el_pais_1," ",bloque_el_pais_2," ",bloque_el_pais_3," ",
                        bloque_el_pais_4," ",bloque_el_pais_5) #Conseguido! Ahora falta formatearlo.

#---- FORMATEANDO----

# Vamos a quitar todos los caracteres "/" empleando Gsub y expresioner regulares.
# 1. El Mundo.
bloque_el_mundo <- gsub("[^a-zA-ZáéíóúÁÉÍÓÚñ.,:0-9 ]","",bloque_el_mundo)
bloque_el_mundo <- tolower(bloque_el_mundo)
bloque_el_mundo <- gsub("  +"," ",bloque_el_mundo)


# 2. OkDiario.
bloque_ok_diario <- gsub(("[^a-zA-ZáéíóúÁÉÍÓÚñ.,:0-9 ]"),"",bloque_ok_diario)
bloque_ok_diario <- tolower(bloque_ok_diario)

# 3. El Periódico.
bloque_el_periodico <- gsub("[^a-zA-ZáéíóúÁÉÍÓÚñ.,:0-9 ]","",bloque_el_periodico)
bloque_el_periodico <- gsub("  +"," ",bloque_el_periodico)
bloque_el_periodico <- gsub("rdquor","",bloque_el_periodico)
bloque_el_periodico <- tolower(bloque_el_periodico)

# 4. El País.
bloque_el_pais <- gsub(("[^a-zA-ZáéíóúÁÉÍÓÚñ.,:0-9 ]"),"",bloque_el_pais)
bloque_el_pais <- gsub("  +"," ",bloque_el_pais)
bloque_el_pais <- tolower(bloque_el_pais)


#---- TIDYTEXT ----
#-- Frecuencia de palabras --
bloques_raw <- c(el_mundo = bloque_el_mundo, el_pais = bloque_el_pais, 
                 ok_diario = bloque_ok_diario, el_periodico = bloque_el_periodico)

bloques_df <- tibble(line = 1:4 , text = bloques_raw) %>%
  unnest_tokens(word, text) %>%
  as.data.frame()

bloques_df <-anti_join(bloques_df,stop_words_esp, by = c("word"="text"))

bloques_df[bloques_df$line == 1,1] <- "el_mundo"
bloques_df[bloques_df$line == 2,1] <- "el_pais"
bloques_df[bloques_df$line == 3,1] <- "ok_diario"
bloques_df[bloques_df$line == 4,1] <- "el_periodico"

bloques_count <- bloques_df %>%
  count(line, word, sort = T) %>%
  as.data.frame()

colnames(bloques_count) <- c("periodico","palabra","n")

# filtraremos por periodico y generaremos otras tablas para aquellas palabras que se repiten
# más de una vez.
bloques_count_mundo <- bloques_count[bloques_count$periodico == "el_mundo",]
bloques_count_mundo_f <- bloques_count_mundo[bloques_count_mundo$n > 1,]

bloques_count_pais <- bloques_count[bloques_count$periodico == "el_pais",]
bloques_count_pais_f <- bloques_count_pais[bloques_count_pais$n > 1,]

bloques_count_okdiario <- bloques_count[bloques_count$periodico == "ok_diario",]
bloques_count_okdiario_f <- bloques_count_okdiario[bloques_count_okdiario$n > 1,]

bloques_count_periodico <- bloques_count[bloques_count$periodico == "el_periodico",]
bloques_count_periodico_f <- bloques_count_periodico[bloques_count_periodico$n > 1,]

bloques <- rbind(bloques_count_mundo_f[1:5,],bloques_count_pais_f[1:5,],
                 bloques_count_okdiario_f[1:5,],bloques_count_periodico_f[1:5,])


bloques2 <- rbind(bloques_count_mundo_f,bloques_count_pais_f,
                  bloques_count_okdiario_f,bloques_count_periodico_f)



#-- Bigramas --
# Empleando las funciones de TidyText es muy sencillo llevar esto a cabo

bloques_bigrams_df <- tibble(line = 1:4 , text = bloques_raw) %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  as.data.frame()

bloques_bigrams_df[bloques_bigrams_df$line == 1,1] <- "el_mundo"
bloques_bigrams_df[bloques_bigrams_df$line == 2,1] <- "el_pais"
bloques_bigrams_df[bloques_bigrams_df$line == 3,1] <- "ok_diario"
bloques_bigrams_df[bloques_bigrams_df$line == 4,1] <- "el_periodico"

bigrams_separated <- bloques_bigrams_df %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Quitamos palabras más comunes en español
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words_esp$text) %>%
  filter(!word2 %in% stop_words_esp$text)

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigram_counts <- bigrams_united %>%
  count(line, bigram, sort = T) %>%
  as.data.frame()

# Filtro para bigramas más recurrentes
bigram_counts <- bigram_counts[bigram_counts$n > 1,]

# -- Palabras únicas --
# Empleamos la función DCAST y obtenemos las "máscaras" (filtros booleanos) para filtrar por
# palabras únicas.
bloques_count_dcast <- dcast(bloques2, palabra ~ periodico, fun.aggregate = sum)

unicas_mundo <- bloques_count_dcast$el_mundo != 0 & bloques_count_dcast$el_pais == 0 &
  bloques_count_dcast$el_periodico == 0 & bloques_count_dcast$ok_diario == 0

unicas_pais <- bloques_count_dcast$el_mundo == 0 & bloques_count_dcast$el_pais != 0 &
  bloques_count_dcast$el_periodico == 0 & bloques_count_dcast$ok_diario == 0

unicas_okdiario <- bloques_count_dcast$el_mundo == 0 & bloques_count_dcast$el_pais == 0 &
  bloques_count_dcast$el_periodico == 0 & bloques_count_dcast$ok_diario != 0
unicas_periodico <- bloques_count_dcast$el_mundo == 0 & bloques_count_dcast$el_pais == 0 &
  bloques_count_dcast$el_periodico != 0 & bloques_count_dcast$ok_diario == 0


tabla_unicas_mundo <- bloques_count_dcast[unicas_mundo,]
tabla_unicas_pais <- bloques_count_dcast[unicas_pais,]
tabla_unicas_okdiario <- bloques_count_dcast[unicas_okdiario,]
tabla_unicas_periodico <- bloques_count_dcast[unicas_periodico,]

#Facet palabras más repetidas por diario
ggplot(bloques, aes(x = reorder_within(palabra, -n, periodico), y = n)) +
  geom_bar(stat ="identity", aes(fill = periodico, alpha = n) , size = 5) +
  scale_x_reordered() +
  scale_fill_manual("periodico", 
                    values = c("el_mundo" = list(colores_azules(9))[[1]][9],
                               "el_pais" = list(colores_rojos(9))[[1]][9],
                               "el_periodico" = list(colores_naranjas(9))[[1]][9],
                               "ok_diario" = list(colores_verdes(9))[[1]][9])) +
  labs(title = "Palabras más repetidas", x = "Palabra", y = "Recuento", 
       subtitle = "Agrupadas por periódico") +
  scale_y_continuous(breaks = seq(0,15,2))+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        title = element_text(size = 15, lineheight = 1),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 10 )
  )+
  facet_grid(~periodico, scales = "free_x", space = "free")

#Facet bigramas más repetidos por diario

ggplot(bigram_counts, aes(x = reorder_within(bigram, -n, line), y = n)) +
  geom_bar(stat ="identity", aes(fill = line, alpha = n)) +
  scale_x_reordered() +
  scale_fill_manual("periodico", values = c("el_mundo" = list(colores_azules(9))[[1]][9],
                                            "el_pais" = list(colores_rojos(9))[[1]][9],
                                            "el_periodico" = list(colores_naranjas(9))[[1]][9],
                                            "ok_diario" = list(colores_verdes(9))[[1]][9])) +
  labs(title = "Bigramas más repetidos.", x = "Bigrama", y = "Recuento", 
       subtitle = "Agrupados por periódico.") +
  scale_y_continuous(breaks = seq(0,15,2)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5),
        title = element_text(size = 15, lineheight = 1),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, size = 10 )
  )+
  facet_grid(~line, scales = "free_x", space = "free")

# Nubes de palabras

# 1. El Mundo
hw = wordcloud2(bloques_count_mundo[,2:3],color=rev(colores_azules(nrow(bloques_count_mundo))))
#Pasando el plot para que se pueda visualizar en html
saveWidget(hw,"1.html",selfcontained = F) 
webshot::webshot("1.html","1.png",vwidth = 1800, vheight = 900, delay =10)
hw

# 2. El País
hw2 = wordcloud2(bloques_count_pais[,2:3],color=rev(colores_rojos(nrow(bloques_count_pais))))
saveWidget(hw2,"1.html",selfcontained = F)
webshot::webshot("1.html","1.png",vwidth = 1800, vheight = 900, delay =10)
hw2

# 3. El Periódico
hw3 = wordcloud2(bloques_count_periodico[,2:3], color = rev(colores_naranjas(nrow(bloques_count_periodico))))
saveWidget(hw3,"1.html",selfcontained = F)
webshot::webshot("1.html","1.png",vwidth = 1800, vheight = 900, delay =10)
hw3

# 4. Ok Diario
hw4 = wordcloud2(bloques_count_okdiario[,2:3], color = rev(colores_verdes(nrow(bloques_count_okdiario))))
saveWidget(hw4,"1.html",selfcontained = F)
webshot::webshot("1.html","1.png",vwidth = 1800, vheight = 900, delay =10)
hw4

# Tablas palabras únicas
 # El Mundo
tabla_unicas_mundo %>%
  mutate_if(is.numeric, function(x) {
    cell_spec(x, bold = T, 
              color = spec_color(x, end = 0.5,option = "C"),
              font_size = spec_font_size(x))
  }) %>%
  mutate(Palabra = cell_spec(
    palabra , color = "white", bold = T,
    background = spec_color(1:nrow(tabla_unicas_mundo), end = 0.9, option = "A", direction = -1)
  )) %>%
  select(2:6) %>%
  kable(escape = F, align = "c",col.names = c("El Mundo", "El País", "El Periódico", "Ok Diario", "Palabras")) %>%
  kable_styling(c("striped", "condensed"), full_width = F)

  # El País
tabla_unicas_pais %>%
  mutate_if(is.numeric, function(x) {
    cell_spec(x, bold = T, 
              color = spec_color(x, end = 0.5,option = "C"),
              font_size = spec_font_size(x))
  }) %>%
  mutate(Palabra = cell_spec(
    palabra , color = "white", bold = T,
    background = spec_color(1:nrow(tabla_unicas_pais), end = 0.9, option = "A", direction = -1)
  )) %>%
  select(2:6) %>%
  kable(escape = F, align = "c",col.names = c("El Mundo", "El País", "El Periódico", "Ok Diario", "Palabras")) %>%
  kable_styling(c("striped", "condensed"), full_width = F)

 # El Periódico
tabla_unicas_periodico %>%
  mutate_if(is.numeric, function(x) {
    cell_spec(x, bold = T, 
              color = spec_color(x, end = 0.5,option = "C"),
              font_size = spec_font_size(x))
  }) %>%
  mutate(Palabra = cell_spec(
    palabra , color = "white", bold = T,
    background = spec_color(1:nrow(tabla_unicas_periodico), end = 0.9, option = "A", direction = -1)
  )) %>%
  select(2:6) %>%
  kable(escape = F, align = "c",col.names = c("El Mundo", "El País", "El Periódico", "Ok Diario", "Palabras")) %>%
  kable_styling(c("striped", "condensed"), full_width = F)

 # Ok Diario
tabla_unicas_okdiario %>%
  mutate_if(is.numeric, function(x) {
    cell_spec(x, bold = T, 
              color = spec_color(x, end = 0.5,option = "C"),
              font_size = spec_font_size(x))
  }) %>%
  mutate(Palabra = cell_spec(
    palabra , color = "white", bold = T,
    background = spec_color(1:nrow(tabla_unicas_okdiario), end = 0.9, option = "A", direction = -1)
  )) %>%
  select(2:6) %>%
  kable(escape = F, align = "c",col.names = c("El Mundo", "El País", "El Periódico", "Ok Diario", "Palabras")) %>%
  kable_styling(c("striped", "condensed"), full_width = F)



