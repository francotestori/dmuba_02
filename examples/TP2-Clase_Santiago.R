library(mongolite)
library(ggplot2)
library(tidyr)
library(dplyr)
library(arules)
library(arulesViz)


# Nombres de las colecciones que definimos en mongo
c1 = "usuarios_nhashtgs_dia"
c2 = "usuarios_hashtags"

# Conecciones a mongo
usuarios_nhashtgs_dia <- mongo(collection = c1, db = "DMUBA")
usuarios_hashtags <- mongo(collection = c2, db = "DMUBA")

# Lectura de las colecciones
df_c1 = usuarios_nhashtgs_dia$find(
  query = '{}',
  fields = '{ "_id": false, "user_id" : true, "dia_semana" : true, "n_hashtags": true, "friends_count": true, "followers_count": true, "retweet_favorite_count": true, "retweet_retweet_count": true}'
  )

View(
  head(df_c1)
)

df_c2 = usuarios_hashtags$find(
  query = '{}', 
  fields = '{"_id": false, "user_id" : true, "cat_hashtags" : true}'
)

View(
  head(df_c2)
)

# Elimino los términos de búsqueda de los hashtags
df_c2 = df_c2[-grep(x =  df_c2$cat_hashtags, pattern = "^(covid|coronav|cuarentena)"),]

# Creación de la variable categorica día
dias = c( "lunes",  "martes",  "mi?rcoles",  "jueves",  "viernes",  "s?bado",  "domingo")
df_c1$cat_nombre_dia = apply(df_c1[c(4)], 1, function(x){return(dias[x[1]])})
table(df_c1$cat_nombre_dia)

barplot(
  table(df_c1$n_hashtags),
  main = "Uso de hashtags", 
  xlab = "# hashtags", 
  ylab="Cantidad"
)

# ---------- Discretizaciones  --------------------------
# 1) friends_count
hist(df_c1$friends_count)
hist(
  log10(df_c1$friends_count)
)
df_c1$cat_friends = discretize(
  log10(df_c1$friends_count),
  method = "fixed", 
  breaks = c(-Inf, 1, 3, 4, 5, Inf), 
  labels=c("muy pocos","pocos", "medio", "muchos", "muchisimos")
)
table(df_c1$cat_friends)

# 2) followers_count
hist(
  log10(df_c1$followers_count)
)
df_c1$cat_followers = discretize(
  log10(df_c1$followers_count),
  method = "fixed", 
  breaks = c(-Inf, 1, 3, 4, 5, Inf), 
  labels=c("muy pocos","pocos", "medio", "muchos", "muchisimos")
)
table(df_c1$cat_followers)

# 3) retweet_favorite_count
hist(df_c1$retweet_favorite_count)
hist(log10(df_c1$retweet_favorite_count))
df_c1$cat_retweet_favorite_count = discretize(log10(df_c1$retweet_favorite_count),method = "fixed", breaks = c(-Inf, 1.5, 3, Inf), labels=c("bajo", "medio", "alto"))
table(df_c1$cat_retweet_favorite_count)

# 4) retweet_retweet_count
hist(
  log10(df_c1$retweet_retweet_count)
)
df_c1$cat_retweet_retweet_count = discretize(
  log10(df_c1$retweet_retweet_count),
  method = "fixed", 
  breaks = c(-Inf, 1.5, 3, Inf), 
  labels=c("bajo", "medio", "alto")
)
table(df_c1$cat_retweet_retweet_count)

View(head(df_c1))

## Rotacion de Matrices
# Rotación de las matriz de datos de usuario y las categóricas
df_tuples_c1 = df_c1 %>% 
  pivot_longer(
    cols =starts_with("cat"),
    names_to = "feat", 
    values_to = "val", 
    names_prefix = "cat_",
    values_drop_na = TRUE) %>% 
  select("user_id", "feat", "val")

head(df_tuples_c1)  

# Rotación de las matriz de datos de usuario y los hashtags
df_tuples_c2 = df_c2 %>% 
  pivot_longer(
    cols =starts_with("cat"),
    names_to = "feat", 
    values_to = "val", 
    names_prefix = "cat_",
    values_drop_na = TRUE) %>% 
  select("user_id", "feat", "val")

head(df_tuples_c2)  

# Concateno los dos data.frames
df_tuples = rbind(df_tuples_c1,df_tuples_c2)

# Se generan las tuplas TID-ITEM (el TID es el user_id)
df_tuples = df_tuples %>% 
  mutate("item" = paste0(feat,"=",val)) %>% 
  select("user_id", "item")

# Cantidad de transacciones (son los user_id únicos)
length(
  unique(df_tuples$user_id)
)

# Generamos las transacciones
trans <- as(split(df_tuples$item, df_tuples$user_id), "transactions")

inspect(trans[101])

# Buscamos reglas con min_sup=0.005 y min_conf=0.5
# Adem?s, se limitan la cantidad de ?tems (orden) entre 2 y 9
rules = apriori(
  trans,
  parameter=list(
    target="rule", 
    support=0.005, 
    confidence=0.5, 
    maxlen=9, 
    minlen=2
  )
)
print(rules)

inspect(
  sort(rules, by="lift", decreasing = TRUE)[1:20]
)

# Scatter plot de support vs lift
plot(rules, measure = c("support", "lift"), shading = "confidence")

# Two-Key Plots: Grafica el Orden y las m?tricas
plot(rules, method = "two-key plot")


#   Filtros de reglas
# ---------------------
# Utilizo el comando subset y su parámetro (también se llama subset :s )
# Ejemplo: 
#         7 < lift < 10 y  0.04 < support < 0.1
#   quedar?a: (lift < 10 & lift > 7) & (support > 0.04 & support < 0.1)
rules.filter = arules::subset(
  rules, 
  subset = (lift < 10 & lift > 7) & (support > 0.04 & support < 0.1)
)
print(rules.filter)

inspect(
  head(rules.filter, 20)
)

# Scatter plot de support vs lift
plot(rules.filter, measure = c("support", "lift"), shading = "confidence")

# Two-Key Plots: Grafica el Orden y las m?tricas
plot(rules.filter, method = "two-key plot")

# Referencias:
# https://cran.r-project.org/web/packages/arulesViz/vignettes/arulesViz.pdf
# 

