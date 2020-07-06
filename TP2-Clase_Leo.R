library(mongolite)
library(ggplot2)
library(tidyr)
library(dplyr)
library(tm, exclude = "inspect")
library(stringr)
library(stringi)
library(arules)
library(tokenizers)


users <- mongo(collection = "backup_usuarios", db = "DMUBA")
tweets <- mongo(collection = "tweets_mongo_covid19", db = "DMUBA")

# ----------- Fromato clásico -----------------------------------------
# Se discretizan variables numéricas
df_user_features = users$find(query = '{}', 
                              fields = '{ "_id": false, "user_id" : true, "friends_count" : true, "favourites_count":true, "verified": true }')

df_user_features$cat_friends =discretize(df_user_features$friends_count, labels=c("pocos", "medio", "muchos"))
df_user_features$cat_favourites =discretize(df_user_features$favourites_count, labels=c("pocos", "medio", "muchos"))
df_user_features$cat_verified =  as.factor(ifelse(df_user_features$verified, "si", NA))

hist(log10(df_user_features$friends_count+1))
View(df_user_features[df_user_features$cat_friends=="pocos",])

# generación de reglas
trans <- as(df_user_features[c("cat_friends", "cat_favourites","cat_verified")], "transactions")
rules = apriori(trans, parameter=list(target="rule", support=0.0001, confidence=0.02))
print(rules)
inspect(sort(rules, by="lift", decreasing = TRUE))


# ------ Formato de tuplas user-item -----------------------------

# Pivot de columnas que empiezan con "cat"
df_user_tuples = df_user_features %>% 
  pivot_longer(
    cols =starts_with("cat"),
    names_to = "feat", 
    values_to = "val", 
    names_prefix = "cat_",
    values_drop_na = TRUE) %>% 
  select("user_id", "feat", "val")

# se agrega prefijo de tipo de ítem:
df_user_tuples = df_user_tuples %>% 
  mutate("item" = paste0(feat,"=",val)) %>% 
  select("user_id", "item")

length(unique(df_user_tuples$user_id))

# ------------------ Hashtags --------------------------
# consulta
df_ht = tweets$find(query = '{"hashtags": {"$ne": null}}', 
                    fields = '{"user_id" : true, "hashtags" : true, "_id": false}')

length(unique(df_ht$user_id))

# formato tuplas: explota columna de hastagh
df_ht = df_ht %>% unnest(hashtags)

# limpieza
# Se quitan acentos
df_ht$hashtags = stri_trans_general(df_ht$hashtags, "Latin-ASCII")
# Se pasa a minúsculas
df_ht$hashtags = tolower(df_ht$hashtags)
# Se quita todo lo que no es alfanumérico
df_ht$hashtags= gsub("[^a-zA-z0-9]", "", df_ht$hashtags)
# se agrega prefijo de tipo de ítem hashtag:
df_ht$item = paste0("hashtag=", df_ht$hashtags)
df_ht = df_ht[c("user_id", "item")]

# A las transacciones con hashtags se les agregan los atributos del usuario.
df_tuples = rbind(df_user_tuples, df_ht)

# reglas de asociación
trans <- as(split(df_tuples$item, df_tuples$user_id), "transactions")
inspect(trans[100])
rules = apriori(trans, parameter=list(target="rule", support=0.001, confidence=0.5))
print(rules)
inspect(sort(rules, by="lift", decreasing = TRUE)[1:20])
inspect(head(rules, 20))


### ----- Tratamiento de Textos ------------
df_text = tweets$find(query = '{}',  fields = '{"user_id" : true, "text" : true, "_id": false}')

# Se quitan caracteres no alfanuméricos (por cuestiones de errores en RStudio)
df_text$text <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", df_text$text)
df_text$text <- gsub("U00..", "", df_text$text)

# --- limpieza de textos
# Se quitan acentos
df_text$text = stri_trans_general(df_text$text, "Latin-ASCII")
# Se pasa a minusculas
df_text$text = tolower(df_text$text)
# Se quita puntuacion
df_text$text = removePunctuation(df_text$text)
# Se quitan números
df_text$text = removeNumbers(df_text$text)
# se quitan espacios extras
df_text$text =  stripWhitespace(df_text$text)
# se quitan espacios al principio y final de la cadena
df_text$text = str_trim(df_text$text)
# sin stop words
df_text$text = removeWords(df_text$text, stopwords("spanish"))

# se separa el texto en términos
df_text$words = tokenizers::tokenize_words(df_text$text, simplify = T)
# se pasa a formato pares: user-término
df_text = df_text %>% select("user_id", "words")  %>% unnest(words) %>%  distinct()
# se agrega prefijo de tipo de ítem:
df_text$item = paste0("word=", df_text$words)

# reglas
trans <- as(split(df_text$item, df_text$user_id), "transactions")
print(trans)
rules = apriori(trans, parameter=list(target="rule", support=0.0005, confidence=0.02))
print(rules)
View(inspect(sort(rules, by="lift", decreasing = TRUE)[1:100]))
inspect(head(rules, 20))
