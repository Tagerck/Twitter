# Instalar paquetes si no se pueden cargar
EnsurePackage <- function(x) {
  x <- as.character(x)
  if (!require(x, character.only = TRUE)) {
      require(x, character.only = TRUE)
    }
}

library(wordcloud)
PrepareTwitter <- function() {
  EnsurePackage("twitteR")
  EnsurePackage("stringr")
  EnsurePackage("ROAuth")
  EnsurePackage("RCurl")
  EnsurePackage("ggplot2")
  EnsurePackage("reshape")
  EnsurePackage("tm")
  EnsurePackage("RJSONIO")
  EnsurePackage("wordcloud")
  EnsurePackage("gridExtra")
  EnsurePackage("plyr")
}
PrepareTwitter()

#Autenticación a Twitter
source("credenciales.R")

#Se ejecuta el servidor
shinyServer(function(input, output) {
  library(sentiment)
  library(ggplot2)
  library(dplyr)
  library(purrr)
  library(twitteR)
  library(rsconnect)

  rsconnect::setAccountInfo(name = "twitter-sentiment0530",
                            token = "E461A103DE33BF8647B54E7779E82F05",
                            secret = "UM6OBhggO+zRCLOHkZOTtdUMBWGDq2dLXDGU+X5D")

  #Limpieza de los tweets
  TweetFrame <- function(twtList) {
    df <- do.call("rbind", lapply(twtList, as.data.frame))
    #Eliminar emoticonos
    df$text <- sapply(df$text, function(row) iconv(row, "latin1", "ASCII", sub = ""))
    df$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", df$text)
    cat("Tweets sacados\n")
    return(df$text)
  }

  TweetFrameLimpio <- function(twtList) {
    df <- do.call("rbind", lapply(twtList, as.data.frame))
    df$text <- sapply(df$text, function(row) iconv(row, "latin1", "ASCII", sub = ""))
    df$text = df$text
    #Eliminar signos de puntuacion y links
    df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", df$text)
    df$text = gsub("@\\w+", "", df$text)
    df$text = gsub("[[:punct:]]", "", df$text)
    df$text = gsub("[[:digit:]]", "", df$text)
    df$text = gsub("http\\w+", "", df$text)
    df$text = gsub("[ \t]{2,}", "", df$text)
    df$text = gsub("^\\s+|\\s+$", "", df$text)
    #Eliminar posibles errores al pasar a minúscula
    try.error = function(x) {
      y = NA
      try_error = tryCatch(tolower(x), error=function(e) e)
      if (!inherits(try_error, 'error'))
        y = tolower(x)
      return(y)
    }
    df$text = sapply(df$text, try.error)
    df$text = df$text[!is.na(df$text)]
    names(df$text) = NULL
    cat("Tweets sacados y limpios\n")
    return (df$text)
  }

  # Sacamos las palabras negativas y positivas de los diccionarios obtenidos
  pos.words = scan("positive-words.txt", what = "character", comment.char = ";")
  neg.words = scan("negative-words.txt", what = "character", comment.char = ";")

  #Se puntuan las frases por negativas o positivas usando los diccionarios
  score.sentiment <- function(sentences, pos.words, neg.words, .progress = "none") {
    require(plyr)
    require(stringr)
    list = lapply(sentences, function(sentence, pos.words, neg.words) {
      sentence = gsub("[[:punct:]]", " ", sentence)
      sentence = gsub("[[:cntrl:]]", "", sentence)
      sentence = gsub("\\d+", "", sentence)
      sentence = gsub("\n", "", sentence)
      sentence = tolower(sentence)
      word.list = str_split(sentence, "\\s+")
      words = unlist(word.list)
      pos.matches = match(words, pos.words)
      neg.matches = match(words, neg.words)
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)
      pp = sum(pos.matches)
      nn = sum(neg.matches)
      score = sum(pos.matches) - sum(neg.matches)
      list1 = c(score, pp, nn)
      return (list1)
    }, pos.words, neg.words)
    score_new = lapply(list, `[[`, 1)
    pp1 = score = lapply(list, `[[`, 2)
    nn1 = score = lapply(list, `[[`, 3)
    scores.df = data.frame(score = score_new, text = sentences)
    positive.df = data.frame(Positive = pp1, text = sentences)
    negative.df = data.frame(Negative = nn1, text = sentences)
    list_df = list(scores.df, positive.df, negative.df)
    return(list_df)
  }

  #Se agregan columnas necesarias para el analisis de sentimientos
  library(reshape)
  sentimentAnalyser <- function(result) {
    test1 = result[[1]]
    test2 = result[[2]]
    test3 = result[[3]]
    #Creamos 3 diferentes dataframes para la puntuación, positivos y negativos.
    test1$text = NULL
    test2$text = NULL
    test3$text = NULL
    #Almacenamos la primera columna (contiene la puntuación de los sentimientos) en la variable q
    q1 = test1[1,]
    q2 = test2[1,]
    q3 = test3[1,]
    qq1 = melt(q1, , var = 'Score')
    qq2 = melt(q2, , var = 'Positive')
    qq3 = melt(q3, , var = 'Negative') 
    qq1['Score'] = NULL
    qq2['Positive'] = NULL
    qq3['Negative'] = NULL
    #Se crea el dataframe con los sentimientos
    table1 = data.frame(Text = result[[1]]$text, Score = qq1)
    table2 = data.frame(Text = result[[2]]$text, Score = qq2)
    table3 = data.frame(Text = result[[3]]$text, Score = qq3)
    #Unimos los 3 dataframe en 1
    table_final = data.frame(Text = table1$Text, Positive = table2$value, Negative = table3$value, Score = table1$value)
    return(table_final)
  }

  #Se obtiene el porcentaje de positividad y negatividad
  percentage <- function(table_final) {
    #Porcentaje positivo
    posSc = table_final$Positive
    negSc = table_final$Negative
    table_final$PosPercent = posSc / (posSc + negSc)
    pp = table_final$PosPercent
    pp[is.nan(pp)] <- 0
    table_final$PosPercent = pp * 100

    #Porcentaje negativo
    table_final$NegPercent = negSc / (posSc + negSc)
    nn = table_final$NegPercent
    nn[is.nan(nn)] <- 0
    table_final$NegPercent = nn * 100
    return(table_final)
  }

  #Extraemos los tweets y los limpiamos
  twtList <- reactive({twtList <- searchTwitter(input$searchTerm, n = input$maxTweets, lang = "es") })
  tweets <- reactive({tweets <- TweetFrame(twtList())})
  tweets_limpios <- reactive({tweets_limpios <- TweetFrameLimpio(twtList())})

  #Se les asigna puntuación según los sentimientos y obtenemos los resultados
  result <- reactive({result <- score.sentiment(tweets(), pos.words, neg.words, .progress = 'none')})
  table_final <- reactive({table_final <- sentimentAnalyser(result())})
  table_final_percentage <- reactive({table_final_percentage <- percentage(table_final())})
  output$tabledata <- renderTable(table_final_percentage())

  #Se obtienen Trending Hashtags
  toptrends <- function(place) {
    cat("Segunda toptrending\n")
    a_trends = availableTrendLocations()
    woeid = a_trends[which(a_trends$name == place), 3]
    trend = getTrends(woeid)
    trends = trend[1:2]
    dat <- cbind(trends$name)
    dat2 <- unlist(strsplit(dat, split = ", "))
    dat3 <- grep("dat2", iconv(dat2, "latin1", "ASCII", sub = "dat2"))
    if(dat3 == 0)
      return(dat2)
    dat4 <- dat2[-dat3]
    return(dat4)
  }
  trend_table <- reactive({trend_table <- toptrends(input$trendingTable)})
  output$trendtable <- renderTable(trend_table())

  #Se crea la Wordcloud
  wordclouds <- function(texto_tweets) {
    cat("Comienzo wordcloud\n")
    clasificacion_emociones = classify_emotion(texto_tweets, algorithm = "bayes", prior = 1.0)
    emociones = clasificacion_emociones[, 7]
    # sustituimos valores "NA's" por "neutral"
    emociones[is.na(emociones)] = "neutral"
    clasificacion_popularidad = classify_polarity(texto_tweets, algorithm = "bayes")
    popularidad = clasificacion_popularidad[, 4]
    data = data.frame(text = texto_tweets, emotion = emociones, polarity = popularidad, stringsAsFactors = FALSE)
    data = within(data, emotion <- factor(emotion, levels = names(sort(table(emotion), decreasing = TRUE))))
    emociones_data = levels(factor(data$emotion))
    tama_emociones_data = length(emociones_data)
    documento_emociones = rep("", tama_emociones_data)
    for (i in 1:tama_emociones_data) {
      tmp = texto_tweets[emociones == emociones_data[i]]
      documento_emociones[i] = paste(tmp, collapse = " ")
    }
    #Se eliminan stopwords
    documento_emociones = removeWords(documento_emociones, stopwords("spanish"))
    #Se crea el corpus
    corpus = Corpus(VectorSource(documento_emociones))
    termdocumentmatrix = TermDocumentMatrix(corpus)
    termdocumentmatrix = as.matrix(termdocumentmatrix)
    colnames(termdocumentmatrix) = emociones_data
    #Se crea la wordcloud
    png("./images/nube.png", width = 568, height = 606)
    comparison.cloud(termdocumentmatrix, colors = brewer.pal(tama_emociones_data, "Set2"),
                      scale = c(4.5, 0.8), random.order = FALSE, title.size = 1.5, max.words = 150)
    dev.off()
    cat("Acabo wordcloud\n")
  }
  sacar_nube <- reactive({sacar_nube <- wordclouds(tweets_limpios())})
  output$word <- renderImage({
    wordclouds(tweets_limpios())
    filename <- normalizePath(file.path("./images",
                                        paste("nube", ".png", sep="")))
    list(src = filename)
  }, deleteFile = FALSE)

  #Se grafican los de histogramas
  output$histPos <- renderPlot({hist(table_final()$Positive, col = rainbow(10), main = "Histograma del sentimiento positivo", xlab = "Puntuación Positiva", ylab = "Frencuencias") })
  output$histNeg <- renderPlot({hist(table_final()$Negative, col = rainbow(10), main = "Histograma del sentimiento negativo", xlab = "Puntuación Negativa", ylab = "Frencuencias") })
  output$histScore <- renderPlot({hist(table_final()$Score, col = rainbow(10), main = "Histograma de Puntuación", xlab = "Puntuación total")})

  #Analisis con clasificador Naive Bayes
  AnalisisSentimientos <- function(texto_tweets, i){
    cat("Comienzo análisis emociones o popularidad\n")
    clasificacion_emociones = classify_emotion(texto_tweets, algorithm = "bayes", prior=1.0)
    emociones = clasificacion_emociones[, 7]
    # Se sustituyen "NA's" por "neutral"
    emociones[is.na(emociones)] = "neutral"
    clasificacion_popularidad = classify_polarity(texto_tweets, algorithm = "bayes")
    popularidad = clasificacion_popularidad[, 4]
    data = data.frame(text = texto_tweets, emotion = emociones, polarity = popularidad, stringsAsFactors = FALSE)
    data = within(data, emotion <- factor(emotion, levels = names(sort(table(emotion), decreasing = TRUE))))
    if (i == 0) {
      ggplot(data, aes(x = emotion)) +
        geom_bar(aes(y = ..count.., fill = emotion)) +
        scale_fill_brewer(palette = "Set2") +
        labs(x = "Categoria de emociones", y = "Número de Tweets") +
        theme(plot.title = element_text(size = 12, face = "bold"))
      ggsave("./images/emociones.png", dpi = 80)
    }
    else {
      ggplot(data, aes(x = polarity)) +
        geom_bar(aes(y = ..count.., fill = polarity)) +
        scale_fill_brewer(palette = "Set1") +
        labs(x = "Categorias de polaridad", y = "Número de Tweets") +
        theme(plot.title = element_text(size = 12, face = "bold"))
      ggsave("./images/popularidad.png", dpi = 80)
    }
    cat("Acabo análisis emociones o polaridad\n")
  }
  sacar_sentimientos <- reactive({sacar_sentimientos <- AnalisisSentimientos(tweets_limpios())})
  output$emociones <- renderImage({
    AnalisisSentimientos(tweets_limpios() , 0)
    filename <- normalizePath(file.path("./images",
                                        paste("emociones", ".png", sep = "")))
    list(src = filename)
  }, deleteFile = FALSE)
  output$popularidad <- renderImage({
    AnalisisSentimientos(tweets_limpios(), 1)
    filename <- normalizePath(file.path("./images",
                                        paste("popularidad", ".png", sep = "")))
    list(src = filename)
  }, deleteFile = FALSE)

  #Creacion de grafica de pastel
  slices <- reactive({c(sum(table_final()$Positive), sum(table_final()$Negative))})
  labels <- c("Positivo", "Negativo")
  library(plotrix)
  output$piechart <- renderPlot({pie3D(slices(), labels = labels, col = c("#132edb", "#db0f12"), explode = 0.00, main = "Análisis de Sentimientos")})

  #Obtencion de las cuentas mas activas
  toptweeters <- function(tweetlist) {
    tweets <- twListToDF(tweetlist)
    tweets <- unique(tweets)
    #Tabla con el número de tweets por usuario
    d <- as.data.frame(table(tweets$screenName))
    d <- d[order(d$Freq, decreasing = T), ]
    names(d) <- c("Usuario", "Tweets")
    return (d)
  }

  # Creacion de la grafica de las cuentas mas activas
  d <- reactive({d <- toptweeters(twtList())})
  output$tweetersplot <- renderPlot(barplot(head(d()$Tweets, 20), names.arg = head(d()$Usuario, 20), cex.names = 0.7, las = 2, horiz = F, main="Top 20: Tweets por Usuario", col=1))
  output$tweeterstable <- renderTable(head(d(), 20))
}) #shiny server