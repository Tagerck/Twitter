#Cargamos las librerías
library("ROAuth")
library("base64enc");
library("twitteR");
library("streamR");

#Cargar parámetros de configuración
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
options(httr_oauth_cache = T)

#Cargar las credenciales
consumer_key <- "St9TqYJ4KiRYRHs7it3HKoTWo"
consumer_secret <- "09CV7bsa3i8BoYxGxfIYm2FEPiF0ZfIuysd3unBwo0p90BRp0s"
access_token <- "1452150524-1GZyDvPmmIXUiZEWJZ0BnBXYZK0Gia5H8z7oJDF"
access_secret <- "4106Fotob6yZ7lzWgPHJOMSP2Fbb3E3Hse6ccAaJQHkzE"

#Ejecutar la autenticación de TwitteR
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#streamR authentication
credentials_file <- "my_oauth.Rdata"
if (file.exists(credentials_file)){
  load(credentials_file)
} else {
  cred <- OAuthFactory$new(consumerKey = consumer_key, consumerSecret =
                            consumer_secret, requestURL = reqURL, accessURL = accessURL, authURL = authURL)
  cred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  save(cred, file = credentials_file)
}