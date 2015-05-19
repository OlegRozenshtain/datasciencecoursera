# Access the API to get information on your instructors repositories (hint: this
# is the url you want "https://api.github.com/users/jtleek/repos"). Use this
# data to find the time that the datasharing repo was created.
ex1<-function(clientID, clientSecret)
{
    library(httr)
    library(httpuv)
    myapp <- oauth_app("github", key = clientID, secret = clientSecret)
    github_token <- oauth2.0_token(oauth_endpoints("github"), myapp,
                                   cache = FALSE)
    gtoken <- config(token = github_token)
    
    
    getURL <- GET("https://api.github.com/users/jtleek/repos", gtoken)
    
    contentURL<-content(getURL, as = "text")
    
    library(jsonlite)
    jsoned <- fromJSON(contentURL)
    
    subset(jsoned, name == "datasharing", select = c(name, created_at))
}