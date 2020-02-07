
load("proj_data.rda")
load("Project_app.rda")
load("project_rtweets.Rda")
load("project_rtweets2.Rda")
load("projet_timeline.Rda")
load("projet_timeline2.Rda")
load("Project_app2.rda")
load("proj_data2.rda")
load("sentimentwhole.rda")
load("bigramcount.rda")
load("LDA_top_terms.rda")
load("hashtags_unnested_count.rda")
load("word_count_wordcloud.rda")
load("chase_nov19_jan20.Rda")
load("project_tweets_organic.rda")
load("summarySentiment2.rda")
load("summarySentiment.rda")




library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$geomap <- renderPlot({

        # generate bins based on input$bins from ui.R
        rt <- lat_lng(chase_nov19_jan20)
        
        par(mar = c(0, 0, 0, 0))
        maps::map("world", lwd = .25)
        points(rt$lng, rt$lat, pch = 20, cex = 1,col="red")
        
    })
    
    
    output$Tweets_Distribution <- renderPlot({
        
        ts_plot(chase_nov19_jan20, by = "60 secs")
       
    })
    
    
    output$wordcloudChase <- renderPlot({
        
        layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
        par(mar=rep(0, 4))
        plot.new()
        text(x=0.5, y=0.2, "Most frequently used hashtags @Chase")
        wordcloud(project_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
                  colors=brewer.pal(8, "Dark2"), main = "Title")
        
    })
    
    output$wordcloudChaseSupport <- renderPlot({
        
        layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
        par(mar=rep(0, 4))
        plot.new()
        text(x=0.5, y=0.2, "Most frequently used hashtags @ChaseSupport")
        wordcloud(project_tweets_organic2$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
                  colors=brewer.pal(8, "Dark2"))
        
        
    })
    
    
    output$wholesentiment <- renderPlot({
        
        summary_sentiment %>%
            ungroup() %>%
            mutate(word = reorder(word, n)) %>%
            ggplot(aes(word, n, fill = sentiment)) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~sentiment, scales = "free_y") +
            labs(y = "Contribution to sentiment",
                 x = NULL) +
            coord_flip()
        
        
    })
    
    
    output$TopicModelling <- renderPlot({
        
        top_tweet_terms %>%
            mutate(term = reorder_within(term, beta, topic)) %>%
            ggplot(aes(term, beta, fill = factor(topic))) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~ topic, scales = "free") +
            coord_flip() +
            scale_x_reordered()
        
    })
    
    output$wordcloudChasesupport <- renderPlot({
        
        wordcloud(
            words = word.count$word, 
            freq = word.count$n, 
            min.freq = 100,
            colors = brewer.pal(8, 'Dark2')
        )
        
        
    })
    
    output$wordcloudhashtag <- renderPlot({
        
        wordcloud(
            words = str_c('#',hashtags.unnested.count$hashtag), 
            freq = hashtags.unnested.count$n, 
            min.freq = 5, 
            colors=brewer.pal(8, 'Dark2')
        )
        
    })
    output$Network_Analysis <- renderPlot({
        
        bi.gram.count %>% 
            ggplot(mapping = aes(x = weight)) +
            theme_light() +
            geom_histogram() +
            labs(title = "Bigram Weight Distribution")
        ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
        
        
        #Note that curve is very skewed, for visualization purposes it might be a good idea to perform a transformation, e.g. log transform:
        
        bi.gram.count %>% 
            mutate(weight = log(weight + 1)) %>% 
            ggplot(mapping = aes(x = weight)) +
            theme_light() +
            geom_histogram( binwidth = NULL) +
            labs(title = "Bigram log-Weight Distribution")
        ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
        
        
        #For visualization purposes, we can set a threshold which defines the minimal weight allowed in the graph.
        #Remark: It is necessary to set the weight column name as weight (see igraph docs).
        
        threshold <- 30
        
        # For visualization purposes we scale by a global factor. 
        ScaleWeight <- function(x, lambda) {
            x / lambda
        }
        
        network <-  bi.gram.count %>%
            filter(weight > threshold) %>%
            mutate(weight = ScaleWeight(x = weight, lambda = 2E3)) %>% 
            graph_from_data_frame(directed = FALSE)
        
        # verify we have a weighted network:
        is.weighted(network) 
        
        
        #Visualization :To visualize the network (here is a great reference for it) we can simply use the plot function with some additional parameters:
        
        plot(
            network, 
            vertex.size = 1,
            vertex.label.color = 'black', 
            vertex.label.cex = 0.7, 
            vertex.label.dist = 1,
            edge.color = 'gray', 
            main = 'Bigram Count Network', 
            sub = glue('Weight Threshold: {threshold}'), 
            alpha = 50
        )
        
        
    
    })
    
    output$Sentiments_Chase_Support <- renderPlot({
        summarySentiment %>%
            ungroup() %>%
            mutate(word = reorder(word, n)) %>%
            ggplot(aes(word, n, fill = sentiment)) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~sentiment, scales = "free_y") +
            labs(y = "Contribution to sentiment",
                 x = NULL) +
            coord_flip()
        
        
    })
    
    output$Sentiments_Chase <- renderPlot({
        summarySentiment2 %>%
            ungroup() %>%
            mutate(word = reorder(word, n)) %>%
            ggplot(aes(word, n, fill = sentiment)) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~sentiment, scales = "free_y") +
            labs(y = "Contribution to sentiment",
                 x = NULL) +
            coord_flip()
        
        
    })
    
    
    output$Hashtags_Chase_Support <- renderPlot({
        wordcloud(
            words = str_c('#',hashtags.unnested.count$hashtag), 
            freq = hashtags.unnested.count$n, 
            min.freq = 5, 
            colors=brewer.pal(8, 'Dark2')
        )
        
    })
    
    output$Hashtags_Chase <- renderPlot({
        project_tweets_organic$hashtags <- as.character(project_tweets_organic$hashtags)
        project_tweets_organic$hashtags <- gsub("c\\(", "", project_tweets_organic$hashtags)
        set.seed(1234)
        wordcloud(project_tweets_organic$hashtags, min.freq=5, scale=c(3.5, .5), random.order=FALSE, rot.per=0.35, 
                  colors=brewer.pal(8, "Dark2"))
    })
    output$typesoftweets_Chase <- renderPlot({
        Type_of_Tweet_Chase <- paste(proj_data$category, proj_data$percentage, "%")
        ggplot(proj_data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type_of_Tweet_Chase)) +
            geom_rect() +
            coord_polar(theta="y") + 
            xlim(c(2, 4)) +
            theme_void() +
            theme(legend.position = "right")
        
    })
    output$Source_Chase <- renderPlot({
        data2 <- data.frame(
            category=Project_app$source,
            count=Project_app$count
        )
        data2$fraction = data2$count / sum(data2$count)
        data2$percentage = data2$count / sum(data2$count) * 100
        data2$ymax = cumsum(data2$fraction)
        data2$ymin = c(0, head(data2$ymax, n=-1))
        
        Source <- paste(data2$category, data2$percentage, "%")
        ggplot(data2, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Source)) +
            geom_rect() +
            coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
            xlim(c(2, 4)) +
            theme_void() +
            theme(legend.position = "right")
    })
    
    
    output$typesoftweets_Chase_Support <- renderPlot({
        Type_of_Tweet_Chase_Support <- paste(proj_data2$category, proj_data2$percentage, "%")
        ggplot(proj_data2, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type_of_Tweet_Chase_Support)) +
            geom_rect() +
            coord_polar(theta="y") + 
            xlim(c(2, 4)) +
            theme_void() +
            theme(legend.position = "right")
    })
    
    
    output$Source_Chase_Support <- renderPlot({
        data2_1 <- data.frame(
            category=Project_app2$source,
            count=Project_app2$count
        )
        data2_1$fraction = data2_1$count / sum(data2_1$count)
        data2_1$percentage = data2_1$count / sum(data2_1$count) * 100
        data2_1$ymax = cumsum(data2_1$fraction)
        data2_1$ymin = c(0, head(data2_1$ymax, n=-1))
        
        Source <- paste(data2_1$category, data2_1$percentage, "%")
        ggplot(data2_1, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Source)) +
            geom_rect() +
            coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
            xlim(c(2, 4)) +
            theme_void() +
            theme(legend.position = "right")
        
    })    

})
