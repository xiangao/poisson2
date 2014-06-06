library(shiny)
library(ggplot2)
library(reshape)

results <- read.csv('results.csv')

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {
output$plot1 <-  renderPlot({
    nobs = ceiling(exp(input$n)/100)*100
    th=input$th
    match <- results[(nobs==results$nobs & th - results$th<1e-5 & !is.na(results$zip2)),c('nobs','nsim','zip','poisson','log.linear','zip2')]
    match.melt <- melt(match, id.vars=c('nobs','nsim'))
    names(match.melt) <- c('nobs','nsim','type','bias')
    ave.bias <- ddply(match.melt, c('type'),
                       function(x) data.frame(
                         mean.bias=mean(x$bias, na.rm=TRUE)
                         ))
    mse <- ddply(match.melt, c('type'),
                       function(x) data.frame(
                         mean.abs =mean(abs(x$bias), na.rm=TRUE)
                         ))
    mse2 <- ddply(match.melt, c('type'),
                  function(x) data.frame(
                      mse =mean((x$bias)^2, na.rm=TRUE)
                      ))

    p <- ggplot(match.melt,aes(x=bias, fill=type)) + geom_histogram(binwidth=.05)  + labs(title="bias comparison") +  geom_vline(data=ave.bias, aes(xintercept=mean.bias,  colour=type),
               linetype="dashed", size=1) + geom_vline(data=mse2, aes(xintercept=mse,  colour=type),   linetype="dashed", size=2) + facet_grid(type ~ .) + xlim(-1, 1)
    print(p)
#    plot(lm ~ heckit, data=match)
})

})
