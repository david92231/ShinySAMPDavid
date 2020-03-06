#' Server
#'
#' @param input
#' @param output
#'
#' @return Plots constructed from the constructor function of xbarthetadist
#' @export
#'
#' @examples
server <- function(input, output) {
  mu1=c(0,0)
  sigma1 = matrix(c(100,40,40,400), nr=2,nc=2,byrow=FALSE)

  omat<-xbarthetadist(n=30,iter=1000,mu=mu1,sigma=sigma1)$mat
  dmat = as.data.frame(omat)

  output$plot1 <- renderPlot({
    mu1=c(as.numeric(input$text1),as.numeric(input$text2))
    sigma1 = matrix(c(as.numeric(input$text3),as.numeric(input$text5),as.numeric(input$text5),as.numeric(input$text4)), nr=2,nc=2,byrow=FALSE)

    omat<-xbarthetadist(n=as.numeric(input$slider1),iter=as.numeric(input$slider2),mu=mu1,sigma=sigma1)$mat
    dmat = as.data.frame(omat)

    g=ggplot(dmat, aes(x=xbar1,y=xbar2))  + coord_equal() + geom_point()
    print(g)
  })
  output$plot2 <- renderPlot({
    mu1=c(as.numeric(input$text1),as.numeric(input$text2))
    sigma1 = matrix(c(as.numeric(input$text3),as.numeric(input$text5),as.numeric(input$text5),as.numeric(input$text4)), nr=2,nc=2,byrow=FALSE)

    omat<-xbarthetadist(n=as.numeric(input$slider1),iter=as.numeric(input$slider2),mu=mu1,sigma=sigma1)$mat
    dmat = as.data.frame(omat)

    g=ggplot(dmat, aes(x=xbar1,y=xbar2))  + coord_equal() + stat_density_2d()
    print(g)
  })
  output$plot3 <- renderPlot({
    mu1=c(as.numeric(input$text1),as.numeric(input$text2))
    sigma1 = matrix(c(as.numeric(input$text3),as.numeric(input$text5),as.numeric(input$text5),as.numeric(input$text4)), nr=2,nc=2,byrow=FALSE)

    omat<-xbarthetadist(n=as.numeric(input$slider1),iter=as.numeric(input$slider2),mu=mu1,sigma=sigma1)$mat
    dmat = as.data.frame(omat)

    ar = ggplot(dmat, aes(x=thetar))  + geom_histogram()
    print(ar)
  })
  output$plot4 <- renderPlot({
    mu1=c(as.numeric(input$text1),as.numeric(input$text2))
    sigma1 = matrix(c(as.numeric(input$text3),as.numeric(input$text5),as.numeric(input$text5),as.numeric(input$text4)), nr=2,nc=2,byrow=FALSE)

    omat<-xbarthetadist(n=as.numeric(input$slider1),iter=as.numeric(input$slider2),mu=mu1,sigma=sigma1)$mat
    dmat = as.data.frame(omat)

    ar = ggplot(dmat, aes(x=thetar))  + geom_density()
    print(ar)
  })
}
