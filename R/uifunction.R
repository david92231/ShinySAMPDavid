#' Ui for shiny
#'
#' @param n
#' @param itermax
#' @param x1mu
#' @param x2mu
#' @param varx1
#' @param varx2
#' @param covx1x2
#'
#' @return input and output for server
#' @export
#'
#' @examples ui = uifunction(300,7000,0,0,200,500,50)
uifunction = function(n,itermax,x1mu,x2mu,varx1,varx2,covx1x2){
  library(shinydashboard)
  library(shiny)
  dashboardPage(
    dashboardHeader(title = paste("Simulation of Xbar and ", expression(theta) ), titleWidth = 500),
    dashboardSidebar(disable=TRUE),
    dashboardBody(
      # Boxes need to be put in a row (or column)

      fluidRow(
        box(plotOutput("plot1", height = 400)),
        box(plotOutput("plot2", height = 400)),
        box(plotOutput("plot3", height = 400)),
        box(plotOutput("plot4", height = 400)),


        box(
          title = "Sample size and iteration",
          sliderInput("slider1", "n:", 1,n, 50), # Sample size input
          sliderInput("slider2", "iterations:", 1, itermax, 1000) # Number of iteration
        ),
        box(
          title = "X mu",
          textInput("text1", "x1 mu:",x1mu), # x1 mu
          textInput("text2", "x2 mu:",x2mu) # x2 mu
        ),
        box(
          title = "Variance",
          textInput("text3", "Var x1:",varx1), # x1 var
          textInput("text4", "Var x2:",varx2), # x2 var
          textInput("text5", "covariance x1 x2",covx1x2) #S x1x2 covariance
        )
      )
    )
  )
}
