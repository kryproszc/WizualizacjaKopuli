#Pakiety
if(require(VineCopula)){
  detach("package:VineCopula", unload = TRUE)
}
library(tinytex)
library(shiny)
library(VC2copula)
library(copula)
ui1<-shinyUI(pageWithSidebar(
    headerPanel("Symulacje kopuli"),
  sidebarPanel(
    radioButtons("dist",
                 label = "Wybierz kopule",
                 list(
                      
                      "Kopula Gaussa" = '1',
                      "Kopula tStudenta" = '2',
                      "Kopula Claytona" = '3',
                      "Kopula Gumbela" = "4",
                      "Kopula Franka" = '5',
                      "Kopula Joe" = "6",
                      "Kopula BB1" = '7',
                      "Kopula BB6" = '8',
                      "Kopula BB7" = '9',
                      "Kopula rBB7" = '0',
                      "Kopula BB8" = '10'
                     
                 ),
                 width = "1000px"),
    conditionalPanel(
      condition = "input.dist == '1'",
      sliderInput('n1',
                  label = withMathJax(helpText("Wartość parametru: $$\\theta$$")),
                  min = -1, max = 1,step = 0.01, value = 0),
    ),
    conditionalPanel(
      condition = "input.dist == '2'",
      sliderInput("n2",
                  label = withMathJax(helpText("Wartość parametru: $$\\theta$$")),
                  min = -1, max = 1,step = 0.01, value = 0),
    ),
    conditionalPanel(
      condition = "input.dist == '3'",
      sliderInput('n3',
                  label = withMathJax(helpText("Wartość parametru: $$\\theta$$")),
                  min = 0, max = 50,step = 0.1, value = 0.1),
    ),
    conditionalPanel(
      condition = "input.dist == '4'",
      sliderInput("n4",
                  label = withMathJax(helpText("Wartość parametru: $$\\theta$$")),
                  min = 1, max = 50,step = 0.1, value = 1.1),
    ),
    conditionalPanel(
      condition = "input.dist == '5'",
      sliderInput("n5",
                  label = withMathJax(helpText("Wartość parametru: $$\\theta$$")),
                  min = -50, max = 50,step = 0.1, value = 0.1),
    ),
    conditionalPanel(
      condition = "input.dist == '6'",
      sliderInput("n6",
                  label = withMathJax(helpText("Wartość parametru: $$\\theta$$")),
                  min = 1, max = 50,step = 0.1, value = 1.1),
    ),
    conditionalPanel(
      condition = "input.dist == '7'",
      sliderInput("n7",
                  label = withMathJax(helpText("Wartość parametru: $$\\theta$$")),
                  min = 0.1, max = 7,step = 0.1, value = 0.1),
    ),
    conditionalPanel(
      condition = "input.dist == '7'",
      sliderInput("m7",
                  label = withMathJax(helpText("Wartość parametru: $$\\delta$$")),
                  min = 1, max = 7,step = 0.1, value = 1.1),
    ),
    conditionalPanel(
      condition = "input.dist == '8'",
      sliderInput("n8",
                  label = withMathJax(helpText("Wartość parametru: $$\\theta$$")),
                  min = 1, max = 6,step = 0.1, value = 1.1),
    ),
    conditionalPanel(
      condition = "input.dist == '8'",
      sliderInput("m8",
                  label = withMathJax(helpText("Wartość parametru: $$\\delta$$")),
                  min = 1, max = 7,step = 0.1, value = 1.1),
    ),
    conditionalPanel(
      condition = "input.dist == '9'",
      sliderInput("n9",
                  label = withMathJax(helpText("Wartość parametru: $$\\theta$$")),
                  min = 1, max = 6,step = 0.1, value = 1.1),
    ),
    conditionalPanel(
      condition = "input.dist == '9'",
      sliderInput("m9",
                  label = withMathJax(helpText("Wartość parametru: $$\\delta$$")),
                  min = 0, max = 50,step = 0.1, value = .1),
    ),
    conditionalPanel(
      condition = "input.dist == '10'",
      sliderInput("n10",
                  label = withMathJax(helpText("Wartość parametru: $$\\theta$$")),
                  min = 1, max = 8,step = 0.1, value = 1.1),
    ),
    conditionalPanel(
      condition = "input.dist == '10'",
      sliderInput("m10",
                  label = withMathJax(helpText("Wartość parametru: $$\\delta$$")),
                  min = 0, max = 1,step = 0.1, value = .1),
    ),
    conditionalPanel(
      condition = "input.dist == '11'",
      sliderInput("n11",
                  label = withMathJax(helpText("Wartość parametru: $$\\delta$$")),
                  min = 0, max = 360,step = 90, value = 0),
      
    ),
    conditionalPanel(
      condition = "input.dist == '0'",
      sliderInput("n0",
                  label = withMathJax(helpText("Wartość parametru: $$\\theta$$")),
                  min = -6, max = -1,step = 0.1, value = -1.1),
    ),
    conditionalPanel(
      condition = "input.dist == '0'",
      sliderInput("m0",
                  label = withMathJax(helpText("Wartość parametru: $$\\delta$$")),
                  min = -50, max = 0,step = 0.1, value = -.1),
    ),
    textInput("iloscsymulacji", h3("Liczba obserwacji do wygenerowania"), 
              value = 1000) 
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Definicja", h4(textOutput("diagTitle")),
               withMathJax(uiOutput("formula"))))
    ,
    tabsetPanel(
      tabPanel("Wykres", plotOutput("plot"))
     
      
      
    ),
    tabPanel("Wykres", uiOutput("zaleznosc")),
    
    
    
    
      
      
    )
  )
)


serwer2<-shinyServer(function(input, output) {
  data <- reactive({  
    dist <- switch(input$dist,
                   '1' = normalCopula,
                   '2' = tCopula,
                   '3' = claytonCopula,
                   '4' = gumbelCopula,
                   '5' = frankCopula,
                   '6' = joeCopula,
                   '7' = BB1Copula,
                   '8' = BB6Copula,
                   '9' = BB7Copula,
                   '0' = r90BB7Copula,
                   '10' = BB8Copula,
                   '11' = BiCop
                   )
    
    if(input$dist %in% list('3') ){
      dist(input$n3)
    }
    else  if(input$dist %in% list('1') ){
      dist(input$n1)
    }
    else  if(input$dist %in% list('0') ){
      dist(c(input$n0,input$m0))
    }
    else  if(input$dist %in% list('2') ){
      dist(input$n2)
    }
    else  if(input$dist %in% list('4') ){
      dist(input$n4)
    }
    else  if(input$dist %in% list('5') ){
      dist(input$n5)
    }
    else  if(input$dist %in% list('6') ){
      dist(input$n6)
    }
    else if(input$dist %in% list('7')){
      dist(c(input$n7,input$m7))
    }
    else if(input$dist %in% list('8')){
      dist(c(input$n8,input$m8))
    }
    else if(input$dist %in% list('9')){
      dist(c(input$n9,input$m9))
    }
    else if(input$dist %in% list('10')){
      dist(c(input$n10,input$m10))
    }
    else if(input$dist %in% list('11')){
      if(input$n11==90){
        BiCop(13,2)
      }
      
    }
    
  })
  

  output$plot <- renderPlot({
    nazwa_kopuli <- switch(input$dist,
                           '0' = 'BB7 obrócona o 90 stopni',
                           '1' = 'Gaussa',
                           '2' = 'tStudenta',
                           '3' = 'Claytona',
                           '4' = 'Gumbela',
                           '5' = 'Franka',
                           '6' = 'Joe',
                           '7' = 'BB1',
                           '8' = 'BB6',
                           '9' = 'BB7',
                           '10' = 'BB8'
    )
    plot(rCopula(as.numeric(input$iloscsymulacji), data()), 
         main=paste("Kopula ", nazwa_kopuli),
         xlab = 'u1', ylab = 'u2')
  })
  output$formula <- renderUI({
    nazwa_kopuli <- switch(input$dist,
                           '1' = 'Gaussa',
                           '2' = 'tStudenta',
                           '3' = 'Claytona',
                           '4' = 'Gumbela',
                           '5' = 'Franka',
                           '6' = 'Joe' ,
                           '7' = 'BB1',
                           '8' = 'BB6',
                           '9' = 'BB7',
                           '10' = 'BB8',
                           '0' = 'rBB7'
    )
    if(input$dist=="3"){
      withMathJax(
        helpText('Dwuwymiarową kopulę ',nazwa_kopuli, 'definiujemy w następujący sposób: ','          
        $$C_{\\theta}^{Clayton}(u_{1}, u_{2})=\\left(u_{1}^{-\\theta}+
               u_{2}^{-\\theta}-1\\right)^{-\\frac{1}{\\theta}}$$
               dla $$\\theta >0.$$
                 '))
    }
    else if(input$dist=="1"){
      withMathJax(
        helpText('Dwuwymiarową kopulę ',nazwa_kopuli, 'definiujemy w następujący sposób: ','
               $$C_{\\theta}^{G a}(u_{1}, u_{2})=\\int_{-\\infty}^{\\Phi^{-1}(u_{1})} \\int_{-\\infty}^{\\Phi^{-1}(u_{2})} 
               \\frac{1}{2 \\pi \\sqrt{1-\\theta^{2}}}exp\\left(-\\frac{s^{2}-2 \\theta s t+t^{2}}
               {2\\left(1-\\theta^{2}\\right)}\\right) d s d t$$
               dla $$ -1 \\leq \\theta \\leq 1.$$
                 '))
    }
    else if(input$dist=="2"){
      withMathJax(
        helpText('Dwuwymiarową kopulę ',nazwa_kopuli, 'definiujemy w następujący sposób: ','
               $$C_{\\theta, v}^{S t}\\left(u_{1}, u_{2}\\right)=\\int_{-\\infty}^{t_{\\nu}^{-1}(u_{1})}
               \\int_{-\\infty}^{t_{\\nu}^{-1}(u_{2})} \\frac{1}{2 \\pi \\sqrt{1-\\theta^{2}}}
               \\left(1+\\frac{s^{2}-2 \\theta s t+t^{2}}{\\nu\\left(1-\\theta^{2}\\right)}\\right)^{-\\frac{(\\nu+2)}{2}} 
               \\mathrm{~d} s \\mathrm{~d} t$$
               dla $$ -1 \\leq \\theta \\leq 1.$$
                 '))
    }
    else if(input$dist=="4"){
      withMathJax(
        helpText('Dwuwymiarową kopulę ',nazwa_kopuli, 'definiujemy w następujący sposób: ','
               $$C_{\\theta}^{Gumbel}(u_{1}, u_{2})=\\exp \\left[-\\left\\{\\left(-\\ln u_{1}\\right)^{\\theta}+
               \\left(-\\ln u_{2}\\right)^{\\theta}\\right\\}^{\\frac{1}{\\theta}}\\right]$$
               dla $$\\theta \\geq 1.$$
                 '))
    }
    else if(input$dist=="5"){
      withMathJax(
        helpText('Dwuwymiarową kopulę ',nazwa_kopuli, 'definiujemy w następujący sposób: ','
               $$C_{\\theta}^{Frank}(u_{1}, u_{2})=-\\frac{1}{\\theta} \\ln 
               \\left(\\frac{1}{1-e^{-\\theta}}\\left[\\left(1-e^{-\\theta}\\right)
               -\\left(1-e^{-\\theta u_{1}}\\right)
               \\left(1-e^{-\\theta u_{2}}\\right)\\right]\\right)$$
               dla $$\\theta >0.$$
                 '))
    }
    else if(input$dist=="6"){
      withMathJax(
        helpText('Dwuwymiarową kopulę ',nazwa_kopuli, 'definiujemy w następujący sposób: ','
               $$C_{\\theta}^{Joe}(u_{1}, u_{2})=1-\\left(\\left(1-u_{1}\\right)^{\\theta}+
               \\left(1-u_{2}\\right)^{\\theta}-
               \\left(1-u_{1}\\right)^{\\theta}\\left(1-
               u_{2}\\right)^{\\theta}\\right)^{\\frac{1}{\\theta}}$$
               dla $$\\theta >1.$$
                 '))
    }
    else if(input$dist=="7"){
      withMathJax(
        helpText('Dwuwymiarową kopulę ','Claytona-Gumbela', 'definiujemy w następujący sposób: ','
               $$C_{\\theta, \\delta}^{B B 1}(u_{1}, u_{2})=
               \\left\\{1+\\left[\\left(u_{1}^{-\\theta}-1\\right)^{\\delta}+
               \\left(u_{2}^{-\\theta}-1\\right)^{\\delta}\\right]^{\\frac{1}{\\delta}}
               \\right\\}^{-\\frac{1}{\\theta}}$$
               dla $$\\theta > 0,$$
               oraz $$\\delta \\geq 1.$$
                 '))
    }
    else if(input$dist=="8"){
      withMathJax(
        helpText('Dwuwymiarową kopulę ','Joe-Gumbela', 'definiujemy w następujący sposób: ','
               $$\\left.\\left.C_{\\theta, \\delta}^{B B 6}\\left(u_{1}, u_{2}\\right)=
               1-\\left(1-\\exp -\\left[\\left(-\\log \\left(1-u_{1}\\right)^{\\theta}
               \\right)\\right)\\right)^{\\delta}+\\left(-\\log \\left(1-
               \\left(1-u_{2}\\right)^{\\theta}\\right)\\right)^{\\delta}\\right]^{\\frac{1}{\\delta}}\\right)^{\\frac{1}{\\theta}}$$
               dla $$\\theta \\geq 1,$$
               oraz $$\\delta > 0.$$
                 '))
    }
    else if(input$dist=="9"){
      withMathJax(
        helpText('Dwuwymiarową kopulę ','Joe-Claytona', 'definiujemy w następujący sposób: ','
               $$C_{\\theta, \\delta}^{B B 7}\\left(u_{1}, u_{2}\\right)=
               1-\\left[1-\\left(\\left(1-u_{1}^{\\theta}\\right)^{-\\delta}+
               \\left(1-u_{2}\\right)^{-\\delta}-1\\right)^{-\\frac{1}{\\delta}}\\right]^{\\frac{1}{\\theta}}$$
               dla $$\\theta \\geq 1,$$
               oraz $$\\delta > 0.$$
                 '))
    }
    else if(input$dist=="10"){
      withMathJax(
        helpText('Dwuwymiarową kopulę ','Joe-Franka', 'definiujemy w następujący sposób: ','
               $$\\left.C_{\\theta, \\delta}^{B B 8}\\left(u_{1}, u_{2}\\right)=
               \\frac{1}{\\theta}\\left(1-\\left[1-\\frac{1}{1-(1-\\delta)^{\\theta}}
               \\left(1-\\left(1-\\delta u_{1}\\right)^{\\theta}\\right)
               \\left(1-\\delta u_{2}\\right)^{\\theta}\\right)\\right]^{\\frac{1}{\\delta}}\\right)$$
               dla $$\\theta \\geq 1,$$
               oraz $$ 0< \\delta < 1.$$
                 '))
      
    }
    else if(input$dist=="0"){
      withMathJax(
        helpText('Kopule obrócone: ','
               $$\\begin{array}{l}
                C_{r o t 90}\\left(u_{1}, u_{2}\\right)=u_{2}-C\\left(1-u_{1}, u_{2}\\right) \\\\
                C_{r o t 180}\\left(u_{1}, u_{2}\\right)=u_{1}+u_{2}-1+C\\left(1-u_{1}, 1-u_{2}\\right) \\\\
                C_{r o t 270}\\left(u_{1}, u_{2}\\right)=u_{1}-C\\left(u_{1}, 1-u_{2}\\right)
                \\end{array}$$
                 '))}
    
  })
  output$zaleznosc<-renderText({
    Kend<-round(tau(data()),2)
    
    paste('<B>Wartość współczynnika Kendalla wynosi:<B>',Kend)})
})



shinyApp(ui = ui1, server = serwer2)
