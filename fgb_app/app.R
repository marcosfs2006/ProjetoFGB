#
# Aplicação shiny para o cálculo do Fundo Garantidor de Benefícios
# 



library(shiny)


# Carrega as funções e dados necessários

source("FuncaoFGMetodoDeterministico.R")

ibge_fem   <- readRDS("dados/ibge_fem.Rds")
ibge_masc  <- readRDS("dados/ibge_mas.Rds")
ibge_ambos <- readRDS("dados/ibge_ambos.Rds")

tabuas_iba <- readRDS("dados/tabuasIBA.Rds")


# Definição da UI
ui <- fluidPage(

    # Título da Aplicação
    titlePanel(strong("Calculadora do Fundo Garantidor de Benefícios - FGB")),
    
    hr(),
            
    # Definição dos controles
    selectInput("tabua_motalidade_geral",
                label = "Tábua de Motalidade Geral", 
                choices = list("IBGE 2010 Masculino"   = 1,
                               "IBGE 2010 Feminino"    = 2,
                               "IBGE 2010 Ambos Sexos" = 3), 
                selected = 1),
    
             selectInput("tabua_invalidez",
                        label = "Tábua de Invalidez", 
                        choices = list("ALVARO VINDAS"   = 1,
                                       "GRUPO AMERICANA" = 2,
                                       "HUNTERS"         = 3,
                                       "IAPB-57 FORTE"   = 4,
                                       "IAPB-57 FRACA"   = 5), 
                        selected = 1),
             
             sliderInput("tx_juros",
                         label = "Taxa de Juros",
                         min = 0, 
                         max = 6,
                         value = 0,
                         step=0.1,
                         post="%"),
    
             numericInput("idade_beneficiario",
                          label = "Idade do Beneficiário",
                          min = 18,
                          max = 115,
                          value = 0),

            
             numericInput("vlr_beneficio",
                          label = "Valor do Benefício Mensal",
                          value = 0),
    
    radioButtons("sexo",
                 label = "Sexo",
                 choices = list("Masculino" = 1, "Feminino" = 2), 
                 selected = NULL,
                 inline = TRUE),
    
    
    hr(),
            
            fluidRow(column(3, verbatimTextOutput("valor1")))
)




# Define server logic required to draw a histogram
server <- function(input, output) {

    output$valor1 <- renderPrint({ input$tabua_invalidez })
    
}



# Run the application 
shinyApp(ui = ui, server = server)


# Tem conjuge? sim não
# Idade conjuge
# idade filhos

