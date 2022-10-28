

# 0001  Presidente Majoritário
# 0003  Governador Majoritário
# 0005  Senador Majoritário
# 0011  Prefeito Majoritário
# 0006  Deputado Federal Proporcional
# 0007  Deputado Estadual Proporcional
# 0008  Deputado Distrital Proporcional
# 0013  Vereador Proporcional


if ( interactive() ) {
  
  {if(!require(shiny)) install.packages('shiny')
    require(shiny)}

  {if(!require(shinydashboard)) install.packages('shinydashboard')
    require(shinydashboard)}

  {if(!require(dashboardthemes)) install.packages('dashboardthemes')
    require(dashboardthemes)}

  {if(!require(tidyverse)) install.packages('tidyverse')
    require(tidyverse)}

  {if(!require(plotly)) install.packages('plotly')
    require(plotly)}
  
  
  # Titulo 
  header <- dashboardHeader(titleWidth = 500,
    title = 'Apuração dos Votos do Segundo Turno'
  )
  
  UFP <- list(Brasil = 'br', Acre = 'ac', Alagoas = 'al', Amapa = 'ap', Amazonas = 'am',
             Bahia = 'ba', Ceara = 'ce', Distrito_Federal = 'df', Espirito_Santo = 'es',
             Goias = 'go', Maranhao = 'ma', Mato_Grosso = 'mt', Mato_Grosso_do_Sul = 'ms',
             Minas_Gerais = 'mg', Para = 'pa', Paraiba = 'pb', Parana = 'pr',
             Pernambuco = 'pe', Piaui = 'pi', Rio_de_Janeiro = 'rj', Rio_Grande_do_Norte = 'rn',
             Rio_Grande_do_Sul = 'rs', Rondonia = 'ro', Roraima = 'rr', Santa_Catarina = 'sc',
             Sao_Paulo = 'sp', Sergipe = 'se', Tocantins = 'to')
  
  UFG <- list(Alagoas = 'al', Amazonas = 'am', Bahia = 'ba', Espirito_Santo = 'es', 
              Mato_Grosso = 'mt', Mato_Grosso_do_Sul = 'ms', Paraiba = 'pb', Pernambuco = 'pe',
              Rondonia = 'ro', Santa_Catarina = 'sc', Sao_Paulo = 'sp', Sergipe = 'se')
  
  # Barra Lateral
  sidebar <- dashboardSidebar(disable = T)
  
  # Corpo do Web App 
  body <- dashboardBody(
    shinyDashboardThemes(
      theme = 'grey_light' #"onenote"
    ),
    fluidRow(tabsetPanel(
      tabPanel(p("Presidente"),
               box(width = 12, title = 'Informações Gerais da Apuração',
                   infoBoxOutput('box3', width = 6), # Urnas Apuradas
                   infoBoxOutput('box4', width = 6)  # Ultima Atualizacao
               ),
               box(width = 12,title = 'Candidato Eleito',
                   column(width = 6,
                          infoBoxOutput('box1', width = 12)),  # Presidente Eleito
                   # Estado
                   column(width = 6,
                   selectizeInput(
                     inputId = "UFP",
                     label =  "Unidade Federativa",
                     selected = 'Brasil',
                     choices = names(UFP),
                     multiple = F,
                     width = 135,
                     size = 12
                   ))),
               box(width = 6, title = 'Grafico Presidente',
                   plotlyOutput('plot1')),
               box(width = 6, title = 'Tabela Presidente',
                   dataTableOutput("tab1"))
      ),
      tabPanel(p("Governador"),
               box(width = 12, title = 'Informações Gerais da Apuração',
                   infoBoxOutput('box5',width = 6), # Urnas Apuradas
                   infoBoxOutput('box6',width = 6)  # Ultima Atualizacao
               ),
               box(width = 12, title = 'Candidato Eleito',
                   column(width = 6,
                   infoBoxOutput('box2',width = 12)), # Governador Eleito
                   # Estado
                   column(width = 6,
                   selectizeInput(
                     inputId = "UFG",
                     label =  "Unidade Federativa",
                     selected = 'Bahia',
                     choices = names(UFG),
                     multiple = F,
                     width = 135,
                     size = 12
                   ))),
               box(width = 6, title = 'Grafico Governador',
                   plotlyOutput('plot2')
                   ),
               box(width = 6, title = 'Tabela Governador',
                   dataTableOutput("tab2"))
      )
    )),
    fluidRow(
      box(width = 6, title = 'Autor',
                 icon("user-tie",'fa-solid', lib = "font-awesome"),
                 'Lucas da Silva Costa',
                 br(),
                 icon("user-graduate",'fa-solid', lib = "font-awesome"),
                 'Estatístico pela Universidade Federal de Sergipe',
                 br(),
                 icon("laptop-code",'fa-solid', lib = "font-awesome"),
                 'Estagiário no Tribunal Regional Eleitoral de Sergipe',
                 br(),
                 icon("linkedin",'fa-solid', lib = "font-awesome"),
                 a('Linkedin',href = 'https://www.linkedin.com/in/lucas-costa-2a43aa1a1'),
                 br(),
                 icon("github",'fa-solid', lib = "font-awesome"),
                 a('GitHub',href = 'https://github.com/luukas20')
                 
      )
    )
  )
  
  # Define server logic required to draw a histogram
  server <- function(input, output) {
    # Presidente Eleito
    output$box1 <- renderInfoBox({
        url_ee <- paste('https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/',UFP[input$UFP],'/',UFP[input$UFP],'-c0001','-e000545-r.json', sep = '')
        ap <- jsonlite::fromJSON(url_ee, simplifyDataFrame = T)
        infoBox(
          'Presidente', ap$nm[max(as.numeric(ap$vap))],
          color = 'green', fill = T, icon=icon("user",'fa-solid', lib = "font-awesome")
        )
    })
    # Governador Eleito
    output$box2 <- renderInfoBox({
        url_ee <- paste('https://resultados.tse.jus.br/oficial/ele2022/547/dados-simplificados/',UFG[input$UFG],'/',UFG[input$UFG],'-c0003','-e000547-r.json', sep = '')
        ap <- jsonlite::fromJSON(url_ee, simplifyDataFrame = T)
        infoBox(
          'Governador', ap$nm[max(as.numeric(ap$vap))],
          color = 'green', fill = T, icon=icon("user",'fa-solid', lib = "font-awesome")
        )
    })
    # Urnas Apuradas Presidente
    output$box3 <- renderInfoBox({
      url_ee <- paste('https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/',UFP[input$UFP],'/',UFP[input$UFP],'-c0001','-e000545-r.json', sep = '')
      ap <- jsonlite::fromJSON(url_ee, simplifyDataFrame = T)
        infoBox(
          'Urnas Apuradas', paste(ap$psa,'%'),
          color = 'green', fill = T, icon=icon("sync-alt",'fa-solid', lib = "font-awesome")
        )
    })
    # Ultima Atualizacao Presidente
    output$box4 <- renderInfoBox({
      url_ee <- paste('https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/',UFP[input$UFP],'/',UFP[input$UFP],'-c0001','-e000545-r.json', sep = '')
      ap <- jsonlite::fromJSON(url_ee, simplifyDataFrame = T)
        infoBox(
          'Ultima Atualização', paste(ap$dt,ap$ht),
          color = 'green', fill = T, icon=icon("calendar-alt",'fa-solid', lib = "font-awesome")
        )
    })
    # Urnas Apuradas Governador
    output$box5 <- renderInfoBox({
      url_ee <- paste('https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/',UFG[input$UFG],'/',UFG[input$UFG],'-c0001','-e000545-r.json', sep = '')
      ap <- jsonlite::fromJSON(url_ee, simplifyDataFrame = T)
      infoBox(
        'Urnas Apuradas', paste(ap$psa,'%'),
        color = 'green', fill = T, icon=icon("sync-alt",'fa-solid', lib = "font-awesome")
      )
    })
    # Ultima Atualizacao Governador
    output$box6 <- renderInfoBox({
      url_ee <- paste('https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/',UFG[input$UFG],'/',UFG[input$UFG],'-c0001','-e000545-r.json', sep = '')
      ap <- jsonlite::fromJSON(url_ee, simplifyDataFrame = T)
      infoBox(
        'Ultima Atualização', paste(ap$dt,ap$ht),
        color = 'green', fill = T, icon=icon("calendar-alt",'fa-solid', lib = "font-awesome")
      )
    })
    # Tabela Presidente
    output$tab1 <- renderDataTable({
      url_ee <- paste('https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/',UFP[input$UFP],'/',UFP[input$UFP],'-c0001','-e000545-r.json', sep = '')
      ap <- jsonlite::fromJSON(url_ee, simplifyDataFrame = T)
      ap <- ap %>% 
        purrr::pluck("cand") %>% 
        tibble::as.tibble() %>%
        select(n,nm,vap,pvap,st) %>% 
        dplyr::mutate(Numero = n, Candidato = nm, Votos = as.numeric(vap), Porcentagem = pvap,Situação = st,.keep = 'unused')
      
      ap[order(ap$Votos, decreasing = T),]
    })
    # Tabela Governador
    output$tab2 <- renderDataTable({
        url_ee <- paste('https://resultados.tse.jus.br/oficial/ele2022/547/dados-simplificados/',UFG[input$UFG],'/',UFG[input$UFG],'-c0003','-e000547-r.json', sep = '')
        ap <- jsonlite::fromJSON(url_ee, simplifyDataFrame = T)
        ap <- ap %>% 
          purrr::pluck("cand") %>% 
          tibble::as.tibble() %>%
          select(n,nm,vap,pvap,st) %>% 
          dplyr::mutate(Numero = n, Candidato = nm, Votos = as.numeric(vap), Porcentagem = pvap,Situação = st,.keep = 'unused')
        
        ap[order(ap$Votos, decreasing = T),]
    })
    # Grafico Presidente
    output$plot1 <- renderPlotly({
      url_ee <- paste('https://resultados.tse.jus.br/oficial/ele2022/545/dados-simplificados/',UFP[input$UFP],'/',UFP[input$UFP],'-c0001','-e000545-r.json', sep = '')
      ap <- jsonlite::fromJSON(url_ee, simplifyDataFrame = T)
      ap <- ap %>% 
        purrr::pluck("cand") %>% 
        tibble::as.tibble() %>%
        select(n,nm,vap,pvap,st) %>% 
        dplyr::mutate(Numero = n, Candidato = nm, Votos = as.numeric(vap), Porcentagem = pvap,Situação = st,.keep = 'unused')
     
       ggplot(ap, aes(x=Candidato, y=Votos)) +
        geom_segment( aes(x=Candidato, xend=Candidato, y=0, yend=Votos), color="grey") +
        geom_point( color="green4", size=4) +
        theme_light() +
        theme(
          panel.grid.major.x = element_blank(),
          panel.border = element_blank(),
          axis.ticks.x = element_blank()
        ) +
        xlab("Candidato") +
        ylab("Votos")
    })
    # Grafico Governador
    output$plot2 <- renderPlotly({
      url_ee <- paste('https://resultados.tse.jus.br/oficial/ele2022/547/dados-simplificados/',UFG[input$UFG],'/',UFG[input$UFG],'-c0003','-e000547-r.json', sep = '')
      ap <- jsonlite::fromJSON(url_ee, simplifyDataFrame = T)
      ap <- ap %>% 
        purrr::pluck("cand") %>% 
        tibble::as.tibble() %>%
        select(n,nm,vap,pvap,st) %>% 
        dplyr::mutate(Numero = n, Candidato = nm, Votos = as.numeric(vap), Porcentagem = pvap,Situação = st,.keep = 'unused')
      
      ggplot(ap, aes(x=Candidato, y=Votos)) +
        geom_segment( aes(x=Candidato, xend=Candidato, y=0, yend=Votos), color="grey") +
        geom_point( color="green4", size=4) +
        theme_light() +
        theme(
          panel.grid.major.x = element_blank(),
          panel.border = element_blank(),
          axis.ticks.x = element_blank()
        ) +
        xlab("Candidato") +
        ylab("Votos")
    })
  }
  
  ui <- dashboardPage(skin = 'green',header, sidebar, body)
  
  shinyApp(ui,server)
}

