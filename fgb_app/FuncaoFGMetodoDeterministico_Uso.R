# Definindo pasta padrao que contem a tabua de vida

# setwd("C:/Users/danil/Dropbox/Projeto Fundos Garantidores RCC/Funções no R")
# options("scipen" = 999)

# Carregando as tabuas de mortalidade

  #Tabua <- read.csv("TabuaMorteIBGE2010.csv",sep = ";", dec=',')
    


  # Definindo taxa de juros
  TaxaJuros = 0.06
  Tabua <- geraTabuaComutUmaVida(Tabua,TaxaJuros)

  
  # Definindo parametros e premissas para rodar uma execucao da funcao

  Estado = 2                      # 1=Invalido, 2=Conjuge dependente, 3=Filho dependente
  Idade = 21
  Sexo = "Masculino"              # Pode ser Masculino ou Feminino
  DiferencaIdadeMaridoEsposa = 5
  DiferencaIdadePaiFilho = 30
  DiferencaIdadeMaeFilho = 25
  
  # Definindo as tabuas e premissas demograficas de composicao familiar
  {
    TabuaInvalido = Tabua[Tabua$sexo == Sexo, -2]
    
    if( Sexo == "Masculino" & Estado == 1 ) {
      
      TabuaConjuge = Tabua[Tabua$sexo=="Feminino", -2]
    
    } else if( Sexo == "Feminino" & Estado == 1 ) {
      
      TabuaConjuge = Tabua[Tabua$sexo=="Masculino", -2]
    
    } else if(Sexo == "Masculino" & Estado == 2) {
      
      TabuaConjuge = Tabua[Tabua$sexo == "Masculino", -2]
    
    } else if(Sexo=="Feminino"&Estado==2) {
      TabuaConjuge = Tabua[Tabua$sexo=="Feminino",-2]
    } else {
      TabuaConjuge = Tabua[Tabua$sexo=="Feminino",-2]
      TabuaConjuge[,] = NA
    }
    TabuaFilho = Tabua[Tabua$sexo=="Feminino",-2]
    
    if(Sexo=="Masculino") {
      DiferencaIdadeConjuge = DiferencaIdadeMaridoEsposa
      DiferencaIdadePaisFilhos = DiferencaIdadePaiFilho
    } else {
      DiferencaIdadeConjuge = -DiferencaIdadeMaridoEsposa
      DiferencaIdadePaisFilhos = DiferencaIdadeMaeFilho
    }
    TabuaFilho = Tabua[Tabua$sexo=="Feminino",-2]
  }
  
  IdadeLimite = 21
  
  ValorBenefAnual = 13000

  

#-------------------------------------------------------------------------------
# Executando funcao com os parametros acima selecionados
calculaFG(Estado,
          ValorBenefAnual,
          Idade,
          Sexo,
          TabuaInvalido,
          TabuaConjuge,
          TabuaFilho,
          TaxaJuros,
          IdadeLimite,
          DiferencaIdadeConjuge,
          DiferencaIdadePaisFilhos)


estados = c("Invalido","Conjuge","Filho")

Resultados = data.frame(Estado=NULL,
                        Sexo=NULL,
                        Idade=NULL,
                        FGAposentadorias=NULL,
                        FGPensoes=NULL,
                        FGTotal=NULL)


for(estado in 3:1){
  for(sexo in c("Feminino","Masculino")) {
    for(idade in c(0,15,21,40,60,80,100,111)) {
      if(estado==3&idade>21) break
      if(estado==1&idade<=18) next
      
      # Definindo as tabuas e premissas demograficas de composicao familiar
      {
        TabuaInvalido = Tabua[Tabua$sexo==sexo,-2]
        if(sexo=="Masculino"&estado==1) {
          TabuaConjuge = Tabua[Tabua$sexo=="Feminino",-2]
        } else if(sexo=="Feminino"&estado==1) {
          TabuaConjuge = Tabua[Tabua$sexo=="Masculino",-2]
        } else if(sexo=="Masculino"&estado==2) {
          TabuaConjuge = Tabua[Tabua$sexo=="Masculino",-2]
        } else if(sexo=="Feminino"&estado==2) {
          TabuaConjuge = Tabua[Tabua$sexo=="Feminino",-2]
        } else {
          TabuaConjuge = Tabua[Tabua$sexo=="Feminino",-2]
          TabuaConjuge[,] = NA
        }
        TabuaFilho = Tabua[Tabua$sexo=="Feminino",-2]
        
        if(sexo=="Masculino") {
          DiferencaIdadeConjuge = DiferencaIdadeMaridoEsposa
          DiferencaIdadePaisFilhos = DiferencaIdadePaiFilho
        } else {
          DiferencaIdadeConjuge = -DiferencaIdadeMaridoEsposa
          DiferencaIdadePaisFilhos = DiferencaIdadeMaeFilho
        }
      }
      Resultado = calculaFG(estado,ValorBenefAnual,idade,sexo,TabuaInvalido,TabuaConjuge,TabuaFilho,TaxaJuros,IdadeLimite,DiferencaIdadeConjuge,DiferencaIdadePaisFilhos)
      Resultados = rbind(Resultados,cbind(estados[estado],sexo,idade,Resultado))
    }
  }
}


names(Resultados)[1:3] = c("Estado","Sexo","Idade")

Resultados

write.table(Resultados, "clipboard", sep="\t", dec = ",",row.names=FALSE)




# Plotando grafico para todas idades do invalido
  
  Dist = NULL
  for(i in 18:max(TabuaInvalido$x)) {
    FG = calculaFGInvalidez(ValorBenefAnual,i,Sexo,TabuaInvalido,TabuaConjuge,TabuaFilho,TaxaJuros,IdadeLimite,DiferencaIdadeConjuge,DiferencaIdadePaisFilhos)
    Dist = c(Dist,FG)
  }
  plot(Dist,xaxt='n')
  axis(1,1:length(Dist),18:(max(TabuaInvalido$x)))
 

