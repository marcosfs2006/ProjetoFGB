#Funcao
{
  calculaFG = function(Idade,Sexo,Estado,Beneficio,TabuaMorte,ProbFamilia,Repeticoes,Periodo,TaxaJuros,DiferencaIdadeConjuge,DiferencaIdadePaiFilho,DiferencaIdadeMaeFilho,CrescimentoSalarial,IdadeLimite) {
    Dados = data.frame(Idade=Idade,Sexo=Sexo,Estado=Estado,Beneficio=Beneficio,ProbMorte=NA,ProbConj=NA,ProbFilho=NA,IdadeAtual=Idade,
                       SexoAtual=Sexo,EstadoAtual=Estado,stringsAsFactors = FALSE)
    Dados[1:Repeticoes,] = Dados[1,]
    
    #Defindindo as probabilidades de morte e de composicao familiar para cada idade, sexo e estado
    Dados$ProbMorte[Dados$EstadoAtual==1] = TabuaMorte$qx[Dados$IdadeAtual[Dados$EstadoAtual==1]+1]
    Dados$ProbMorte[Dados$EstadoAtual==2] = TabuaMorte$qx[Dados$IdadeAtual[Dados$EstadoAtual==2]+NTabuaMorte+1]
    Dados$ProbMorte[Dados$EstadoAtual==3] = TabuaMorte$qx[Dados$IdadeAtual[Dados$EstadoAtual==3]+NTabuaMorte*2+1]
    Dados$ProbConj[Dados$SexoAtual=="Feminino"] = ProbFamilia$ProbConj[Dados$IdadeAtual[Dados$SexoAtual=="Feminino"]+1]
    Dados$ProbConj[Dados$SexoAtual=="Masculino"] = ProbFamilia$ProbConj[Dados$IdadeAtual[Dados$SexoAtual=="Masculino"]+NProbFamilia+1]
    Dados$ProbFilho[Dados$SexoAtual=="Feminino"] = ProbFamilia$ProbFilho[Dados$IdadeAtual[Dados$SexoAtual=="Feminino"]+1]
    Dados$ProbFilho[Dados$SexoAtual=="Masculino"] = ProbFamilia$ProbFilho[Dados$IdadeAtual[Dados$SexoAtual=="Masculino"]+NProbFamilia+1]
    
    if(Sexo=="Masculino") {
      DiferencaIdadeConjuge = DiferencaIdadeMaridoEsposa
    } else {
      DiferencaIdadeConjuge = -DiferencaIdadeMaridoEsposa
    }
    
    Estados = matrix(0,nrow = nrow(Dados),Periodo)
    Estados[,1] = Dados$Estado
    colnames(Estados) = 1:Periodo
    
    for (t in 1:(Periodo-1)) {
      #print(paste(Dados$IdadeAtual[1],Dados$EstadoAtual[1],Dados$ProbMorte[1]))
      #Testando se o individuo morreu
      Indices = which(Estados[,t]!=4) #Todos os vivos no tempo t
      if(length(Indices)>0) {
        TesteMorreu = rbinom(length(Indices),1,Dados$ProbMorte[Indices])
        #Se morreu invalido ou conjuge pensionista torna-se 9
        TesteMorreu = replace(TesteMorreu,TesteMorreu==1&(Estados[Indices,t]==1|Estados[Indices,t]==2),9) 
        #Se filho dependente morreu, entao acabam-se os beneficios para este vinculo, tornando-se 4
        TesteMorreu = replace(TesteMorreu,TesteMorreu==1&(Estados[Indices,t]==3),4)
        TesteMorreu[TesteMorreu==FALSE] = Estados[Indices[TesteMorreu==FALSE],t]
        Estados[Indices,t+1] = TesteMorreu
      }
      #Se o individuo ja alcancou a idade limite da tabua de morte, entao morreu
      Estados[Dados$IdadeAtual>max(TabuaMorte$x),t+1] = 4
      
      #Testando se invalido que morreu deixou conjuge dependente
      Indices = which(Estados[,t+1]==9&Estados[,t]==1) #Todos os invalidos que morreram
      if(length(Indices)>0) {
        TesteConjuge = rbinom(length(Indices),1,Dados$ProbConj[Indices])
        TesteConjuge = replace(TesteConjuge,TesteConjuge==1,2)
        TesteConjuge = replace(TesteConjuge,TesteConjuge==0,9)
        #IndicesMulherFalecida = intersect(Indices[TesteConjuge==4],which(Dados$Sexo=="Mulher"))
        #IndicesHomemFalecido = intersect(Indices[TesteConjuge==4],which(Dados$Sexo=="Homem"))
        #Dados$IdadeAtual[IndicesMulherFalecida] = calculaIdadeConjuge(Dados$IdadeAtual[IndicesMulherFalecida],1)
        #Dados$IdadeAtual[IndicesHomemFalecido] = calculaIdadeConjuge(Dados$IdadeAtual[IndicesHomemFalecido],0)
        Dados$IdadeAtual[Indices] = Dados$IdadeAtual[Indices] - DiferencaIdadeConjuge
        Dados$SexoAtual[Dados$Sexo=="Masculino"][Indices] = "Feminino"
        Dados$SexoAtual[Dados$Sexo=="Feminino"][Indices] = "Masculino"
        Estados[Indices,t+1] = TesteConjuge
        
        #Testando se o novo conjuge dependente morreu
        Dados$ProbMorte[Estados[,t+1]==2] = TabuaMorte$qx[Dados$IdadeAtual[Estados[,t+1]==2]+NTabuaMorte+1]
        if(length(Indices)>0) {
          TesteMorreu = rbinom(length(Indices),1,Dados$ProbMorte[Indices])
          TesteMorreu = replace(TesteMorreu,TesteMorreu==1,9) 
          TesteMorreu = replace(TesteMorreu,TesteMorreu==0,2) 
          Estados[Indices,t+1] = TesteMorreu
        }
        #Se o conjuge ja alcancou a idade limite da tabua de morte, entao morreu
        Estados[Dados$IdadeAtual>max(TabuaMorte$x),t+1] = 4
      }
      
      #Testando se invalido/conjuge que morreu deixou filho dependente
      #Indices = which(Estados[,t+1]==9)
      #Todos os o que morreram no tempo atual e nao deixaram conjuge dependente e tem idade para ter filho dependente segundo as premissas demograficas e legal
      Indices = which(Estados[,t+1]==9&(((Dados$SexoAtual=="Masculino")&(Dados$IdadeAtual>=DiferencaIdadePaiFilho)&(IdadeLimite>(Dados$IdadeAtual-DiferencaIdadePaiFilho)))|
                                          ((Dados$SexoAtual=="Feminino")&(Dados$IdadeAtual>=DiferencaIdadeMaeFilho)&(IdadeLimite>(Dados$IdadeAtual-DiferencaIdadeMaeFilho)))))
      if(length(Indices)>0) {
        TesteFilho = rbinom(length(Indices),1,Dados$ProbFilho[Indices])
        TesteFilho = replace(TesteFilho,TesteFilho==1,3)
        TesteFilho = replace(TesteFilho,TesteFilho==0,9)
        #IndicesMaeFalecida = intersect(Indices[TesteFilho==5],which(Dados$Sexo=="Mulher"))
        #IndicesPaiFalecido = intersect(Indices[TesteFilho==5],which(Dados$Sexo=="Homem"))
        #Dados$IdadeAtual[IndicesMaeFalecida] = calculaIdadeFilho(Dados$IdadeAtual[IndicesMaeFalecida],1)
        #Dados$IdadeAtual[IndicesPaiFalecido] = calculaIdadeFilho(Dados$IdadeAtual[IndicesPaiFalecido],0)
        x = Dados$IdadeAtual[Dados$SexoAtual=="Masculino"][Indices]
        y = Dados$IdadeAtual[Dados$SexoAtual=="Feminino"][Indices]
        Dados$IdadeAtual[intersect(which(Dados$SexoAtual=="Masculino"),Indices)] = Dados$IdadeAtual[intersect(which(Dados$SexoAtual=="Masculino"),Indices)] - DiferencaIdadePaiFilho
        Dados$IdadeAtual[intersect(which(Dados$SexoAtual=="Feminino"),Indices)] = Dados$IdadeAtual[intersect(which(Dados$SexoAtual=="Feminino"),Indices)] - DiferencaIdadeMaeFilho
        Dados$SexoAtual[Indices] = sample(c("Masculino","Feminino"),length(Indices),replace = TRUE)
        #Dados$IdadeAtual[Dados$IdadeAtual<0] = 0
        Estados[Indices,t+1] = TesteFilho
        
        #Testando se o novo filho dependente morreu
        Dados$ProbMorte[Estados[,t+1]==3] = TabuaMorte$qx[Dados$IdadeAtual[Estados[,t+1]==3]+NTabuaMorte*2+1]
        if(length(Indices)>0) {
          TesteMorreu = rbinom(length(Indices),1,Dados$ProbMorte[Indices])
          TesteMorreu = replace(TesteMorreu,TesteMorreu==1,9) 
          TesteMorreu = replace(TesteMorreu,TesteMorreu==0,3) 
          Estados[Indices,t+1] = TesteMorreu
        }
        
        #Se o filho ja alcancou a idade limite da tabua de morte, entao morreu
        Estados[Dados$IdadeAtual>max(TabuaMorte$x),t+1] = 4
      }
      
      #Testando se filho dependente atingiu a idade limite legal para receber pensao
      Indices = which(Estados[,t]==3&Estados[,t+1]!=4) #Todos os filhos dependentes vivos no tempo t que nao morreram no tempo t
      if(length(Indices)>0) {
        TesteFilhoSuperou = Dados$IdadeAtual[Indices]>=IdadeLimite
        TesteFilhoSuperou[TesteFilhoSuperou==TRUE] = 4
        TesteFilhoSuperou[TesteFilhoSuperou==FALSE] = 3
        Estados[Indices,t+1] = TesteFilhoSuperou
      }
      
      #Atribuindo 4 aos mortos que nao deixaram dependentes
      Estados[Estados[,t+1]==9,t+1] = 4
      
      #Aumentando um ano de idade para cada invalido/pensionista
      Dados$IdadeAtual = Dados$IdadeAtual + 1
      
      #Vinculo terminado, continua terminado para sempre
      Estados[Estados[,t]==4,t+1] = 4
      
      #Atualizando o novo estado atual para o proximo periodo
      Dados$EstadoAtual = Estados[,t+1]
      
      #Atualizando probablidades de morte e familiar para a nova idade atual
      Dados$ProbMorte[Dados$EstadoAtual==1] = TabuaMorte$qx[Dados$IdadeAtual[Dados$EstadoAtual==1]+1]
      Dados$ProbMorte[Dados$EstadoAtual==2] = TabuaMorte$qx[Dados$IdadeAtual[Dados$EstadoAtual==2]+NTabuaMorte+1]
      Dados$ProbMorte[Dados$EstadoAtual==3] = TabuaMorte$qx[Dados$IdadeAtual[Dados$EstadoAtual==3]+NTabuaMorte*2+1]
      Dados$ProbConj[Dados$SexoAtual=="Feminino"] = ProbFamilia$ProbConj[Dados$IdadeAtual[Dados$SexoAtual=="Feminino"]+1]
      Dados$ProbConj[Dados$SexoAtual=="Masculino"] = ProbFamilia$ProbConj[Dados$IdadeAtual[Dados$SexoAtual=="Masculino"]+NProbFamilia+1]
      Dados$ProbFilho[Dados$SexoAtual=="Feminino"] = ProbFamilia$ProbFilho[Dados$IdadeAtual[Dados$SexoAtual=="Feminino"]+1]
      Dados$ProbFilho[Dados$SexoAtual=="Masculino"] = ProbFamilia$ProbFilho[Dados$IdadeAtual[Dados$SexoAtual=="Masculino"]+NProbFamilia+1]
      
      if(sum(Estados[,t]==4)==nrow(Estados)) {
        Estados[,(t+1):ncol(Estados)] = 4
        break
      }
    }
    
    BenefProj = matrix(NA,nrow(Estados),ncol(Estados))
    BenefProj[,1] = Dados$Beneficio
    for(i in 2:ncol(BenefProj)) {
      BenefProj[,i] = BenefProj[,i-1]*(1+CrescimentoSalarial)
    }
    
    AposentadoriaInvPagas = ifelse(Estados==1,1,0)*BenefProj
    PensoesConjugePagas = ifelse(Estados==2,1,0)*BenefProj
    PensoesFilhoPagas = ifelse(Estados==3,1,0)*BenefProj
    PensoesPagas = ifelse(Estados==2|Estados==3,1,0)*BenefProj
    
    VPAposentadorias = (t(t(AposentadoriaInvPagas)*((1+TaxaJuros)^(-(0:(ncol(AposentadoriaInvPagas)-1))))))
    VPPensoes = (t(t(PensoesPagas)*((1+TaxaJuros)^(-(0:(ncol(PensoesPagas)-1))))))
    VPPensoesConjuge = mean(rowSums(t(t(PensoesConjugePagas)*((1+TaxaJuros)^(-(0:(ncol(PensoesConjugePagas)-1)))))))
    VPPensoesFilhoPagas = mean(rowSums(t(t(PensoesFilhoPagas)*((1+TaxaJuros)^(-(0:(ncol(PensoesFilhoPagas)-1)))))))
    
    
    FGAposentadorias = rowSums(VPAposentadorias)
    FGPensoes = rowSums(VPPensoes)
    FGTotal = FGAposentadorias + FGPensoes
    MediaFGTotal = mean(FGTotal)
    FGTotal95Confianca = c(quantile(FGTotal,0.05),quantile(FGTotal,0.95))
    FundoOscilacaoRisco = FGTotal95Confianca[2] - MediaFGTotal
    TabelaResultados = data.frame(MediaFGAposentadorias=mean(FGAposentadorias),MediaFGPensoes=mean(FGPensoes),MediaFGTotal=MediaFGTotal,
                                  FGTotalPercentil5=FGTotal95Confianca[1],FGTotalPercentil95=FGTotal95Confianca[2],
                                  FundoOscilacaoRisco95=FundoOscilacaoRisco)
    rownames(TabelaResultados) = ""
    return(TabelaResultados)
  }
}

#Definindo pasta padrao que contem a tabua de vida
setwd("C:/Users/danil/Dropbox/Projeto Fundos Garantidores RCC/Funções no R")
Tabua = read.csv("TabuaMorteIBGE2010.csv",sep = ";",dec=',')

#Definindo hipoteses e informacoes do beneficiario
{
  Repeticoes = 100000
  Periodo = 100
  TaxaJuros = 0.06
  Idade = 21
  Estado = 2 #1=Invalido, 2=Conjuge pensionista, 3=Filho pensionista
  Sexo = "Masculino" #Masculino ou Feminino
  DiferencaIdadeMaridoEsposa = 5
  Beneficio = 13000 #Valor total do beneficio anualmente
  DiferencaIdadePaiFilho = 30
  DiferencaIdadeMaeFilho = 25
  CrescimentoSalarial = 0
  IdadeLimite = 21
  #Carregando as tabuas de mortalidade (IBGE2010)
  {
    TabuaInvalido = Tabua[Tabua$sexo==Sexo,-2]
    if(Sexo=="Masculino"&Estado==1) {
      TabuaConjuge = Tabua[Tabua$sexo=="Feminino",-2]
    } else if(Sexo=="Feminino"&Estado==1) {
      TabuaConjuge = Tabua[Tabua$sexo=="Masculino",-2]
    } else if(Sexo=="Masculino"&Estado==2) {
      TabuaConjuge = Tabua[Tabua$sexo=="Masculino",-2]
    } else if(Sexo=="Feminino"&Estado==2) {
      TabuaConjuge = Tabua[Tabua$sexo=="Feminino",-2]
    } else {
      TabuaConjuge = Tabua[Tabua$sexo=="Feminino",-2]
      TabuaConjuge[,] = NA
    }
    TabuaFilho = Tabua[Tabua$sexo=="Feminino",-2]
    TabuaMorte = rbind(TabuaInvalido,TabuaConjuge,TabuaFilho)
    NTabuaMorte = nrow(TabuaInvalido)
    #Criando uma tabua de prob de composicao familiar com prob de 100% em todas as idades para testes
    ProbFamilia = data.frame(Idade=rep(0:((nrow(Tabua)-1)/2),2),
                             Sexo=c(rep("Feminino",nrow(Tabua)/2),rep("Masculino",nrow(Tabua)/2)),ProbConj=1,ProbFilho=1,stringsAsFactors = FALSE)
    NProbFamilia = nrow(ProbFamilia)/2
  }
}

calculaFG(Idade,Sexo,Estado,Beneficio,TabuaMorte,ProbFamilia,Repeticoes,Periodo,TaxaJuros,DiferencaIdadeMaridoEsposa,DiferencaIdadePaiFilho,DiferencaIdadeMaeFilho,CrescimentoSalarial,IdadeLimite)

estados = c("Invalido","Conjuge","Filho")
Resultados = data.frame(Estado=NULL,Sexo=NULL,Idade=NULL,FGAposentadorias=NULL,FGPensoes=NULL,FGTotal=NULL,FGTotalPercentil5=NULL,FGTotalPercentil95=NULL,FundoOscilacaoRisco95=NULL)
for(estado in 3:1){
  for(sexo in c("Feminino","Masculino")) {
    for(idade in c(0,15,21,40,60,80,100,111)) {
      if(estado==3&idade>21) break
      if(estado==1&idade<=18) next
      #Carregando as tabuas de mortalidade (IBGE2010)
      {
        Tabua = read.csv("TabuaMorteIBGE2010.csv",sep = ";",dec=',')
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
        TabuaMorte = rbind(TabuaInvalido,TabuaConjuge,TabuaFilho)
        NTabuaMorte = nrow(TabuaInvalido)
        #Criando uma tabua de prob de composicao familiar com prob de 100% em todas as idades para testes
        ProbFamilia = data.frame(Idade=rep(0:((nrow(Tabua)-1)/2),2),
                                 Sexo=c(rep("Feminino",nrow(Tabua)/2),rep("Masculino",nrow(Tabua)/2)),ProbConj=1,ProbFilho=1,stringsAsFactors = FALSE)
        NProbFamilia = nrow(ProbFamilia)/2
      }
      Resultado = calculaFG(idade,sexo,estado,Beneficio,TabuaMorte,ProbFamilia,Repeticoes,Periodo,TaxaJuros,DiferencaIdadeMaridoEsposa,DiferencaIdadePaiFilho,DiferencaIdadeMaeFilho,CrescimentoSalarial,IdadeLimite)
      Resultados = rbind(Resultados,cbind(estados[estado],sexo,idade,Resultado))
    }
  }
}  
names(Resultados)[1:3] = c("Estado","Sexo","Idade")
Resultados
write.table(Resultados, "clipboard", sep="\t", dec = ",",row.names=FALSE)
