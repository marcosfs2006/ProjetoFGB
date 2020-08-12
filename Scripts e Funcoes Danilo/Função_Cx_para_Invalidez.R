#Funcoes
{
  #Considera-se que, na morte do invalido, o conjuge recebe a pensao integral enquanto estiver vivo.
  #Isso porque o valor a ser pago sera o mesmo, independente de haver filho dependente
  #Em caso de morte do conjuge, a pensao eh revertida para o filho mais novo que seja menor do que a idade limite legal
  #Em caso de filho pensionista (ou seja, o servidor falecido nao possuia conjuge), considera-se que o filho mais novo eh o pensionista
  #Em caso da morte deste filho pensionista, cessa-se o beneficio
  calculaCxInvalidez = function (ValorBenefAnual,Idade,Sexo,TabuaInvalido,TabuaConjuge,TabuaFilho,TaxaJuros,IdadeLimite,DiferencaIdadeConjuge,DiferencaIdadePaisFilhos) {
    IdadeConjuge = Idade - DiferencaIdadeConjuge
    if(Sexo=="Masculino") {
      SexoConjuge = "Feminino"
    } else {
      SexoConjuge = "Masculino"
    }
    
    Cx = 0
    for(n in 0:(max(TabuaInvalido$x)-Idade)) {
      Cx = Cx + Vn*npx*qxn*Hx
    }
    
    return(Cx)
  }
  
  geraTabuaComutUmaVida = function(Tabua,TaxaJuros) {
    Tabua$dx = Tabua$lx*Tabua$qx
    Tabua$v = 1/((1+TaxaJuros)^Tabua$x)
    Tabua$Dx = Tabua$lx*Tabua$v
    Tabua$Nx[Tabua$sexo=="Masculino"] = rev(cumsum(rev(Tabua$Dx[Tabua$sexo=="Masculino"])))
    Tabua$Nx[Tabua$sexo=="Feminino"] = rev(cumsum(rev(Tabua$Dx[Tabua$sexo=="Feminino"])))
    Tabua$ax = Tabua$Nx/Tabua$Dx
    return(Tabua)
  }
  
}

#Definindo pasta padrao que contem a tabua de vida
setwd("C:/Users/danil/Dropbox/Projeto Fundos Garantidores RCC/Funções no R")
#options("scipen" = 999)

#Definindo taxa de juros
TaxaJuros = 0.06

#Carregando as tabuas de mortalidade (IBGE2010)
Tabua = read.csv("TabuaMorteIBGE2010.csv",sep = ";",dec=',')
Tabua = geraTabuaComutUmaVida(Tabua,TaxaJuros)

#Definindo parametros para rodar uma execucao da funcao
Idade = 18
Sexo = "Masculino"
TabuaInvalido = Tabua[Tabua$sexo==Sexo,-2]
DiferencaIdadeMaridoEsposa = 5
DiferencaIdadePaiFilho = 30
DiferencaIdadeMaeFilho = 25
if(Sexo=="Masculino") {
  TabuaConjuge = Tabua[Tabua$sexo=="Feminino",-2]
  DiferencaIdadeConjuge = DiferencaIdadeMaridoEsposa
  DiferencaIdadePaisFilhos = DiferencaIdadePaiFilho
} else {
  TabuaConjuge = Tabua[Tabua$sexo=="Masculino",-2]
  DiferencaIdadeConjuge = -DiferencaIdadeMaridoEsposa
  DiferencaIdadePaisFilhos = DiferencaIdadeMaeFilho
}
TabuaFilho = Tabua[Tabua$sexo=="Feminino",-2]
IdadeLimite = 21
ValorBenefAnual = 13000

#Executando funcao com os parametros acima selecionados
calculaCxInvalidez(ValorBenefAnual,Idade,Sexo,TabuaInvalido,TabuaConjuge,TabuaFilho,TaxaJuros,IdadeLimite,DiferencaIdadeConjuge,DiferencaIdadePaisFilhos)
  

#Plotando grafico para todas idades do invalido
Dist = NULL
for(i in 18:max(TabuaInvalido$x)) {
  FG = calculaFGInvalidez(ValorBenefAnual,i,Sexo,TabuaInvalido,TabuaConjuge,TabuaFilho,TaxaJuros,IdadeLimite,DiferencaIdadeConjuge,DiferencaIdadePaisFilhos)
  Dist = c(Dist,FG)
}
plot(Dist,xaxt='n')
axis(1,1:length(Dist),18:(max(TabuaInvalido$x)))


