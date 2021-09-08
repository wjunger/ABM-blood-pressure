# define diretório dos dados
setwd('xxx\\tabelas_sim')

# carrega as funções
source('xxx\\abm_gera_pop_funcoes.r')
source('xxx\\abm_sim_funcoes.r')
require(abind)
# install.packages('abind',dep=TRUE)
                 
# carrega os dados relevantes
popsim=read.csv2('popsim.csv')
trans_np=read.csv2('trans_np.csv')
sobrevida=read.csv2('sobrevida.csv')
trans_imc=read.csv2('trans_imc.csv')
intervencao=read.csv2('interv.csv')


# source('xxx\\abm_gera_pop.r')


# parametros
nreplic=1
# critérios de parada
set.seed(500)
anos=100  # números de anos para simular
minpop=0.1  # proporção mínima da população ainda viva ou não hipertensa
idademax=85
iini=31
ifim=60
# meta
probaf=0.1
probdieta=0.1
imcmin=25
meta=0.3
tempomaximc=3
wearoff=4


sim=NULL
for (i in 1:nreplic)
{
    cat('Replicação:',i,'\n')

    # complementa o objeta da população com novas variáveis da simulação
    poprun0=cbind.data.frame(popsim,pas0=popsim$pas,idade0=popsim$idade,pad0=popsim$pad,imc0=popsim$imc,morto=0,risco=0,
                             selecao=0,ativfis=0,dieta=0,tdi=0,ured=0,tdiimc=0,cont=0)
    poprun=subset(poprun0,has!=9)
    # parâmetros operacionais da simulação
    n=dim(poprun)[1]
    
    # redefine variáveis locais
    t=1
    pop=1
    has=0
    inc0=0
    n0=0
    
    # cria um array para amazenar as parciais
    #sim=array(NA,c(nrow(poprun),ncol(poprun)),anos)
    inc_iter_i=NULL
    
    #start time
    strt=Sys.time()
    # atualização/simulação
    while ((t <= anos) & (pop >= minpop))
    {
    
        # salva a população sob risco em t0
        n0=sum(poprun[,'morto']==0 & poprun[,'has']==0)
        
        # aplica equações de atualização
        poprun=atualiza_agentes(poprun,sim_atualiza_idade,sobrevida,idademax)
        poprun=atualiza_agentes(poprun,sim_atualiza_imc,trans_imc)
        poprun=atualiza_agentes(poprun,sim_atualiza_np,trans_np)
        # determina populaçao sob risco
        poprun=atualiza_agentes(poprun,sim_atualiza_risco,imcmin,tempomaximc)
        selecao=sim_gera_selecao(poprun[,'id'],poprun[,'risco'],meta)
        poprun=atualiza_agentes(poprun,sim_atualiza_intervencao,intervencao,selecao,probaf,probdieta,imcmin,t,iini,ifim,wearoff)
        poprun=atualiza_agentes(poprun,sim_atualiza_has)
        
        # traça agente
        # traca_agente(poprun,992,c('imc'),t,anos)
        # atualiza contadores
        # atualiza tamanho da população
        pop=sum(poprun[,'morto']==0)/n
        # atualiza a incidência
        inc=sum(poprun[,'has']==1)
        has=(inc-inc0)/n0
        sim=abind(sim,poprun,along=3)
        inc_iter_i=c(inc_iter_i,has)
        inc0=inc # salva a incidência para a próxima iteração
        # imprime info
        cat('ano: ',sprintf('%03i',t),' pop: ',sprintf("%.4f", pop),'has: ',sprintf("%.4f", has),'\n')
        t=t+1    
    }
    #end time
    print(Sys.time()-strt)
    sim=rbind.data.frame(sim,inc_iter_i)
    #sim=rbind.data.frame(sim,imc)
    # write.csv2(poprun,file='poprun.csv',row.names=FALSE)
}
print('Fim!')
# converte em dataframe e renomeia.
#sim=as.data.frame(sim)
#colnames(sim)=paste('Ano',1:ifim,sep='')
#rownames(sim)=paste('Iter',1:nreplic,sep='')

# plot agentes
plot_agente(sim,992,c('imc'),list(c(20,40)))
plot_agente(sim,345,c('tdiimc'))
plot_agente(sim,152,c('pas','pad'))

#imc5=sim[3,4,]
