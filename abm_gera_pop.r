# define diretório dos dados
wkdir='xxx\\tabelas_pop'

# carrega as funções
source('xxx\\abm_gera_pop_funcoes.r')

# carrega os dados relevantes
pop_sexo_idade=read.csv2(paste(wkdir,'\\','pop_sexo_idade.csv',sep=''))
pop_sexo_imc=read.csv2(paste(wkdir,'\\','pop_sexo_imc.csv',sep=''))
tabag_sexo_idade=read.csv2(paste(wkdir,'\\','tabag_sexo_idade.csv',sep=''),row.names=1)
idade_limites=read.csv2(paste(wkdir,'\\','idade_limites.csv',sep=''))
imc_limites=read.csv2(paste(wkdir,'\\','imc_limites.csv',sep=''))
sexo_fe_imc_norm=read.csv2(paste(wkdir,'\\','sexo_fe_imc norm.csv',sep=''))
histfam=read.csv2(paste(wkdir,'\\','histfam.csv',sep=''))
np_sexo_idade=read.csv2(paste(wkdir,'\\','np_sexo_idade.csv',sep=''))

# criando a população
N=10000
id=gera_id(N)

# variáveis brutas
fe_sx=gera_fxetaria_sexo(id,pop_sexo_idade)
fxetaria=fe_sx$fxetaria
sexo=fe_sx$sexo

idade=gera_idade(fxetaria,idade_limites)

# pesos amostrais - manter proporções entre estratos
peso=N/table(sexo,fxetaria)

#histórico familiar
histfam=gera_histfam(id,histfam)

# imc por idade e sexo
imccat=gera_imccat(fxetaria,sexo,sexo_fe_imc_norm,peso)

# converte para numérico
imc=gera_imc(imccat,imc_limites)

# tabagismo por sexo e idade
fumante=gera_fumante(fxetaria,sexo,tabag_sexo_idade,peso)

# pressão arterial
pressao=gera_np(fxetaria,sexo,np_sexo_idade,0.75)

# juntando as variáveis
sexonum=ifelse(sexo=='M',1,2) # substitui sexo por uma variável numérica
gruposis=ifelse(pressao[,'pas']<120,119,139)
popsim=data.frame(id,sexo=sexonum,idade,imc,fumante,histfam,pressao,grupo=gruposis)
# limpeza
rm(id,sexonum,idade,imc,fumante,histfam,pressao,gruposis,fxetaria,imccat,peso,sexo,N)
rm(fe_sx,idade_limites,imc_limites,np_sexo_idade,pop_sexo_imc,pop_sexo_idade,sexo_fe_imc_norm,tabag_sexo_idade)
# salva em arquivo
# write.csv2(popsim,file='..\\tabelas_sim\\popsim.csv',row.names=FALSE)

#print('Fim!!!')







