# Auditoria do estimador de votação por renda

Esta pasta contém verificações adicionais, sem substituir os produtos existentes.
Execute da raiz, após `020_Cruzar_Setores_Votos.R`:

```r
source("shared-pipeline/eleicoes/validacao/040_Auditar_Amostra_Fixa.R")
```

Também pode ser executado com `Rscript`, com argumentos opcionais para o RDS
de entrada e a pasta de saída. O padrão grava em
`data-raw/eleicoes-renda/validacao/`; os resultados são derivados dos insumos
locais, sem novos downloads.

## O que a verificação resolve

O script atual forma centis apenas depois de excluir os setores sem ligação
espacial ou além do limite de distância. Isso faz o mesmo número de centil
representar conjuntos diferentes quando se muda a distância, o ano ou o cargo.
O novo diagnóstico fixa primeiro o ranking em todos os setores com renda
elegível do Censo 2022, com desempate pelo código do setor. Depois mede a
cobertura e calcula as médias somente dos setores observados em cada centil.
Não preenche voto ausente com zero, nem declara representatividade dos ausentes.

São produzidos:

- `sensibilidade-ranking-fixo.csv`: curvas com universo fixo e com ranking
  recalculado, cobertura de cada centil e renda média do universo e da amostra.
- `diagnostico-amostra.csv`: deslocamento dos centis, setores incluídos,
  locais distintos e concentração da quantidade de setores por local.
- `cobertura-uf.csv`: cobertura por UF e distância.

O campo `locais_equivalentes_peso_setorial` é o inverso da concentração dos
pesos dos locais (total de setores ao quadrado dividido pela soma dos
quadrados dos setores por local). Não é tamanho efetivo de amostra estatística.
Setores que recebem os mesmos votos de um local não são observações eleitorais
independentes; o total de setores não pode ser apresentado como tamanho de
amostra para intervalos de confiança de uma pesquisa.

A verificação reproduz independentemente as curvas de ranking recalculado e
as compara com `sensibilidade.csv`. Isso verifica o cálculo, sem comprovar a
validade da hipótese de proximidade.

## Requisitos para ampliar anos e cargos

1. Fixar a renda e a geografia em 2022 e descrever eleições de outros anos como
   votação histórica associada à renda territorial de 2022. Não se trata de
   renda de 2010 ou de mobilidade de renda dos mesmos eleitores.
2. Obter coordenadas e ligação seção/local correspondentes ao ano e turno.
   Reutilizar o número de local de 2026 em 2010 pode criar ligações falsas,
   mesmo que a chave tenha coincidido. Ausência de coordenadas históricas deve
   aparecer como indisponibilidade ou como aproximação temporal explicitamente
   identificada e separada da série com coordenadas contemporâneas.
3. Usar chaves que incluam eleição, turno, UF, município TSE, zona, seção e
   cargo; nomes de candidatos não identificam candidaturas entre anos. Consultar
   `SQ_CANDIDATO` e metadados de candidatos para nomes e partidos históricos.
4. Não aplicar `NR_VOTAVEL < 95` fora da presidência/governo: isso apaga
   candidatos a senador (três dígitos) e deputados (quatro/cinco). Identificar
   branco, nulo e códigos especiais pelo dicionário do arquivo e conferir
   com totais oficiais. Candidatura anulada/sub judice exige regra explícita;
   a simples presença de nome ou número não certifica voto válido totalizado.
5. Presidência deve incluir todos os candidatos válidos no denominador, mesmo
   que a tela mostre dois. Soma das séries mostradas pode ser menor que 100%.
   Segundo turno deve ser uma disputa separada, nunca somado ao primeiro.
6. Governador, senador e deputados pertencem à disputa de uma UF. Comparar
   candidatos dentro da UF. Escolher se o eixo mostra ranking nacional ou
   estadual; informar essa escolha e não trocar o universo silenciosamente.
   Distintos candidatos com número igual em UFs diferentes não são a mesma
   candidatura. Segundo turno de governador só existe nas UFs onde ocorreu.
7. Senador: a razão votos do candidato / soma dos votos válidos do cargo
   representa participação nos votos válidos. Quando há duas vagas, um eleitor
   pode registrar dois votos, portanto não é percentual de eleitores. Não
   multiplicar automaticamente por dois: votos incompletos, brancos e nulos
   impedem essa conversão. Anos de uma e duas vagas precisam de rótulo claro.
8. Deputados: manter votos nominais e votos de legenda. Para participação nos
   votos válidos do cargo, o denominador inclui ambos; soma de candidatos
   nominais fica abaixo de 100% quando há legenda. Série partidária soma votos
   nominais dos candidatos do partido e votos de legenda, com filiação do ano.
   Nome/partido ausente no cadastro é erro ou categoria auditável, nunca zero.
9. Calcular a ligação espacial por eleição independentemente da seleção de
   candidatos na tela. Selecionar candidato não pode alterar a cobertura nem
   escolher outro local mais próximo. Votação zero é válida em um local com
   denominador válido; local sem resultado de um cargo é ausência, não zero.
10. Comparações temporais precisam da cobertura por centil/UF para cada ano e
    de uma análise em setores comuns. Ranking fixo torna o eixo comparável,
    mas médias sobre amostras diferentes ainda misturam composição e mudança.

## Limites que permanecem

O local de votação mais próximo de um ponto interno do setor não identifica
onde seus moradores votam. Um setor pode conter vários locais, barreiras de
deslocamento e eleitorado que vota longe da residência. A restrição municipal
reduz erros grosseiros, mas não resolve esses casos. O limite de 5 km exclui
setores de modo seletivo, em especial rurais. Média simples das participações
setoriais dá peso igual a cada setor, e peso implícito maior aos locais
associados a mais setores; não reproduz a participação nacional de votos.

A reportagem informa grupos de igual quantidade de setores por renda. O texto
acessível não fornece código, todos os parâmetros de atribuição espacial ou
regra completa de ponderação. Assim, proximidade simples e média setorial
devem ser denominadas estimador independente. Sem esses detalhes ou os dados
agregados originais, aproximar um ponto do gráfico não prova reprodução fiel.
Não calibrar a regra para forçar coincidência com o ponto da imagem.

## Fontes consultadas

- [Reportagem original da Folha, 12/10/2026](https://www1.folha.uol.com.br/poder/2026/10/flavio-bolsonaro-supera-votos-do-pai-em-todas-as-rendas-e-lula-cresce-entre-mais-ricos.shtml):
  especifica renda dos responsáveis, Censo 2022 e grupos iguais de setores.
- [TSE: informações técnicas dos resultados](https://www.tse.jus.br/eleicoes/informacoes-tecnicas-sobre-a-divulgacao-de-resultados-2024):
  distingue votos válidos e categorias de votos não válidos; edição/ano do
  dicionário deve acompanhar cada arquivo efetivamente usado.
- [TSE: conteúdo do boletim de urna](https://www.tse.jus.br/comunicacao/noticias/2021/Agosto/auditagem-do-resultado-de-cada-votacao-e-garantida-por-meio-do-boletim-de-urna):
  identifica votos nominais, legenda, brancos, nulos e comparecimento.
- [Senado: dois votos para senador em 2026](https://www12.senado.leg.br/noticias/materias/2026/09/28/eleicoes-2026-veja-o-que-e-fato-sobre-o-voto-para-o-senado).
- [Senado: votos incompletos em eleições com duas vagas](https://www12.senado.leg.br/noticias/materias/2026/07/14/eleitor-escolhera-dois-senadores-neste-ano-veja-como-votar).
- [Portal público de dados do TSE](https://dadosabertos.tse.jus.br/): fontes
  por ano e respectivos dicionários.

Agent: Codex / modelo da sessão não identificado / desktop
