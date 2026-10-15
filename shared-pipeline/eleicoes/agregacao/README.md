# Agregação em centis nacionais fixos

`centis.R` não baixa dados nem modifica insumos ao ser carregado.

```r
source("shared-pipeline/eleicoes/agregacao/centis.R")
universe <- eleicao_income_universe(readRDS("data-raw/eleicoes-renda/renda2022.rds"))
dataset <- eleicao_aggregate_contest(
  votes = votos_normalizados,
  universe = universe,
  links = ligacoes_espaciais,
  year = 2026, round = 1, office = 1, uf = "BR",
  sources = list(list(name = "TSE", url = "https://dadosabertos.tse.jus.br/"))
)
item <- eleicao_write_dataset(dataset, "output/eleicoes/2026-1-1-BR.json")
```

`votes` segue o contrato de `historico/tse.R`. Pode conter linhas de seção
ou linhas já somadas por local. Precisa de `year`, `round`, `uf`,
`municipality_tse`, `zone`, `place`, `office`, `candidate_id`,
`candidate_name`, `votes` e `vote_type`. Se existir, `place_key` é utilizado;
caso contrário a chave é construída como `UF:municipality_tse:zone:place`.
Os tipos de voto aceitos são `candidate`, `party`, `special` e `invalid_code`.
Votos positivos sem classificação são recusados.

`links` contém `code_tract`, `place_key`, `distancia_m`, com uma linha por
setor; os códigos são normalizados para texto. Ligações que cruzam UFs são
recusadas. A construção espacial continua sendo responsabilidade de outro
componente, inclusive a validade do município e do ano das coordenadas.

O universo é criado uma vez a partir de **todos** os setores de renda
elegível do Censo 2022. Os centis são nacionais, com grupos iguais de setores
e desempate pelo código. Filtros de UF/cargo/ano não recalculam o ranking.
A renda de cada ponto considera o grupo completo da UF ou do Brasil,
conforme a série, antes das exclusões por distância e disponibilidade de votos.

A agregação primeiro soma os votos por local/candidato, calcula o
denominador completo do cargo/local e conta setores por local/centil.
Depois multiplica percentuais por essa contagem. Assim evita materializar
uma tabela candidato × setor. O custo ainda depende da quantidade de
candidatos e locais; para deputados deve-se processar uma UF/cargo por vez.

Presidência admite exportação nacional e por UF. Os cargos estaduais exigem
UF explícita. Locais sem votos válidos positivos para o cargo são excluídos;
um candidato ausente num local com denominador válido recebe zero.
Grupos sem cobertura recebem `votacao: null`, mantendo `n_setores: 0` e o
total de setores do universo. Percentuais de senadores são participações nos
votos válidos, sem conversão para percentual de eleitores.

Nos cargos proporcionais, legenda integra o denominador. O padrão exporta
apenas candidatos nominais; `include_party_series = TRUE` também exporta
séries dos votos dados diretamente à legenda. Essas séries não são o total
partidário, que exigiria somar votos nominais usando a filiação histórica do
cadastro de candidatos.

O JSON contém `metadata`, `candidates` e `points`, em formato de linhas.
`eleicao_write_dataset()` retorna um item de manifesto com URL, ano, turno,
código e rótulo do cargo, UF e estado `available`. O chamador reúne os itens
no manifesto completo. Nenhum item indisponível é inventado pelo exportador.

Execute a verificação com fixtures da raiz do repositório:

```sh
Rscript shared-pipeline/eleicoes/agregacao/tests.R
```

O teste compara a agregação por pesos com uma implementação independente
que cruza cada setor na fixture. Verifica também ausência versus zero,
legenda, senador, ranking fixo, preservação da renda do universo e JSON.
