# Métodos auxiliares da auditoria do post original

Escopo: A2/A3/A4/A7 de `AUDITORIA-2026-10-16-votacao-renda-centil.md`.
O helper não baixa dados nem executa análise nacional ao ser carregado. Não
altera o explorador ou o pipeline histórico. A figura principal e a série
original que recalcula os centis continuam sob responsabilidade do `020`.

```r
source('shared-pipeline/eleicoes/validacao/metodos-post.R', encoding='UTF-8')
# Dentro de cada município; tr e pp são sf de pontos elegíveis já auditados.
links3 <- post_k_nearest(tr, pp) # default algorithm='s2'
# Após reunir TODOS os municípios, antes de cortes de distância:
result <- post_write_methods(tracts, links3, places,
                            'data-raw/eleicoes-renda', universe='linked',
                            minimum_wage=1212)
```

## Entradas

- `tracts`: uma linha por `code_tract`, com `code_muni`, `code_state`, `V06001`,
  `V06004` e, quando disponível, `situacao`. Colunas antigas de votos/ligação
  são ignoradas, evitando conflitos em joins. Códigos numéricos de setor são
  convertidos sem notação científica.
- `links`: uma linha por par `code_tract/place_key`, com distância em metros.
  O helper ordena por distância e chave e mantém os três primeiros. Não deve
  receber apenas o vizinho mais próximo quando se pretende avaliar k3.
- `places`: uma linha por `place_key`, com `validos`, `lula`, `flavio`; os dois
  candidatos podem somar menos que válidos, pois outros ficam no denominador.
  A validade eleitoral e a elegibilidade geográfica são verificadas a montante.
- `post_k_nearest` recebe `points` sf com `code_tract/code_muni` e `places_sf`
  com `place_key/code_muni`. Nunca procura fora do município. As coordenadas
  devem ser os mesmos pontos internos e locais elegíveis do pipeline principal.

## A2: universo e limites fixos

Padrão aprovado: `universe='linked'`, todos os setores com `V06001` e `V06004`
positivos finitos e uma ligação a qualquer distância. `universe='income'` é
alternativa explícita com todos os elegíveis por renda, inclusive sem ligação.
Em ambos os casos, a ordenação é renda/código do setor, feita uma única vez.
Centil de setores: `floor(100*(i-1)/N)+1`; decil: `(centil-1)%/%10+1`.

Para 1000, 2000, 5000, 10000 e Inf metros, só muda a seleção dos observados.
Não se refaz o ranking. `renda_sm` sempre descreve o grupo completo fixo;
`renda_sm_observada` descreve somente os setores incluídos naquele cenário.
Grupos sem observações mantêm contagens zero e estimativas NA.

Exclusões são tabuladas no universo completo de renda, incluindo quem não
tem malha/local, por UF, decil nacional desse universo e situação real.
Não se presume ruralidade pela distância. O cache real de renda contém
`situacao` Urbana/Rural; sua ausência é explicitamente `nao_disponivel`.

O primeiro centil com Flávio acima de Lula e o primeiro a partir do qual a
superação se mantém até o centil 100 são distintos. Lacunas quebram uma
afirmação de superação sustentada. Registra-se também o número de intervalos.

## A3: pesos e conservação

Centil ponderado: `floor(100*(cumsum(V06001)-V06001)/sum(V06001))+1`, com limite 100.
Cada setor permanece inteiro: um setor muito grande pode pular centis e
gerar grupos vazios. Não se promete exatamente 1% do peso em cada grupo.
As médias de votação e renda usam `V06001` como peso. Trata-se de ponderação
pelas pessoas responsáveis, sem medir a população eleitoral de cada setor.

Razão das somas usa os centis fixos de SETORES, para não trocar simultaneamente
o corte e a forma de agregar. Para cada cenário, entre setores incluídos
ligados ao mesmo local, `allocation=V06001/sum(V06001)`. Cada total do local
é distribuído uma única vez. Somam-se os votos alocados e calcula-se a razão
candidato/válidos por centil. Isso conserva os totais dos locais usados, sem
contar os votos de locais não ligados e sem reconstruir onde seus eleitores
moram. É uma hipótese de alocação descritiva, não um cadastro eleitoral.

## A4: índice exato e regra alternativa

`post_k_nearest` usa `s2::s2_closest_edges`, verificado na API instalada s2 1.1.9.
Consulta inicialmente quatro locais, calcula distâncias geodésicas pareadas
e reordena por distância numérica e chave. Se o terceiro coincide com a
fronteira retornada, amplia a consulta em dobro apenas para esses setores,
até abranger a fronteira ou todos os locais. A margem de expansão
`max(1e-7 m, distancia3*1e-12)` só solicita mais candidatos: não arredonda
nem altera a ordem final. Não existe matriz nacional todos-contra-todos.
`algorithm='bruteforce'` mantém distâncias geodésicas em blocos de 64 para testes.

IDW usa os três locais mais próximos disponíveis dentro do limite, com
pesos `1/d` normalizados. Havendo distância exatamente zero, só os locais
zero recebem pesos iguais. Municípios com menos de três locais usam os
existentes. Empates são resolvidos por `place_key`. A média de percentuais
IDW é comparada à média de percentuais nearest no MESMO universo e seleção;
não substitui a alocação conservativa da série de razão das somas.

## A7: fonte primária e indisponibilidade

Dicionário específico do IBGE, atualizado em 08/05/2026:
https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/Agregados_por_Setores_Censitarios_Rendimento_do_Responsavel/dicionario_de_dados_renda_responsavel_20260508.xlsx

Conferido diretamente em 16/10/2026: 10197 bytes, MD5
`634dd1e839200c1724d6caca75626c69`, igual ao cache local consultado.
Não confundir com o dicionário genérico dos agregados, que não é dessa temática.

- V06001: pessoas responsáveis em domicílios particulares permanentes ocupados.
- V06002: moradores; V06003: variância do número de moradores.
- V06004: rendimento médio dos responsáveis COM rendimentos.
- V06005: variância desse rendimento; V06006: mediana desse rendimento.

Não existe contagem de responsáveis COM renda nessas seis variáveis. Não se
calcula `1-V06002/V06001`, nem se chama V06001 de contagem com rendimento.
O diagnóstico de A7 mantém fração NA e status explícito de indisponibilidade.
Ponderar a média V06004 por todos os responsáveis não recupera uma média
individual exata dos responsáveis com renda; é uma sensibilidade com proxy.

## Arquivos e contrato para a reprodução independente no 030

`post_write_methods` grava os seguintes CSVs, sem sobrescrever `centis.csv`:

| Arquivo | Conteúdo |
|---|---|
| universo-centis.csv | códigos, renda, V06001, situação, centil, centil_ponderado, decil |
| links-metodos.csv | code_tract, place_key, distancia_m, rank; top3 antes dos cortes |
| locais-metodos.csv | place_key, validos, lula, flavio |
| sensibilidade-cortes-fixos.csv | nearest com centis de setores fixos |
| centis-ponderados.csv | cortes e médias ponderados por V06001 |
| centis-razao.csv | razão de votos alocados, com três colunas *_alocados |
| conservacao-votos.csv | totais originais/alocados dos locais usados e erro máximo de massa |
| sensibilidade-ligacao.csv | nearest e idw3, distinguidos por regra |
| exclusoes-distancia.csv | code_state, decil, situacao, motivo, distancia_maxima_m, n_setores, peso_V06001 |
| cruzamentos-cortes-fixos.csv | primeiros centis, superação sustentada e intervalos, por limite/regra |
| diagnostico-renda.csv | fracao_sem_rendimento NA e status indisponivel_sem_contagem_de_responsaveis_com_rendimento |
| centis-originais-comparacao.csv | original reranqueada por cenário, somente para confronto |
| confronto-metodos.csv | máximo absoluto em p.p. e número de centis finitos comparados |

Séries wide: `centil,renda_sm,n_setores_total,peso_V06001_total,n_setores,`
`peso_V06001,renda_sm_observada,lula,flavio,distancia_maxima_m`.
Inf é representado literalmente como Inf no CSV. Há 100 linhas por cenário,
inclusive grupos vazios; votos ausentes são NA, não zero.

`metodos-metadata.json` registra universo, unidade monetária, definições,
fonte do dicionário, pesos, IDW, situação e indisponibilidade de A7.
O verificador independente deve reproduzir as contas dos inputs auxiliares e
auditar geometricamente uma amostra dos top3; não pode considerar o helper
como prova independente de si mesmo.

Teste pequeno, sem dados nacionais:

```powershell
Rscript shared-pipeline/eleicoes/validacao/test-metodos-post.R
```

Fixtures cobrem rankings fixos e ponderados, mudança da amostra, conservação
local, brute force de razão em R base, zeros e sete empates na fronteira k3,
equivalência índice/força bruta geodésica, municípios separados, grupos vazios,
falta de ligação, A7 indisponível e contrato de gravação. Não houve execução
nacional nesta entrega. Resultados nacionais precisam de 030 e revisão Claude.
