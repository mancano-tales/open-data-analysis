Os locais usados representam 108.710.471 de 118.969.906 votos do denominador nacional (91,38%), em 81.076 locais. Nesse conjunto, Lula tem 44,84% e Flávio Bolsonaro 47,29%, diferenças de -0,32 e 0,25 p.p. em relação aos percentuais nacionais de 45,16% e 47,04%.

A figura principal mantém a média simples por setor e os centis recalculados na amostra incluída. As tabelas abaixo preservam os cortes definidos antes do filtro de distância; são exercícios distintos.

**Exclusões a 5,0 km.** Decis nacionais de todos os setores com renda elegível, incluindo os sem ligação geográfica:

| Decil | Elegíveis | Além do limite | Fração | Sem local/malha |
| --- | --- | --- | --- | --- |
| 1 | 44.954 | 10.827 | 24,08% | 5 |
| 2 | 44.953 | 6.546 | 14,56% | 6 |
| 3 | 44.953 | 3.934 | 8,75% | 3 |
| 4 | 44.953 | 3.370 | 7,50% | 7 |
| 5 | 44.953 | 3.253 | 7,24% | 6 |
| 6 | 44.953 | 3.289 | 7,32% | 6 |
| 7 | 44.953 | 3.055 | 6,80% | 2 |
| 8 | 44.953 | 2.707 | 6,02% | 1 |
| 9 | 44.953 | 1.852 | 4,12% | 2 |
| 10 | 44.953 | 650 | 1,45% | 0 |

| Situação | Elegíveis | Além do limite | Fração | Sem local/malha |
| --- | --- | --- | --- | --- |
| Rural | 102.167 | 37.299 | 36,51% | 19 |
| Urbana | 347.364 | 2.184 | 0,63% | 19 |

**Cruzamentos com cortes fixos.** A primeira superação e a superação sustentada até o fim da distribuição podem diferir:

| Limite | Primeiro centil | Primeiro sustentado |
| --- | --- | --- |
| 1 km | 40 | 40 |
| 2 km | 38 | 38 |
| 5 km | 37 | 37 |
| 10 km | 37 | 37 |
| Sem limite | 37 | 37 |

**Diferenças máximas entre métodos**, no limite principal, comparando o mesmo índice de centil de cada definição:

| Referência | Comparação | Candidato | Máximo (p.p.) | Centis comparados |
| --- | --- | --- | --- | --- |
| Original reranqueada | Cortes fixos | Lula | 2,90 | 100 |
| Original reranqueada | Cortes fixos | Flávio Bolsonaro | 2,59 | 100 |
| Original reranqueada | Ponderada por V06001 | Lula | 3,48 | 100 |
| Original reranqueada | Ponderada por V06001 | Flávio Bolsonaro | 3,04 | 100 |
| Original reranqueada | Razão das somas | Lula | 3,90 | 100 |
| Original reranqueada | Razão das somas | Flávio Bolsonaro | 3,37 | 100 |
| Original reranqueada | IDW k3 | Lula | 2,52 | 100 |
| Original reranqueada | IDW k3 | Flávio Bolsonaro | 2,25 | 100 |
| Cortes fixos | IDW k3 | Lula | 0,51 | 100 |
| Cortes fixos | IDW k3 | Flávio Bolsonaro | 0,45 | 100 |

V06001 pondera pessoas responsáveis pelos domicílios, não eleitores. A razão das somas conserva os votos dos locais usados e os aloca entre setores proporcionalmente a V06001; essa aproximação não revela onde cada eleitor reside. O IDW k3 também mede proximidade, não a inscrição eleitoral real.

No bootstrap exploratório de 500 reamostragens de municípios, 500 réplicas tiveram cruzamento observado. Os quantis 2,5%, 50% e 97,5% do primeiro centil de superação foram 33,0, 36,0 e 39,0. As bandas por centil são pontuais, não simultâneas, e não medem erro amostral do Censo nem incerteza da ligação espacial.

A auditoria geométrica independente aprovou 2.000 de 2.000 setores amostrados e 2.000 verificações k3. A discordância da projeção EPSG:5880 permaneceu como diagnóstico em 502 casos, sem substituir a referência geodésica.

Foram excluídos 818 locais com coordenadas fora do respectivo município na malha de 2022, contendo 269.462 votos do denominador nacional (0,23%). Renda e malha de 2022 não captam todas as mudanças até a eleição de 2026.

V06004 considera responsáveis com rendimento. O arquivo ResponsavelRenda não fornece a contagem desse grupo; portanto não calculamos a fração de responsáveis sem rendimento. V06002 conta moradores e não permite recuperar essa fração.

**Referência visual aproximada da Folha.** O erro absoluto médio dos pontos legíveis é:

| Candidato | Pontos legíveis | MAE (p.p.) |
| --- | --- | --- |
| Flávio Bolsonaro (PL) | 6 | 0,82 |
| Lula (PT) | 7 | 1,19 |

Os pontos vêm de leitura visual manual e do tooltip da imagem, com arredondamento e regiões encobertas. Esse MAE não valida a curva completa nem demonstra equivalência ao método da Folha.
