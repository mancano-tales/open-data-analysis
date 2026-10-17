# Resposta à auditoria do post votacao-renda-centil

Escopo: o post original em português e inglês e seu pipeline `010`–`040`.
O explorador eleitoral e a expansão histórica têm pipelines próprios e não
recebem automaticamente as novas alternativas de agregação deste post.
O post mantém `draft: true` e o aviso de interpretação vem antes da figura.

## Correções e evidências

| Item | Tratamento |
|---|---|
| A1 | `cobertura-votos.csv` e `cobertura-votos-uf.csv` contam os votos de cada local uma única vez por etapa. O total doméstico inclui votos sem localização utilizável. Informam frações de cobertura e diferenças dos percentuais de candidatos frente à referência territorial. |
| A2 | `exclusoes-distancia.csv` cruza UF, decil nacional e situação urbana/rural real do Censo. `sensibilidade-cortes-fixos.csv` mantém os cortes do universo de renda elegível com alguma ligação espacial, antes dos limites de 1/2/5/10 km e Inf. Contagens e cruzamentos acompanham cada cenário; a sensibilidade original reranqueada foi preservada. |
| A3 | `centis-ponderados.csv` usa acumulado anterior de V06001, sem dividir setores. `centis-razao.csv` aloca os votos de cada local uma única vez pelos seus setores ligados, proporcionalmente a V06001. `conservacao-votos.csv` confere os três totais por cenário. A série original permanece disponível; os grupos têm definições diferentes. |
| A4 | `sensibilidade-ligacao.csv` compara o vizinho mais próximo com até três vizinhos IDW no mesmo município, sob os mesmos cortes. Índice S2 exato, desempate por distância/chave e expansão da fronteira empatada. Distância zero recebe tratamento explícito entre os até três selecionados. O viés de cadastro/residência continua declarado. |
| A5 | `050_Verificar_Espacial_Independente.R` reconstrói pontos das malhas originais e locais elegíveis do cadastro e dos votos. Amostra fixa de 2.000 setores estratificada por UF, semente 20261016; distâncias e top3 por haversine em R base contra todos os locais do município, sem usar o índice S2 ou os pontos internos em cache. Veja a exceção de projeção abaixo. |
| A6 | `locais-fora-municipio.csv` conserva chaves, UF e votos dos locais incompatíveis; o snapshot publica resumo por UF e sua participação no total doméstico. O texto não atribui a divergência automaticamente a uma mudança de malha. |
| A7 | A fração sem rendimento permanece indisponível, com `NA` e motivo explícito em `diagnostico-renda.csv`: as seis variáveis de ResponsavelRenda não incluem a contagem de responsáveis com rendimento. V06001 conta todos os responsáveis; V06002 conta moradores. Não foi inventada uma fórmula para recuperar a quantidade ausente. |
| A8 | `referencia-folha.csv` contém sete posições escolhidas na imagem, com leituras visuais arredondadas, tolerância declarada e um tooltip textual. O ponto encoberto fica ausente. `comparacao-folha.csv` informa resíduos e MAE dos pontos legíveis, sem afirmar equivalência à curva completa ou inserir referências na série calculada. |
| A9 | 500 reamostragens de municípios, semente fixa, com todos os setores do município e centis refeitos. Réplicas, quantis e cruzamentos são preservados. Validação independente das três primeiras réplicas por expansão explícita; todos os quantis e cruzamentos são reconferidos. Bandas pontuais exploratórias, sem interpretação de erro amostral do Censo ou de incerteza espacial. |
| A10 | Ano, turno, cargo e salário mínimo censitário são parâmetros nomeados. As identidades dos candidatos 13/22 são conferidas no NM_VOTAVEL antes da agregação; 030 filtra explicitamente 2026. CSVs extraídos precisam coincidir em tamanho/CRC32 com os ZIPs atuais. |

`V06001` é um peso de responsáveis, não de eleitores. Ponderar médias
setoriais de V06004 por V06001 tampouco recupera a renda individual agregada
dos responsáveis com rendimento, cuja contagem é desconhecida.
Distribuir votos entre setores é uma hipótese espacial, mesmo conservando
os totais dos locais. Os dois limites são explicitados nas versões PT/EN.

## Exceção metodológica que requer atenção na revisão

A primeira execução do teste projetado EPSG:5880 falhou em 502 de 2.000
setores: 489 apenas por diferença de distância de pelo menos 1%, e 13 também
por mínimo projetado distinto. A maior diferença relativa foi 4,93044%.
Todos os 2.000 mínimos esféricos estavam corretos. A referência projetada
nacional não é adequada para exigir o mesmo mínimo e erro abaixo de 1%
em todo o país.

O teste mantém os campos e as divergências projetadas como diagnóstico;
a aceitação usa a fórmula esférica independente em R base, com tolerância
de 0,05 m na distância salva e verificação dos três vizinhos. A primeira
falha foi preservada no cache. A justificativa detalhada está em
`VERIFICACAO-ESPACIAL.md`. Esta mudança de referência deve ser avaliada
explicitamente pelo Claude: não houve relaxamento silencioso do limiar.

## Reprodução e publicação

A conferência adicional da validade eleitoral identificou 5.123 votos
tecnicamente nulos dentro dos números nominais do arquivo bruto. O filtro
por número inferior a 95 não estabelecia validade. O total doméstico oficial
é 118.969.906 votos válidos. A correção consulta as tabelas de candidatos e
detalhe da votação por município/zona do mesmo ano, turno e eleição, exige
validade integral ou zero e fechamento dos válidos e nulos técnicos em cada
zona. A implementação independente em R base reconstrói a classificação,
sem consultar o cache legal do explorador nem a máscara da produção.
Os quatro CSVs extraídos são vinculados aos ZIPs por tamanho e CRC32;
os ZIPs, CSVs e códigos entram nos hashes do certificado. O `040` exige
explicitamente essa nova verificação antes de aceitar o snapshot.

Snapshot corrigido validado em 17/10/2026: `020` passou nas 27 UFs, `030`
aprovou as tabelas e os 2.000 setores da amostra, e `040` reconferiu os hashes
antes de preparar o snapshot. Alguns resultados auditados:

- 108.710.471 de 118.969.906 votos válidos domésticos cobertos (91,38%),
  em 81.076 locais utilizados. Diferenças de composição: Lula −0,32 p.p.
  e Flávio +0,25 p.p. frente aos percentuais nacionais de votos.
- Exclusão por distância de 5 km: 36,51% dos setores rurais elegíveis e
  0,63% dos urbanos; 24,08% do primeiro decil e 1,45% do décimo.
- Primeira superação com cortes fixos: centis 40/38/37/37/37 sob limites
  de 1/2/5/10 km e sem limite. O centil 35 da curva original reranqueada
  representa uma definição diferente dos grupos.
- Mudança isolada de vizinho mais próximo para IDW k3, com cortes fixos:
  diferenças máximas de 0,51 p.p. (Lula) e 0,45 p.p. (Flávio) a 5 km.
- Bootstrap: quantis 2,5%/50%/97,5% do primeiro cruzamento em 33/36/39;
  todas as 500 réplicas tiveram cruzamento. Distribuição exploratória.
- Referência visual: MAE de 1,18 p.p. em sete pontos de Lula e 0,82 p.p.
  em seis pontos de Flávio, sob as limitações da leitura manual declarada.

Executar `020`, `030`, `040` e depois `posts/votacao-renda-centil/plot.R`
a partir da raiz, após os downloads de `010`. O `030` invalida qualquer
certificado anterior ao começar e só escreve `validacao-metodologica.json`
quando todas as verificações terminam. O `040` confere novamente os hashes
e prepara apenas os agregados e resumos previstos em sua lista explícita.
`publicacao-snapshot.json` registra os bytes publicados. As regras de fim
de linha em `.gitattributes` preservam esses hashes entre plataformas.

As fixtures incluem corrupção deliberada de chaves, coordenadas, distância,
um voto, CSV extraído e saída metodológica. Incluem empates na terceira
fronteira, grupos vazios e municípios de tamanhos diferentes no bootstrap.
Os gráficos são produzidos a partir dos resultados calculados; há versões
PT/EN da figura principal, dos cortes fixos, dos métodos e das bandas.
