# Analise de intervencoes de transporte na acessibilidade

A **acessibilidade urbana** mede o potencial de interação das localidades com as atividades no ambiente urbano. A forma mais comum de medir acessibilidade é através de uma medida cumulativa, que calcula quantas oportunidades (empregos, hospitais, escolas) podem ser alcançadas por um modo de transporte em dado tempo de viagem.

Para calcular a acessibilidade, vc precisa desses dois componentes: a parte relacionado ao transporte (tempo de viagem) e a relaciona as atividades (quantidade de empregos, escolas no territorio etc)

Para avalir o impacto de alguma intervenção de transporte, a gente propõe calcular a acessibilidade antes e depois da intervenção e fazer uma comparação, avaliando as localidades que mais tiveram mudança de acessibilidade.

Esses cenários antes e depois, no caso de transporte público, são incorporados nos dados de GTFS. Vamos ter um arquivo de GTFS pra representar o antes e outro GTFS pra representar o depois.

As etapas do código então são:

Parte 1: fazer as mudanças necessárias no GTFS pra criar o cenário de intervenção
Parte 2: calcular as matrizes de tempo de viagem antes e depois
Parte 3: calcular a acessibilidade antes e depois
Parte 4: comparar a acessibilidade
