Avaliação I
================
Bruno Barros dos Passos
</br> 08/05/2021 Estat 2020.1

------------------------------------------------------------------------

**Questão 01.**

``` r
dados_prm <- read_csv("dados/brutos/peixes_rio_madeira.csv")
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   .default = col_character(),
    ##   id = col_double(),
    ##   data = col_datetime(format = ""),
    ##   mes = col_double(),
    ##   ano = col_double(),
    ##   local = col_double(),
    ##   cp_cm = col_double(),
    ##   peso_g = col_double()
    ## )
    ## i Use `spec()` for the full column specifications.

``` r
dados_prm %>% view()
```

**(a)**

``` r
dados_prm %>% 
  group_by(ordem) %>% 
  summarise(n = n()) %>% 
  arrange(n)
```

    ## # A tibble: 12 x 2
    ##    ordem                  n
    ##    <chr>              <int>
    ##  1 Lepidosireniformes     2
    ##  2 Pleuronectiformes      2
    ##  3 Beloniformes           5
    ##  4 Não identificado      17
    ##  5 Myliobatiformes       41
    ##  6 Osteoglossiformes    433
    ##  7 Gymnotiformes        693
    ##  8 Acanthuriformes     1602
    ##  9 Cichliformes        1947
    ## 10 Clupeiformes        2821
    ## 11 Siluriformes       27451
    ## 12 Characiformes      64356

**(b)** Characiformes. 12 observações.

**(c)**

``` r
dados_prm %>% 
  distinct(ordem)
```

    ## # A tibble: 12 x 1
    ##    ordem             
    ##    <chr>             
    ##  1 Siluriformes      
    ##  2 Characiformes     
    ##  3 Cichliformes      
    ##  4 Clupeiformes      
    ##  5 Acanthuriformes   
    ##  6 Não identificado  
    ##  7 Gymnotiformes     
    ##  8 Osteoglossiformes 
    ##  9 Myliobatiformes   
    ## 10 Beloniformes      
    ## 11 Pleuronectiformes 
    ## 12 Lepidosireniformes

``` r
dados_prm %>% 
  filter(ordem == "Não identificado")
```

    ## # A tibble: 17 x 21
    ##       id tipo_campanha campanha data                  mes   ano ciclo_hidrologi~
    ##    <dbl> <chr>         <chr>    <dttm>              <dbl> <dbl> <chr>           
    ##  1    53 Mensal        Mensal 1 2010-05-05 00:00:00     5  2010 Vazante         
    ##  2    59 Mensal        Mensal 1 2010-05-06 00:00:00     5  2010 Vazante         
    ##  3   281 Mensal        Mensal 1 2010-05-12 00:00:00     5  2010 Vazante         
    ##  4   327 Mensal        Mensal 2 2010-06-01 00:00:00     6  2010 Vazante         
    ##  5   717 Mensal        Mensal 2 2010-06-07 00:00:00     6  2010 Vazante         
    ##  6   947 Mensal        Mensal 2 2010-06-11 00:00:00     6  2010 Vazante         
    ##  7   964 Mensal        Mensal 2 2010-06-11 00:00:00     6  2010 Vazante         
    ##  8  1097 Mensal        Mensal 3 2010-07-03 00:00:00     7  2010 Seca            
    ##  9  1518 Mensal        Mensal 3 2010-07-08 00:00:00     7  2010 Seca            
    ## 10  1621 Mensal        Mensal 4 2010-08-02 00:00:00     8  2010 Seca            
    ## 11  1688 Mensal        Mensal 4 2010-08-03 00:00:00     8  2010 Seca            
    ## 12  1727 Mensal        Mensal 4 2010-08-03 00:00:00     8  2010 Seca            
    ## 13  1800 Mensal        Mensal 4 2010-08-05 00:00:00     8  2010 Seca            
    ## 14  1826 Mensal        Mensal 4 2010-08-05 00:00:00     8  2010 Seca            
    ## 15  2381 Mensal        Mensal 4 2010-08-09 00:00:00     8  2010 Seca            
    ## 16  2408 Mensal        Mensal 4 2010-08-09 00:00:00     8  2010 Seca            
    ## 17 20211 Bimestral     Bimestr~ 2012-08-01 00:00:00     8  2012 Seca            
    ## # ... with 14 more variables: bacia <chr>, ambiente <chr>, local <dbl>,
    ## #   ponto <chr>, especie <chr>, genero <chr>, familia <chr>, ordem <chr>,
    ## #   nome_comum <chr>, cp_cm <dbl>, peso_g <dbl>, sexo <chr>,
    ## #   estadio_de_maturacao <chr>, habito_alimentar <chr>

17 peixes não foram identificados na variável ordem.

**Questão 02.**

``` r
dados_prm %>% 
  select(ordem , peso_g)
```

    ## # A tibble: 99,370 x 2
    ##    ordem         peso_g
    ##    <chr>          <dbl>
    ##  1 Siluriformes      35
    ##  2 Siluriformes      35
    ##  3 Siluriformes      25
    ##  4 Characiformes    265
    ##  5 Siluriformes     160
    ##  6 Characiformes    130
    ##  7 Siluriformes      25
    ##  8 Characiformes     65
    ##  9 Cichliformes      15
    ## 10 Characiformes    250
    ## # ... with 99,360 more rows

Inicialmente, calculei a média da variável peso, ao perceber que la
dentro possui valores desconecidos. Em seguida, calculei dentre os 12
nomes de peixes encontrados na ordem, a variabilidade do peso em relação
a média, percebendo que suas médias em cada ordem eram diferentes.

``` r
dados_prm %>% 
  summarise(
    media_peso = mean(peso_g)
  )
```

    ## # A tibble: 1 x 1
    ##   media_peso
    ##        <dbl>
    ## 1         NA

``` r
dados_prm %>% 
  group_by(ordem) %>% 
  drop_na() %>% 
  summarise(mean(peso_g)) 
```

    ## # A tibble: 11 x 2
    ##    ordem             `mean(peso_g)`
    ##    <chr>                      <dbl>
    ##  1 Acanthuriformes            556. 
    ##  2 Beloniformes                13.6
    ##  3 Characiformes              165. 
    ##  4 Cichliformes               210. 
    ##  5 Clupeiformes               264. 
    ##  6 Gymnotiformes              150. 
    ##  7 Myliobatiformes           1878. 
    ##  8 Não identificado           494. 
    ##  9 Osteoglossiformes          595. 
    ## 10 Pleuronectiformes          156  
    ## 11 Siluriformes               273.

**(a)**

O coeficiente de variação é a medida de variabilidade mais adequada para
esse caso, já que analisamos que dentro de ordem ao observarmos a
variabilidade do peso em relação a média, nos apresenta que suas médias
em cada ordem são diferentes.

``` r
dados_prm %>% 
  group_by(ordem) %>% 
  drop_na() %>% 
  summarise(sd(peso_g)) 
```

    ## # A tibble: 11 x 2
    ##    ordem             `sd(peso_g)`
    ##    <chr>                    <dbl>
    ##  1 Acanthuriformes          459. 
    ##  2 Beloniformes              15.5
    ##  3 Characiformes            236. 
    ##  4 Cichliformes             283. 
    ##  5 Clupeiformes             272. 
    ##  6 Gymnotiformes            206. 
    ##  7 Myliobatiformes         1871. 
    ##  8 Não identificado        1078. 
    ##  9 Osteoglossiformes       1821. 
    ## 10 Pleuronectiformes        187. 
    ## 11 Siluriformes             933.

``` r
dados_prm %>% 
  filter(bacia == "Rio Guaporé" & peso_g) %>% 
  group_by(ordem) %>%
  summarise(
    cv = (sd(peso_g)/mean(peso_g)) * 100
  )
```

    ## # A tibble: 6 x 2
    ##   ordem              cv
    ##   <chr>           <dbl>
    ## 1 Acanthuriformes 126. 
    ## 2 Characiformes   162. 
    ## 3 Cichliformes    136. 
    ## 4 Clupeiformes     78.2
    ## 5 Gymnotiformes    99.0
    ## 6 Siluriformes    223.

**(b)** A ordem dos peixes do Rio Guaporé que possui a distribuição de
peso mais homogênea é a Clupeiformes.

**Questão 03.**

``` r
dados_prm %>%
  distinct(habito_alimentar)
```

    ## # A tibble: 5 x 1
    ##   habito_alimentar
    ##   <chr>           
    ## 1 Onívoro         
    ## 2 Carnívoro       
    ## 3 Detritívoro     
    ## 4 Herbívoro       
    ## 5 Indeterminado

``` r
dados_prm %>%
  mutate(
    sexo_recode = recode(
      sexo,
      "Fêmea"   = "Fêmea",
      "Macho" = "Macho",
      "Não coletado" = "Não coletado",
      "fêmea" = "Fêmea"
    )
  ) %>%
  group_by(sexo_recode) %>% 
  summarise(
    n = n())
```

    ## # A tibble: 3 x 2
    ##   sexo_recode      n
    ##   <chr>        <int>
    ## 1 Fêmea        28331
    ## 2 Macho        21469
    ## 3 Não coletado 49570

**(a)**

``` r
(28331 - 21469)
```

    ## [1] 6862

``` r
(6862/28331) * 100
```

    ## [1] 24.22082

O aumento será de 24.22082% da quantidade de machos para que tenha a
mesma quantidade referente às fêmeas.

**(b)**

``` r
dados_prm %>% 
  mutate(
    sexo_recode = recode(
      sexo,
      "Fêmea"   = "Fêmea",
      "Macho" = "Macho",
      "Não coletado" = "Não coletado",
      "fêmea" = "Fêmea"
    )
  ) %>%
   select(peso_g, sexo_recode) %>% 
  group_by(sexo_recode) %>% 
  drop_na() %>% 
summarise(
  n = sum(peso_g))
```

    ## # A tibble: 3 x 2
    ##   sexo_recode         n
    ##   <chr>           <dbl>
    ## 1 Fêmea        7643750.
    ## 2 Macho        4498112.
    ## 3 Não coletado 8329769.

As fêmeas possuim o maior peso.

**Questão 04.**

``` r
dados_prm %>% 
  distinct(habito_alimentar)
```

    ## # A tibble: 5 x 1
    ##   habito_alimentar
    ##   <chr>           
    ## 1 Onívoro         
    ## 2 Carnívoro       
    ## 3 Detritívoro     
    ## 4 Herbívoro       
    ## 5 Indeterminado

``` r
dados_prm %>% 
  mutate(
    sexo_recode = recode(
      sexo,
      "Fêmea"   = "Fêmea",
      "Macho" = "Macho",
      "Não coletado" = "Não coletado",
      "fêmea" = "Fêmea"
    )
  ) %>%
  filter(habito_alimentar == "Carnívoro") %>% 
  group_by(sexo_recode) %>% 
summarise(
  n = n())
```

    ## # A tibble: 3 x 2
    ##   sexo_recode      n
    ##   <chr>        <int>
    ## 1 Fêmea        13903
    ## 2 Macho         8552
    ## 3 Não coletado 11476

São 8552 carnívoros machos.

**Questão 05.**

``` r
dados_cheque <- read_csv("dados/brutos/contracheque.csv")
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   .default = col_double(),
    ##   cpf = col_logical(),
    ##   nome = col_character(),
    ##   cargo = col_character(),
    ##   lotacao = col_character(),
    ##   url = col_character(),
    ##   tribunal = col_character(),
    ##   orgao = col_character(),
    ##   data_de_publicacao = col_date(format = ""),
    ##   mesano_de_referencia = col_character()
    ## )
    ## i Use `spec()` for the full column specifications.

    ## Warning: 2629 parsing failures.
    ##   row                col   expected              actual                            file
    ## 20914 data_de_publicacao date like  2018-05-11T08:53:36 'dados/brutos/contracheque.csv'
    ## 20915 data_de_publicacao date like  2018-05-11T08:53:36 'dados/brutos/contracheque.csv'
    ## 20916 data_de_publicacao date like  2018-05-11T08:53:36 'dados/brutos/contracheque.csv'
    ## 20917 data_de_publicacao date like  2018-05-11T08:53:36 'dados/brutos/contracheque.csv'
    ## 20918 data_de_publicacao date like  2018-05-11T08:53:36 'dados/brutos/contracheque.csv'
    ## ..... .................. .......... ................... ...............................
    ## See problems(...) for more details.

``` r
dados_cheque %>% 
  glimpse()
```

    ## Rows: 161,767
    ## Columns: 22
    ## $ cpf                              <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
    ## $ nome                             <chr> "Aldir Guimarães Passarinho Junior", ~
    ## $ cargo                            <chr> "Ministro do Superior Tribunal de Jus~
    ## $ lotacao                          <chr> "Superior Tribunal de Justiça", "Gabi~
    ## $ subsidio                         <dbl> 32074.85, 32074.85, 32074.85, 32074.8~
    ## $ direitos_pessoais                <dbl> 0.00, 3528.23, 0.00, 3528.23, 621.03,~
    ## $ indenizacoes                     <dbl> 0.00, 884.00, 0.00, 884.00, 5261.73, ~
    ## $ direitos_eventuais               <dbl> 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0~
    ## $ total_de_rendimentos             <dbl> 32074.85, 36487.08, 32074.85, 36487.0~
    ## $ previdencia_publica              <dbl> 2907.19, 3528.23, 2907.19, 3528.23, 6~
    ## $ imposto_de_renda                 <dbl> 6628.15, 6980.96, 6628.15, 6980.96, 7~
    ## $ descontos_diversos               <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ retencao_por_teto_constitucional <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
    ## $ total_de_descontos               <dbl> 9535.34, 10509.19, 9535.34, 10509.19,~
    ## $ rendimento_liquido               <dbl> 22539.51, 25977.89, 22539.51, 25977.8~
    ## $ remuneracao_do_orgao_de_origem   <dbl> 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0~
    ## $ diarias                          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 4200, 4200~
    ## $ url                              <chr> "http://www.cnj.jus.br/files/conteudo~
    ## $ tribunal                         <chr> "Superior Tribunal de Justiça", "Supe~
    ## $ orgao                            <chr> "Superior Tribunal de Justiça", "Supe~
    ## $ data_de_publicacao               <date> 2018-04-24, 2018-04-24, 2018-04-24, ~
    ## $ mesano_de_referencia             <chr> "2018-4-01", "2018-4-01", "2018-4-01"~

``` r
dados_cheque %>% view()
```

``` r
dados_cheque %>% 
  select(rendimento_liquido) %>% 
  arrange(desc(rendimento_liquido))   
```

    ## # A tibble: 161,767 x 1
    ##    rendimento_liquido
    ##                 <dbl>
    ##  1           7267672.
    ##  2           2874516.
    ##  3            683094.
    ##  4            660934.
    ##  5            511424.
    ##  6            477756.
    ##  7            422214.
    ##  8            421106.
    ##  9            398141.
    ## 10            389054.
    ## # ... with 161,757 more rows

O maior rendimento líquido foi 7267672.

**Questão 06.**

``` r
dados_cheque %>% 
  distinct(cargo)
```

    ## # A tibble: 749 x 1
    ##    cargo                                   
    ##    <chr>                                   
    ##  1 Ministro do Superior Tribunal de Justiça
    ##  2 Juiz Instrutor no STJ                   
    ##  3 Juiz Auxiliar no STJ                    
    ##  4 JUIZ-AUDITOR                            
    ##  5 JUIZ-AUDITOR SUBSTITUTO                 
    ##  6 MINISTRO DO SUPERIOR TRIBUNAL MILITAR   
    ##  7 JUIZ-AUDITOR CORREGEDOR                 
    ##  8 CONSELHEIRO                             
    ##  9 JUIZ AUXILIAR                           
    ## 10 CONSELHEIRO/PRESIDENTE                  
    ## # ... with 739 more rows

``` r
dados_cheque %>% 
  filter(rendimento_liquido > 39293.32) %>%
  count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1 37334

37334 magistrados recebem acima do valor 39293.32.

**(a)**

``` r
dados_cheque %>% 
  filter(rendimento_liquido > 100000) %>%
  count()
```

    ## # A tibble: 1 x 1
    ##       n
    ##   <int>
    ## 1  1136

1136 magistrados ganham acima de R$100.000,00

``` r
dados_cheque %>% 
  distinct(tribunal)
```

    ## # A tibble: 128 x 1
    ##    tribunal                                                                
    ##    <chr>                                                                   
    ##  1 Superior Tribunal de Justiça                                            
    ##  2 Superior Tribunal Militar                                               
    ##  3 Conselho Nacional de Justiça                                            
    ##  4 Tribunal Superior do Trabalho / Conselho Superior da Justiça do Trabalho
    ##  5 Tribunal Superior Eleitoral                                             
    ##  6 Conselho da Justiça Federal                                             
    ##  7 Tribunal Regional Federal da 1a Região                                  
    ##  8 Tribunal Regional Federal da 2a Região                                  
    ##  9 Tribunal Regional Federal da 3a Região                                  
    ## 10 Tribunal Regional Federal da 4a Região                                  
    ## # ... with 118 more rows

**(b)**

``` r
dados_cheque%>% 
  summarise(
    media_rendimento = mean(rendimento_liquido)
  )
```

    ## # A tibble: 1 x 1
    ##   media_rendimento
    ##              <dbl>
    ## 1               NA

``` r
dados_cheque %>% 
  group_by(tribunal) %>% 
  select(rendimento_liquido) %>% 
  drop_na() %>% 
  summarise(
    media_rendimento = mean(rendimento_liquido))
```

    ## Adding missing grouping variables: `tribunal`

    ## # A tibble: 128 x 2
    ##    tribunal                            media_rendimento
    ##    <chr>                                          <dbl>
    ##  1 Conselho da Justiça Federal                    1328.
    ##  2 Conselho Nacional de Justiça                   6333.
    ##  3 Superior Tribunal de Justiça                  28823.
    ##  4 Superior Tribunal Militar                     30118.
    ##  5 Tribunal de Justiça da Bahia                  34935.
    ##  6 Tribunal de Justiça da Paraíba                26949.
    ##  7 Tribunal de Justiça de Alagoas                37809.
    ##  8 Tribunal de Justiça de Goiás                  30591.
    ##  9 Tribunal de Justiça de Minas Gerais           46560.
    ## 10 Tribunal de Justiça de Pernambuco             22337.
    ## # ... with 118 more rows

``` r
dados_cheque %>% 
  group_by(tribunal) %>% 
  select(rendimento_liquido) %>% 
  drop_na() %>% 
  summarise(
    dp_rendimento = sd(rendimento_liquido),
    dp_rendimento = n()) %>% 
  arrange(desc(dp_rendimento)) %>% view()
```

    ## Adding missing grouping variables: `tribunal`

``` r
dados_cheque %>% 
  group_by(tribunal) %>% 
  select(rendimento_liquido) %>% 
  drop_na() %>% 
  summarise(
    cv = (sd(rendimento_liquido)/mean(rendimento_liquido)) * 100) %>% view()
```

    ## Adding missing grouping variables: `tribunal`

``` r
dados_cheque %>% 
  group_by(tribunal) %>% 
  select(rendimento_liquido) %>% 
  drop_na() %>% 
  summarise(
    cv_rendimento = (sd(rendimento_liquido)/mean(rendimento_liquido)) * 100) %>%  
arrange(desc(cv_rendimento)) %>% view()
```

    ## Adding missing grouping variables: `tribunal`

Tribunal Regional do Trabalho da 7ª Região (CE) é o tribunal que possui
maior variabilidade.

**Questão 07.**

``` r
dados_prouni <- read_csv("dados/brutos/cursos-prouni.csv")
```

    ## 
    ## -- Column specification --------------------------------------------------------
    ## cols(
    ##   .default = col_double(),
    ##   grau = col_character(),
    ##   turno = col_character(),
    ##   curso_busca = col_character(),
    ##   cidade_busca = col_character(),
    ##   uf_busca = col_character(),
    ##   cidade_filtro = col_character(),
    ##   universidade_nome = col_character(),
    ##   campus_nome = col_character(),
    ##   nome = col_character()
    ## )
    ## i Use `spec()` for the full column specifications.

``` r
dados_prouni %>% view()
```

**(a)**

Integral.

``` r
dados_prouni %>% 
  select(nome)
```

    ## # A tibble: 41,447 x 1
    ##    nome      
    ##    <chr>     
    ##  1 Medicina  
    ##  2 Enfermagem
    ##  3 Medicina  
    ##  4 Psicologia
    ##  5 Medicina  
    ##  6 Medicina  
    ##  7 Medicina  
    ##  8 Medicina  
    ##  9 Medicina  
    ## 10 Medicina  
    ## # ... with 41,437 more rows

``` r
dados_prouni %>% 
  distinct(nome)
```

    ## # A tibble: 296 x 1
    ##    nome                    
    ##    <chr>                   
    ##  1 Medicina                
    ##  2 Enfermagem              
    ##  3 Psicologia              
    ##  4 Engenharia de Computação
    ##  5 Educação Física         
    ##  6 Direito                 
    ##  7 Engenharia de Produção  
    ##  8 Fisioterapia            
    ##  9 Administração           
    ## 10 Engenharia Civil        
    ## # ... with 286 more rows

**(b)**

``` r
dados_prouni %>% 
  group_by(turno) %>% 
  select(nota_integral_ampla) %>% 
  drop_na() %>% 
  summarise(
    media_ampla = mean(nota_integral_ampla))
```

    ## Adding missing grouping variables: `turno`

    ## # A tibble: 5 x 2
    ##   turno             media_ampla
    ##   <chr>                   <dbl>
    ## 1 Curso a Distância        545.
    ## 2 Integral                 663.
    ## 3 Matutino                 609.
    ## 4 Noturno                  602.
    ## 5 Vespertino               622.

``` r
dados_prouni %>% 
  group_by(turno) %>% 
  select(nota_integral_ampla) %>% 
  drop_na() %>% 
  summarise(
    mediana_ampla = median(nota_integral_ampla))
```

    ## Adding missing grouping variables: `turno`

    ## # A tibble: 5 x 2
    ##   turno             mediana_ampla
    ##   <chr>                     <dbl>
    ## 1 Curso a Distância          552.
    ## 2 Integral                   658.
    ## 3 Matutino                   610.
    ## 4 Noturno                    602.
    ## 5 Vespertino                 621.

A média de integral é 663. E a mediana de integral é 658.

**(c)**

``` r
dados_prouni %>% 
  group_by(turno) %>% 
  select(nota_integral_ampla) %>% 
  drop_na() %>% 
  summarise(
    dp_ampla = sd(nota_integral_ampla))
```

    ## Adding missing grouping variables: `turno`

    ## # A tibble: 5 x 2
    ##   turno             dp_ampla
    ##   <chr>                <dbl>
    ## 1 Curso a Distância     53.2
    ## 2 Integral              58.0
    ## 3 Matutino              43.5
    ## 4 Noturno               41.2
    ## 5 Vespertino            41.0

``` r
dados_prouni%>% 
  group_by(turno) %>%
  select(nota_integral_ampla) %>% 
  drop_na() %>% 
  summarise(
    cv = (sd(nota_integral_ampla)/mean(nota_integral_ampla)) * 100)
```

    ## Adding missing grouping variables: `turno`

    ## # A tibble: 5 x 2
    ##   turno                cv
    ##   <chr>             <dbl>
    ## 1 Curso a Distância  9.77
    ## 2 Integral           8.75
    ## 3 Matutino           7.14
    ## 4 Noturno            6.85
    ## 5 Vespertino         6.59

O Curso a Distância é a que possui menor homogeneidade em relação a nota
imtegral ampla, pois dentre os turnos é que possui maior porcentagem em
relação ao coeviciente de variação.

**Questão 08.**

``` r
dados_prouni %>% 
  group_by(uf_busca) %>% 
  summarise(
    n = n()
  ) %>% 
  arrange(desc(n)) 
```

    ## # A tibble: 27 x 2
    ##    uf_busca     n
    ##    <chr>    <int>
    ##  1 SP       11533
    ##  2 MG        4175
    ##  3 PR        3918
    ##  4 RS        3060
    ##  5 BA        2505
    ##  6 SC        2195
    ##  7 RJ        1442
    ##  8 GO        1278
    ##  9 PA        1201
    ## 10 PE        1148
    ## # ... with 17 more rows

O estadado da Bahia ocupa a 5ª posição.

**Questão 09.**

``` r
dados_prouni %>% 
  distinct(nome)
```

    ## # A tibble: 296 x 1
    ##    nome                    
    ##    <chr>                   
    ##  1 Medicina                
    ##  2 Enfermagem              
    ##  3 Psicologia              
    ##  4 Engenharia de Computação
    ##  5 Educação Física         
    ##  6 Direito                 
    ##  7 Engenharia de Produção  
    ##  8 Fisioterapia            
    ##  9 Administração           
    ## 10 Engenharia Civil        
    ## # ... with 286 more rows

Ao analisarmos os dados, usei a distinção para encontrar dentro da
variável nome os distintos cursos, então foram identificados 296
distintos cursos na variável nome.

``` r
dados_prouni %>% 
  group_by(nome) %>% 
  select(curso_busca) %>% 
  drop_na() %>% 
  summarise(
    n = n())
```

    ## Adding missing grouping variables: `nome`

    ## # A tibble: 296 x 2
    ##    nome                                      n
    ##    <chr>                                 <int>
    ##  1 Administração                          2754
    ##  2 Administração de Empresas                 1
    ##  3 Administração de Recursos Humanos         7
    ##  4 Administração Pública                    21
    ##  5 Agroindústria                             1
    ##  6 Agronegócio                             143
    ##  7 Agronomia                               118
    ##  8 Agropecuária                              1
    ##  9 Alimentos                                 4
    ## 10 Análise e Desenvolvimento de Sistemas  1008
    ## # ... with 286 more rows

**Questão 10.**

Ao observarmos esses gráficos, identificados diferenças em relação aos
cursos. Podemos perceber que o curso de Medicina (gráfico em azul) é um
gráfico simétrico, já que possui valores muito concentrados no centro, o
que faz com que ao calcularmos a média e a mediana seus valores são
muito próximos. Já o gráfico em vermelho que representa o curso de
Direito é um gráfico assimétrico e ao calcularmos os valores da média e
mediana eles serão distintos, sendo mais indicado usar a mediana nesses
casos, já que a ela sofre menos interferência de valores extremos.
