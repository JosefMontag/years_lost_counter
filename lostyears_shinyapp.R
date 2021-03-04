library(shiny)
library(shinydashboard)
library(tidyverse)
library(lemon)
library(grid)

### Read data // to be replaced by reading from a remote source
lost_years <- read_csv("https://raw.github.com/JosefMontag/years_lost_counter/main/lost_years_by_risk_group.csv")
uzis_data <- read_csv("https://raw.github.com/JosefMontag/years_lost_counter/main/Input_data/covid_cases_and_deaths.csv")

uzis_data <- uzis_data %>%
  drop_na(vek_kat) %>%
  drop_na(pohlavi) %>%
  separate(vek_kat, c("died.at.age","agec2"), convert = TRUE, remove = FALSE) %>%
  mutate(
    died.at.age = floor(died.at.age + 2.5) %>% as.integer(),
    gender = ifelse(pohlavi == "M","Male","Female")
  ) %>%
  select(-pohlavi,-agec2) %>%
  mutate(umrti = ifelse(!is.na(tyden_umrti),1,NA)) %>%
  rename(death.date = tyden_umrti)

uzis_data_all <- uzis_data
uzis_data <- uzis_data %>% filter(umrti == 1)

# Mortality tables
mtab_m <- read_csv("https://raw.github.com/JosefMontag/years_lost_counter/main/Input_data/UT_Kannisto_2019M.csv") %>% mutate(gender = "Male")
mtab_f <- read_csv("https://raw.github.com/JosefMontag/years_lost_counter/main/Input_data/UT_Kannisto_2019Z.csv") %>% mutate(gender = "Female")

population <- bind_rows(mtab_f,mtab_m) %>%
  filter(age >= 10) %>%
  filter(age < 105) %>%
  mutate(
    age = cut(age,
              breaks = seq(
                from = 10, to = 105, by = 5
              ),
              labels = seq(
                from = 10, to = 100, by = 5
              ),
              include.lowest = TRUE,
              right = FALSE
    ) %>% as.character()
  ) %>%
  group_by(gender,age) %>%
  summarise(
    population = sum(Px, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    age = as.integer(age) + 2
  )


ui <- dashboardPage(
  dashboardHeader(
    title = "Odhady ztracených let života v souvislosti s COVID-19 v ČR",
    titleWidth = "700"
  ),
  dashboardSidebar(
    disable = TRUE,
    collapsed = FALSE,
    width = 600
  ),
  dashboardBody(
    tags$head(
      #tags$link('https://fonts.googleapis.com/css2?family=Open+Sans:ital,wght@1,300&display=swap'),
      #tags$style('body {font-family: "Open Sans", sans-serif;}'),
      tags$style('
      body {font-family: "Open Sans", sans-serif;}

      .box-header h3 {
                      font-family: "Open Sans", sans-serif;
                  }')
      ),
    fluidRow(
      column(4,
             box(
               p(strong("Nastavení parametru \"Riziková skupina\" určuje v kalkulaci ztracených let klíčový předpoklad o tom,
                        kdo v jednotlivých věkových kohortách typicky umírá na COVID-19."),
               "Nižší hodnoty znamenají, že na COVID-19 umírají zejména lidé na sklonku života (například vážně nemocní).
               Vyšší hodnoty znamenají, že na COVID-19 umírají lidé napříč jednotlivými kohortami
               (tj. zemřelí na COVID-19 v dané kohortě se významně neliší od přeživších). Zjednodušující veřejná diskuse o tom,
               zda lidé umírají \"s COVIDem\" nebo \"na COVID\" fakticky odpovídá porovnání extrémních vymezení
               rizikové skupiny, tj. 0 a 100 % na šoupátku níže."),
               sliderInput(
                 "risk_group",
                 "Riziková skupina (podíl věkové kohorty):",
                 min = 0,
                 max = 100,
                 value = 100,
                 width = '100%',
                 post = " %"
               ),
               p("Konstrukci rizikové skupiny lze vysvětlit takto: Seřadíme lidi z dané věkové kohorty do hypotetické \"fronty\"
                 podle jejich naděje na dožití od nejkratší po nejdelší. Parametr \"riziková skupina\"
                 potom udává předpoklad o tom, kteří lidé typicky umírají na COVID-19. Při nastavení na 100 % na
                 COVID-19 umírají lidé bez ohledu na jejich pořadí ve frontě a průměrné ztracené roky života
                 odpovídají naději na dožití průměrného člověka v kohortě. Při nižších hodnotách parametru umírají
                 typicky lidé na začátku fronty s nižší nadějí na dožití. V tom případě je smrt na COVID-19 spojena s
                 nižším počtem ztracených let života. Pokud tedy například nastavíme parametr \"riziková skupina\" na 50 %,
                 potom typická oběť COVID-19 pochází z první poloviny fronty."),
               p(
                 strong("Aplikace nemá žádné \"správné\" ani \"doporučené\" nastavení.")
               ),
               width = 12,
               title = "Kdo umírá na COVID-19? Vymezení rizikové skupiny",
               solidHeader = TRUE,
               status = "warning"
             )
      ),
      column(4,
             box(
               valueBoxOutput("mean_total", width = 12),
               valueBoxOutput("male", width = 6),
               valueBoxOutput("female", width = 6),
               p("Počet ztracených let života při úmrtí je odhadnut pro každého zemřelého na COVID-19 v
                 ČR na základě nastavení \"rizikové skupiny\".
                 Boxy ukazují hodnotu odhadu průměrného počtu ztracených let života při úmrtí na COVID-19."),
               title = "Průměrný počet ztracených let života při úmrtí v rámci vymezené rizikové skupiny",
               solidHeader = TRUE,
               status = "primary",
               width = 12
             ),
      ),
      column(4,
             box(
               valueBoxOutput("years_total", width = 12),
               valueBoxOutput("male_total", width = 6),
               valueBoxOutput("female_total", width = 6),
               p("Počet ztracených let života při úmrtí je odhadnut pro každého zemřelého na COVID-19 v
                 ČR na základě nastavení \"rizikové skupiny\".
                 Boxy ukazují celkový počet ztracených let života při úmrtí na COVID-19."),
               title = "Celkový počet ztracených let života při úmrtí v rámci vymezené rizikové skupiny",
               solidHeader = TRUE,
               status = "primary",
               width = 12
             ),
      )
    ),
    fluidRow(
      column(4,
             box(
               plotOutput("lostyears", height = "500px"),
               title = "Počet ztracených let života při úmrtí",
               solidHeader = TRUE,
               width = 12,
               status = "primary"
             )
      ),
      column(4,
             box(
               plotOutput("lostyears_mean_gender", height = "500px"),
               title = "Průměrný počet ztracených let života při úmrtí podle pohlaví",
               solidHeader = TRUE,
               width = 12,
               status = "primary"
             )
      ),
      column(4,
             box(
               plotOutput("lostyears_total_gender", height = "500px"),
               title = "Celkový počet ztracených let při úmrtí podle pohlaví",
               solidHeader = TRUE,
               width = 12,
               status = "primary"
             )
      )
    ),
    fluidRow(
      column(4,

             box(
               plotOutput("risk_group", height = "500px"),
               title = "Vymezená riziková skupina v populaci ČR",
               solidHeader = TRUE,
               width = 12,
               status = "info"
             ),
             box(
               plotOutput("COVID_demog", height = "500px"),
               title = "Hospitalizace a úmrtí podle věku a pohlaví",
               solidHeader = TRUE,
               width = 12,
               status = "info"
             )
      ),
      column(
        5,
        box(
          p(
            "Každé úmrtí zkracuje život. Tato aplikace počítá odhady ztracených let života
                                     v důsledku úmrtí na COVID-19 v České republice, přičemž umožňuje explicitně
            specifikovat ohroženou část populace v rámci jednotlivých věkových kohort (\"rizikovou skupinu\")."
          ),
          p("Aplikace počítá hypotetický počet let, kterých by se průměrný jedinec v dané
                                 rizikové skupině dožil, pokud by nezemřel na COVID-19. Z těchto hodnot
                                 jsou následně vypočteny odhady průměrných a celkových ztracených let života v důsledku úmrtí na
                                 COVID-19. Aplikace současně umožňuje sledovat, jak předpoklad o
            rizikové skupině ovlivňuje odhady ztracených let života."),
          h3("Riziková skupina"),
          withMathJax(p("Rizikové skupiny jsou odhadnuty na základě úmrtnostních tabulek pro ČR a jejich
                                   interpretace je následující: Uvažujme člověka, který zemřel ve věku \\(X\\).
                                   Pokud by v tomto věku nezemřel, potom by zemřel někdy v budoucnu ve věku \\(Y\\).
                                   Rozdíl \\(Y - X\\) představuje ztracené roky života.")),
          withMathJax(p("Ztracené roky života se mezi jednotlivci liší a \\(Y\\) je z definice nepozorovatelné. Proto ho
                                   musíme odhadovat z agregovaných dat. Pokud by se zemřelý nijak
                                  systematicky nelišil od ostatních lidí v populaci, potom by \\(Y\\) odpovídalo střední délce života přeživší
                                  populace ze stejné věkové kohorty (tj. kolika let se v průměru
                                  dožijí všichni vrstevníci, kteří nezemřeli ve věku \\(X\\)), nebo-li \\(e_X\\).")),
          withMathJax(p("Předpoklad, že se zemřelí nijak neliší od zbytku kohorty však není nutně
                                  realistický. Zemřelí mohou pocházet z rizikovější skupiny a jejich
                                  průměrná očekávaná délka života \\(e_X'\\) je tedy nižší než \\(e_X\\). To implikuje nižší počet
                                  let ztracených v důsledku smrti, než je průměr kohorty, \\((e_X' - X) < (e_X - X)\\).")),
          p("Úmrtnostní tabulky umožňují identifikaci rizikových skupin podle pravděpodobnosti úmrtí
                                  v jednotlivých následujících letech. Skupina s nejvyšším
                                  rizikem představuje podíl členů dané kohorty, která zemře během následujících dvanácti měsíců a úmrtím tedy
                                  přijde v průměru o šest měsíců života. Druhá riziková skupina představuje podíl členů kohorty,
                                  kteří zemřou během dvou let. Vedle členů první nejrizikovější skupiny tak zahrnuje tedy i jedince, kteří
                                  přijdou o 1,5 roku života. Poslední riziková skupina
                                  zahrnuje všechny členy kohorty, kteří se dožijí až 105 let (nejvyšší věk v úmrtnostních tabulkách)."),
          p("Zjednodušující veřejná diskuse o tom, zda lidé umírají \"s COVIDem\" nebo \"na COVID\"
                                 fakticky odpovídá porovnání extrémních vymezení rizikových skupin. První zmíněná interpretace řadí
                                 zemřelé na COVID-19 do nejvíce rizikové skupiny. Druhá interpretace naopak předpokládá, že zemřelí na
                                 COVID-19 se od zbytku kohorty nijak neliší."),
          h3("Možná zkreslení"),
          p("Detaily výpočtu ztracených let života, definice rizikových skupin a způsob přiřazení konkrétního věku při úmrtí
                                   k jednotlivým věkovým kategoriím vytváří tendenci k mírnému nadhodnocení odhadů
                                   ztracených let života v rámci dané rizikové kategorie. Konkrétně jde o tato zkreslení:"),
          tags$ul(
            tags$li("Minimální počet ztracených let života je 0,5. To je hodnota ztracených let života u
                                             nejrizikovějších skupin v rámci jednotlivých věkových kategorií."),
            tags$li("Vymezení rizikové skupiny je v aplikaci nastaveno tak, že zahrnuje rizikové kategorie spadající
                                     pod dané omezení plus nejbližší vyšší. Například 22,9 % mužů ve věku 92 let
                                     má očekávanou délku života půl roku. Omezení na rizikovou skupinu 10 % nejohroženějších bude v této
                                     kohortě prakticky zahrnovat 22,9 % nejohroženějších."),
            tags$li("Protože data ÚZIS neobsahují specifický věk při úmrtí, ale pouze pětileté věkové kategorie,
                                             díváme se na tato úmrtí jako na úmrtí v druhém roce příslušného intervalu. Protože smrtnost
                                             COVID-19 má strmý věkový gradient, skutečný věk při úmrtí se bude spíše nacházet ve druhé polovině intervalu.")
          ),
          p("Odhadovaný celkový počet ztracených let je naopak podhodnocen tím, že evidence ÚZIS nezahrnuje nediagnostikovaná úmrtí na COVID-19."),
          h3("Vstupní data a replikační balíček"),
          tags$ul(
            tags$li(a("Úmrtnostní tabulky ČSÚ za rok 2019.",href = "https://www.czso.cz/csu/czso/umrtnostni_tabulky")),
            tags$li("Datová sada ÚZIS o hospitalizovaných s onemocněním COVID-19, o přístup lze ",a("žádat.",href = "https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19"),
                    "Data v aplikaci jsou aktualizována každý den.")
          ),
          p("Repozitář na ",a("github.com/JosefMontag/years_lost_counter", href = "https://github.com/JosefMontag/years_lost_counter"),
            " obsahuje kompletní balíček pro replikaci výpočtů včetně výpočetního skriptu, vstupních dat, dokumentace, výsledných
                                   tabulek s odhady ztracených let života pro jednotlivé rizikové skupiny a zdrojového kódu této aplikace."),
          title = "O aplikaci",
          solidHeader = TRUE,
          status = "info",
          width = 12
        )
      ),
      column(3,
             box(
               p(
                 strong("Aplikace přímo nezohledňuje osobní charakteristiky a zdravotní stav lidí, kteří v České republice zemřeli na COVID-19.
                                      Taková data nemáme k dispozici. Aplikace zohledňuje osobní charakteristiky a zdravotní stav pouze
                                      nepřímo - omezením osob, které
                                      mohou umírat s diagnostikovaným COVID-19 na určitou rizikovou skupinu.")
               ),
               p(
                 strong("Aplikace tedy nemá žádné \"správné\" ani \"doporučené\" nastavení.")
               ),
               width = 12,
               title = "Upozornění",
               solidHeader = TRUE,
               status = "danger"
             ),
             box(
               p(strong("Štěpán Mikula"), " Katedra ekonomie, ESF MUNI"),
               p("Zpětnou vazbu k aplikaci a vizulizacím směřujte na email ",em("stepan.mikula@econ.muni.cz.")),
               p(strong("Josef Montag"), " Katedra národního hospodářství, Právnická fakulta UK"),
               p("Zpětnou vazbu, kritiku, či návrhy na vylepšení výpočtů směřujte na email ", em("josef.montag@gmail.com.")),
               width = 12,
               title = "Autoři",
               solidHeader = TRUE,
               status = "info"
             )

      )
    )
  )
)

server <- function(input, output) {

  get_riskgroup_lostyears <- reactive({
    lost_years %>%
      group_by(gender,died.at.age) %>%
      arrange(risk.group, .by_group = TRUE) %>%
      mutate(
        keep = risk.group <= (input$risk_group/100),
        keep = ifelse(row_number() == 1,
                      TRUE, lag(keep))
      ) %>%
      filter(keep) %>%
      select(-keep) %>%
      slice_max(risk.group, n=1L) %>%
      ungroup() %>%
      select(gender,died.at.age,years.lost.by.risk.group)
  })

  get_riskgroup_share <- reactive({
    lost_years %>%
      group_by(gender,died.at.age) %>%
      arrange(risk.group, .by_group = TRUE) %>%
      mutate(
        keep = risk.group <= (input$risk_group/100),
        keep = ifelse(row_number() == 1,
                      TRUE, lag(keep))
      ) %>%
      filter(keep) %>%
      select(-keep) %>%
      slice_max(risk.group, n=1L) %>%
      ungroup() %>%
      select(gender,died.at.age,risk.group)
  })

  get_benchmark_lostyears <- reactive({
    lost_years %>%
      #filter(risk.group <= input$risk_group) %>%
      group_by(gender,died.at.age) %>%
      slice_max(risk.group, n=1L) %>%
      ungroup() %>%
      select(gender,died.at.age,years.lost.by.risk.group)
  })

  get_riskgroup_data <- reactive({
    lost_years %>%
      group_by(gender,died.at.age) %>%
      arrange(risk.group, .by_group = TRUE) %>%
      mutate(
        keep = risk.group <= (input$risk_group/100),
        keep = ifelse(row_number() == 1,
                      TRUE, lag(keep))
      ) %>%
      filter(keep) %>%
      select(-keep) %>%
      slice_max(risk.group, n=1L) %>%
      ungroup() %>%
      select(gender,died.at.age,years.lost.by.risk.group) %>%
      left_join(uzis_data,., by = c("died.at.age", "gender"))  %>%
      select(gender, died.at.age, years.lost.by.risk.group, death.date) %>%
      drop_na()
  })

  get_benchmark_data <- reactive({
    lost_years %>%
      #filter(risk.group <= input$risk_group) %>%
      group_by(gender,died.at.age) %>%
      slice_max(risk.group, n=1L) %>%
      ungroup() %>%
      select(gender,died.at.age,years.lost.by.risk.group) %>%
      left_join(uzis_data,., by = c("died.at.age", "gender"))  %>%
      select(gender, died.at.age, years.lost.by.risk.group, death.date) %>%
      drop_na()
  })

  output$mean_total <- renderValueBox({

    males_myl <- get_riskgroup_data() %>%
      #filter(gender == "Male") %>%
      pull(years.lost.by.risk.group) %>%
      mean(na.rm = TRUE) %>%
      format(
        digits = 1,
        nsmall = 1,
        decimal.mark = ",",
        trim = TRUE,
        scientific = FALSE
      )

    valueBox(
      males_myl, "Let",
      color = "blue",
      icon = icon("users")
    )
  })

  output$male <- renderValueBox({

    males_myl <- get_riskgroup_data() %>%
      filter(gender == "Male") %>%
      pull(years.lost.by.risk.group) %>%
      mean(na.rm = TRUE) %>%
      format(
        digits = 1,
        nsmall = 1,
        decimal.mark = ",",
        trim = TRUE,
        scientific = FALSE
      )

    valueBox(
      males_myl, "Muži",
      color = "olive",
      icon = icon("mars")
    )
  })

  output$years_total <- renderValueBox({

    males_myl <- get_riskgroup_data() %>%
      pull(years.lost.by.risk.group) %>%
      sum(na.rm = TRUE) %>%
      sum(na.rm = TRUE) %>%
      format(
        digits = 1,
        nsmall = 0,
        decimal.mark = ",",
        big.mark = " ",
        trim = TRUE,
        scientific = FALSE
      )

    valueBox(
      males_myl, "Let",
      color = "blue",
      icon = icon("users")
    )
  })

  output$male_total <- renderValueBox({

    males_myl <- get_riskgroup_data() %>%
      filter(gender == "Male") %>%
      pull(years.lost.by.risk.group) %>%
      sum(na.rm = TRUE) %>%
      sum(na.rm = TRUE) %>%
      format(
        digits = 1,
        nsmall = 0,
        decimal.mark = ",",
        big.mark = " ",
        trim = TRUE,
        scientific = FALSE
      )

    valueBox(
      males_myl, "Muži",
      color = "olive",
      icon = icon("mars")
    )
  })

  output$female <- renderValueBox({

    males_fyl <- get_riskgroup_data() %>%
      filter(gender == "Female") %>%
      pull(years.lost.by.risk.group) %>%
      mean(na.rm = TRUE) %>%
      format(
        digits = 1,
        nsmall = 1,
        decimal.mark = ",",
        trim = TRUE,
        scientific = FALSE
      )

    valueBox(
      males_fyl, "Ženy",
      color = "orange",
      icon = icon("venus")
    )
  })

  output$female_total <- renderValueBox({

    males_fyl <- get_riskgroup_data() %>%
      filter(gender == "Female") %>%
      pull(years.lost.by.risk.group) %>%
      sum(na.rm = TRUE) %>%
      format(
        digits = 1,
        nsmall = 0,
        decimal.mark = ",",
        big.mark = " ",
        trim = TRUE,
        scientific = FALSE
      )

    valueBox(
      males_fyl, "Ženy",
      color = "orange",
      icon = icon("venus")
    )
  })

  output$lostyears <- renderPlot({

    labs <- uzis_data_all %>%
      select(vek_kat) %>%
      distinct() %>%
      separate(vek_kat,c("age","age2"), remove = FALSE, convert = TRUE) %>%
      mutate(age = age + 2) %>%
      arrange(age) %>%
      drop_na() %>%
      filter(age >= min(lost_years$died.at.age))

    left_join(
      get_riskgroup_lostyears() %>%
        select(gender, died.at.age, pn = years.lost.by.risk.group),
      get_benchmark_lostyears() %>%
        select(gender, died.at.age, bn = years.lost.by.risk.group),
      by = c("gender", "died.at.age")
    ) %>%
      filter(died.at.age %in% labs$age) %>%
      pivot_longer(-c(gender,died.at.age)) %>%
      mutate(
        value = ifelse(gender == "Male", -1*value, value),
        died.at.age = died.at.age %>%
          as.integer %>%
          factor(
            levels = labs$age,
            labels = labs$vek_kat
          )
      ) %>%
      mutate(
        category = str_c(gender,"_",name)
      ) %>%
      ggplot(
        aes(x = died.at.age, y = value, fill = category)
      ) +
      geom_col(
        position = "identity"
      ) +
      scale_y_symmetric(labels = abs) +
      scale_fill_manual(
        "Pohlaví a riziková skupina",
        values = c("#567f6dff","#ff851bff","#567f6d7d","#ff851b7d"),
        limits = c("Male_pn","Female_pn","Male_bn","Female_bn"),
        labels = c(str_c("Muži, vymezená riziková skupina: ",input$risk_group," %"),
                   str_c("Ženy, vymezená riziková skupina: ",input$risk_group," %"),
                   "Muži celkem (riziková skupina 100 %)",
                   "Ženy celkem (riziková skupina 100 %)"),
        guide = guide_legend(nrow=4,byrow=TRUE)
      ) +
      coord_flip() +
      theme_bw(
        base_size = 15
      ) +
      theme(
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        axis.title = element_blank(),
        legend.box = "vertical",
        legend.title = element_blank()
      )
  })

  output$lostyears_total_gender <- renderPlot({

    pn <- get_riskgroup_data() %>%
      group_by(death.date,gender) %>%
      summarise(
        years.lost.by.risk.group = sum(years.lost.by.risk.group),
        .groups = "drop"
      ) %>%
      group_by(gender) %>%
      arrange(death.date) %>%
      mutate(
        pn = cumsum(years.lost.by.risk.group)
      ) %>%
      select(gender,death.date,pn) %>%
      ungroup()

    bn <- get_benchmark_data() %>%
      group_by(death.date,gender) %>%
      summarise(
        years.lost.by.risk.group = sum(years.lost.by.risk.group),
        .groups = "drop"
      ) %>%
      group_by(gender) %>%
      arrange(death.date) %>%
      mutate(
        bn = cumsum(years.lost.by.risk.group)
      ) %>%
      select(gender,death.date,bn) %>%
      ungroup()

    plot_lines <- left_join(pn,bn, by = c("gender", "death.date")) %>%
      pivot_longer(-c(death.date,gender)) %>%
      mutate(
        gender = factor(gender, levels = c("Male","Female")),
        name = factor(name, levels = c("bn","pn"))
      ) %>%
      mutate(
        total = value/1000
      )

    plot_lines %>%
      mutate(
        category = str_c(gender,"_",name)
      ) %>%
      ggplot(
        aes(x = death.date, y = total, color = category)
      ) +
      geom_line(
        size = 1.5
      ) +
      scale_color_manual(
        "Pohlaví a riziková skupina",
        values = c("#567f6dff","#ff851bff","#567f6d7d","#ff851b7d"),
        limits = c("Male_pn","Female_pn","Male_bn","Female_bn"),
        labels = c(str_c("Muži, vymezená riziková skupina: ",input$risk_group," %"),
                   str_c("Ženy, vymezená riziková skupina: ",input$risk_group," %"),
                   "Muži celkem (riziková skupina 100 %)",
                   "Ženy celkem (riziková skupina 100 %)"),
        guide = guide_legend(nrow=4,byrow=TRUE)
      ) +
      scale_x_date(
        "Týden a rok",
        date_labels = "%U/%Y"
      ) +
      scale_y_continuous("Kumulativní počet ztracených let (tis.)") +
      theme_bw(
        base_size = 15
      ) +
      theme(
        legend.position = "bottom",
        strip.background = element_blank(),
        legend.box = "vertical",
        legend.title = element_blank()
      )
  })

  output$lostyears_mean_gender <- renderPlot({

    bn <- get_benchmark_data() %>%
      group_by(gender) %>%
      arrange(death.date, .by_group = TRUE) %>%
      mutate(
        mean = cummean(years.lost.by.risk.group)
      ) %>%
      group_by(gender, death.date) %>%
      slice_tail(n=1L) %>%
      ungroup() %>%
      mutate(
        gender = factor(gender, levels = c("Male","Female")),
        name = "bn"
      )

    get_riskgroup_data() %>%
      group_by(gender) %>%
      arrange(death.date, .by_group = TRUE) %>%
      mutate(
        mean = cummean(years.lost.by.risk.group)
      ) %>%
      group_by(gender, death.date) %>%
      slice_tail(n=1L) %>%
      ungroup() %>%
      mutate(
        gender = factor(gender, levels = c("Male","Female")),
        name = "pn"
      ) %>%
      bind_rows(.,bn) %>%
      filter(death.date > as.Date("2020-04-13")) %>%
      mutate(
        category = str_c(gender,"_",name)
      ) %>%
      ggplot(
        aes(x = death.date, y = mean, color = category)
      ) +
      geom_line(
        size = 1.5
      ) +
      scale_x_date(
        "Týden a rok",
        date_labels = "%U/%Y"
      ) +
      scale_y_continuous("Kumulativní průměr ztracených let") +
      scale_color_manual(
        "Pohlaví a riziková skupina",
        values = c("#567f6dff","#ff851bff","#567f6d7d","#ff851b7d"),
        limits = c("Male_pn","Female_pn","Male_bn","Female_bn"),
        labels = c(str_c("Muži, vymezená riziková skupina: ",input$risk_group," %"),
                   str_c("Ženy, vymezená riziková skupina: ",input$risk_group," %"),
                   "Muži celkem (riziková skupina 100 %)",
                   "Ženy celkem (riziková skupina 100 %)"),
        guide = guide_legend(nrow=4,byrow=TRUE)
      ) +
      theme_bw(
        base_size = 15
      ) +
      theme(
        legend.position = "bottom",
        strip.background = element_blank(),
        legend.box = "vertical",
        legend.title = element_blank()
      )

  })

  output$COVID_demog <- renderPlot({

    labs <- uzis_data_all %>%
      select(vek_kat) %>%
      distinct() %>%
      separate(vek_kat,c("age","age2"), remove = FALSE, convert = TRUE) %>%
      arrange(age) %>%
      drop_na() %>%
      filter(age >= min(lost_years$died.at.age)-2)

    uzis_data_all %>%
      group_by(vek_kat,gender,umrti) %>%
      summarise(
        obs = n(),
        .groups = "drop"
      ) %>%
      ungroup() %>%
      separate(vek_kat,c("age","age2")) %>%
      replace_na(list(umrti = 0)) %>%
      select(-age2) %>%
      mutate(umrti = umrti == 1) %>%
      drop_na() %>%
      mutate(
        obs = ifelse(gender == "Male",-1*obs, obs),
        age = as.integer(age) %>% factor(
          levels = labs$age,
          labels = labs$vek_kat
        ),
        fill_lab = str_c(gender,"_",umrti)
      ) %>%
      drop_na() %>%
      ggplot(
        aes(x = age, y = obs, fill = fill_lab)
      ) +
      geom_col() +
      scale_y_symmetric(labels = abs) +
      scale_fill_manual(
        "Pohlaví a riziková skupina",
        values = c("#567f6d7d","#567f6dff","#ff851b7d","#ff851bff"),
        limits = c("Male_FALSE","Male_TRUE","Female_FALSE","Female_TRUE"),
        labels = c("Muži, pouze hospitalizace","Muži, úmrtí",
                   "Ženy, pouze hospitalizace","Ženy, úmrtí"),
        guide = guide_legend(nrow=4,byrow=TRUE)
      ) +
      coord_flip() +
      theme_bw(
        base_size = 15
      ) +
      theme(
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        axis.title = element_blank(),
        legend.box = "vertical",
        legend.title = element_blank()
      )

  })

  output$risk_group <- renderPlot({

    labs <- uzis_data_all %>%
      select(vek_kat) %>%
      distinct() %>%
      separate(vek_kat,c("age","age2"), remove = FALSE, convert = TRUE) %>%
      mutate(age = age + 2) %>%
      arrange(age) %>%
      drop_na() %>%
      filter(age >= min(lost_years$died.at.age))

    population %>%
      mutate(
        population = ifelse(gender == "Male", -1*population, population),
        vek_kat = factor(age, levels = labs$age, labels = labs$vek_kat),
        gender = factor(gender, levels = c("Male","Female"))
      ) %>%
      left_join(., get_riskgroup_share(), by = c("gender" = "gender", "age" = "died.at.age")) %>%
      mutate(
        population.in.risk = population*risk.group
      ) %>%
      select(vek_kat,gender,starts_with("population")) %>%
      pivot_longer(-c(vek_kat,gender)) %>%
      mutate(
        category = str_c(gender,"_",name)
      ) %>%
      ggplot(
        aes(x = vek_kat, y = value/1000, fill = category)
      ) +
      geom_col(
        position = "identity"
      ) +
      coord_flip() +
      scale_y_symmetric(labels = function(x){
        y <- abs(x)
        y[x != 0] <- str_c(y[x!=0]," tisíc")
        return(y)
      } ) +
      scale_fill_manual(
        "Pohlaví a riziková skupina",
        values = c("#567f6dff","#ff851bff","#567f6d7d","#ff851b7d"),
        limits = c("Male_population.in.risk","Female_population.in.risk","Male_population","Female_population"),
        labels = c(str_c("Muži, vymezená riziková skupina: ",input$risk_group," %"),
                   str_c("Ženy, vymezená riziková skupina: ",input$risk_group," %"),
                   "Muži celkem (riziková skupina 100 %)",
                   "Ženy celkem (riziková skupina 100 %)"),
        guide = guide_legend(nrow=4,byrow=TRUE)
      ) +
      theme_bw(
        base_size = 15
      ) +
      theme(
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        axis.title = element_blank(),
        legend.box = "vertical",
        legend.title = element_blank()
      )

  })

}

shinyApp(ui, server)
