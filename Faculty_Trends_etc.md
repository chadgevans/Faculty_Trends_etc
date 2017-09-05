Introductory Chapter
================
Chad Evans

Built with 3.3.2. Last run on 2017-09-05.

-   Configure
    -   Libraries
    -   directories
-   Munge
-   [Introduction](#introduction)
-   [Data](#data)
    -   IPEDS
    -   Survey of Doctorate Recipients
    -   HERI Faculty Survey
-   [The Rising Cost of Higher Education](#the-rising-cost-of-higher-education)
    -   How are tuition dollars allocated?
-   [The Shifting Institutional Landscape of Adjunct Work](#the-shifting-institutional-landscape-of-adjunt-work)
-   [Reining in Postsecondary Expenses](#reining-in-postsecondary-expenses)
-   [Where are the Adjuncts?](#where-are-the-adjuncts)

Configure
---------

Munge
-----

``` r
source(file.path(Munge, "02_Clean_the_data.R"))
```

Introduction
============

Decades ago, institutions of higher learning were run almost entirely by long-term faculty with tenured contracts. Over the last four decades, however, there has been a fundamental restructuring of faculty contracts in higher education. Today, the work of higher education is largely performed by non-tenured faculty, some of whom may not be around when the next semester begins. This trend is well documented. In the late 1960s, a mere 22% of faculty were tenure-ineligible (Schuster and Finkelstein 2006). By the Fall of 2009, however, nearly two-thirds of all faculty were non-tenure track (NTT). This is a dramatic change that is revolutionizing the academic workforce: fixed-term faculty are now the new faculty majority.

As the adjunct workforce expanded, research on this topic has broadened and intensified. Some have explored the reasons and consequences of growth (Cross and Goldenberg 2003, Schuster and Finkelstein 2006). Others have focused on common duties and job experiences of these faculty (Baldwin and Chronister 2001, Schell and Stock 2001). There is a line of research exploring typologies of adjunct faculty (Gappa and Leslie's 1993). Baldwin and Chronister (2001) and (Hollenshead et al. 2007) have also made valuable contributions to this line of inquiry.

While our understanding of non-tenure track faculty has grown considerably in the last decade, there is still much we do not know about these important academic laborers and the institutions that employ them. For one, non-tenured faculty have very different job experiences, working conditions, motivations, and disciplinary backgrounds. Yet, researchers commonly analyze contingent faculty as one homogeneous block. As a result, our body of knowledge is tailored to non-tenure track faculty in the aggregate, ignoring important subclasses with different work motivations and experiences. Researchers also commonly assume that adjuncts work exclusively in the domain of instruction. In this reasoning, postsecondary institutions hire adjuncts to temporarily fill in when their course registrations periodically fill up. We do not dispute that instruction is the principle activity of adjuncts. However, it is important to recognize that adjuncts often take on broader responsbilities in their institutions and some even work principally in administration and research, not instruction per se. Exploring the non-instruction contributions of non-tenure track faculty is something overlooked by most researchers. This study gives broad consideration to adjuncts of all work activities.

Another limitation of earlier research is its overwelming focus on degree-granting, non-medical institutions. Medical institutions, it is true, use non-tenure track categories of faculty in ways different from all other departments. However, as we aim to establish the growth of adjuncts across all of postsecondary institutions, we have included medical adjuncts whenever possible. We also, importantly, factor in institutions that do not grant degrees. Non-degree-granting institutions consititue a large segment of postsecondary education (about 1/3 of institutions today) and they are a key pathway to facilitate access to advanced higher education. Yet, research often fails to consider the activities and growth of adjuncts in these important institutions.

This paper principally investigates the growth of adjuncts in higher education, but it also does much more. As the employment of adjunct faculty is viewed largely as a cost-savings strategy, we examine the latest financial data to gain an understanding of how postsecondary institutions allocate resources and how these expenses have shifted over time. Next, we examine the institutional context of higher education, exploring how postsecondary institutions have evolved over the last decades. It is well-known that 2-year and community colleges have grown over the last decades (and we document these trends), but other instiutional characteristics have shifted as well and these merit further investigation. Thirdly, along various institutional and labor dimensions, we examine how the proportion of non-tenure track faculty has changed over time. In other words, we examine how the proportion of adjuncts has grown over time in different ways.

### Research Questions

In short, our main research questions center around the following themes: 1) How are expenses in higher education distributed and how has that changed over time? 2) How has the institutional landscape (and institutional characteristics) shifted over the last decades? 3) In which kinds of institutions has the growth in the proportion of non-tenure track faculty been most prominent?

Data
----

Up until 2004, postsecondary education researchers could utilize the National Study of Postscondary Faculty (NSOPF) to examine the universe of postsecondary faculty. This survey was administered every few years to a panel of postsecondary faculty. However, this instrument was terminated in 2004, making research on faculty much more difficult and less robust. Much has changed in academia over the last thirteed years, and NSOPF is no longer informative of today's conditions and trajectories in postsecondary education. The approach of our research project is to draw on three other survey instruments in hopes of recovering some of the postsecondary insights lost when NSOPF was terminated. To this end, we utilized data from the Integrated Postsecondary Education Data System (IPEDS), the Survey of Doctorate Recipients (SDR) and the Higher Educational Research Institute's (HERI) Faculty Survey.

1.  **Integrated Postsecondary Education Data System (IPEDS):** IPEDS is collected annually by the National Center for Education Statistics (NCES). Here, researchers gather information from every college, university, and technical and vocational institution that participates in the federal student financial aid programs. The survey has information on enrollment, program completions, graduation rates, faculty and staff, finances, institutional prices, and student financial aid and institutional characteristics. Available at the institutional level, this data source provides a “census” of institutions' faculty populations. There were four specific components of IPEDS that were of use to this project. The financial component allowed us to assess institutional expenditures. The admissions and test scores component allowed us to factor in institutional selectivity. The Institutional Characteristics component provided us with information on institutional structure, control and degree-granting status.
    Finally, while many resarch projects draw on the Fall staff component of IPEDS, we utilized the "Employees by Assigned Position (EAP)" component because of its broader sample of institutional employees. The Fall staff segment only contains information on instructors from degree-granting institutions with fifteen or more full-time staff, meaning that important areas and institutions of adjunct labor are necessarily excluded. The EAP component collects information from all institutional employees, regardless of institutional characterstics. Subsetting to individuals classified as "faculty," we were able to examine how adjuncts are utilized in particular institutions, like for-profit colleges and universities and non-degree granting institutions. As we believe a significant proportion of adjuncts hold non-teaching positions in varied institutional settings, it was important to keep our sample broader than earlier research, to the greatest extent possible. Another advantage of EAP is its annual collection of data. The Fall Staff instrument is only required biennially. For data reliabilty, IPEDS uses both surveys, Fall staff and EAP, to clarify ambiguities and impute values. Unfortunately, IPEDS only contains institution-level features and characteristics, limiting its capacity to address some of our research questions.

2.  **Survey of Doctorate Recipients (SDR):** The SDR is a longitudinal biennial survey (panel data) conducted since 1973. It contains demographic and career history information on individuals with a research doctoral degree in a science, technology, engineering, or mathematics (STEM) field from a U.S. academic institution. The survey follows a sample of individuals with STEM doctorates throughout their careers from the year of their degree award until age 76. Our study relies only on data from 1993 and after. This is because the SDR survey underwent significant changes in the early 1990s and many variables changed in their meaning and measurement. Furthermore, information on adjuncts was very limited before the 1990s and therefore our analysis focuses on 1993 and after.

3.  **Higher Education Research Institute (HERI) Faculty Survey:** HERI is a proprietary dataset managed by the eponymous institute at UCLA. The survey was originally designed to collect cross-sectional data on tenured and tenure-track faculty teaching undergraduate students in the United States. The first survey was issued in 1989. Since that time, however, the institute has recognized the importance of fixed-term faculty and begun to collect more detailed information on them as well. More information is also available now on community colleges, where non-tenure-track faculty are common. Despite the inclusion of substantial numbers of contingent faculty, the survey still does not randomly sample from this population and this complicates population inferences. Our HERI sample comes from 2010.

``` r
Year<-c("1980-2015","1993-2013","2010")
Design<-c("Panel","Panel","Cross-sectional")
Sample<-c("Census of Institutions","Doctorate Recipients in STEM","Postsecondary Instructors")
table<-rbind(Year,Design,Sample)
colnames(table)<-c("IPEDS","SDR","HERI")
kable(table, caption = "Survey Instruments in this Study", align="c")
```

|        |          IPEDS         |              SDR             |            HERI           |
|--------|:----------------------:|:----------------------------:|:-------------------------:|
| Year   |        1980-2015       |           1993-2013          |            2010           |
| Design |          Panel         |             Panel            |      Cross-sectional      |
| Sample | Census of Institutions | Doctorate Recipients in STEM | Postsecondary Instructors |

``` r
rm(table)
```

Analytic Strategy
-----------------

IPEDS is the best data source for institutional-level data. It contains information from every postsecondary institution (census) and it is integral in order to understand how institional expenses have changed over time and how the population of non-tenure track faculty has evolved in the aggregate. For many questions, however, institutional-level data will not do. When requiring longitudinal data measuring faculty changes over time, we will draw on SDR. As a panel dataset, SDR offers an excellent portrayal of non-tenure track faculty over the last decades. Its limitation is that it only contains information on doctorate recipients in STEM fields. As many adjuncts only hold a masters and in some cases only a bachelors, this excludes considerable numbers of non-tenure track faculty. Our inferences, then, are significantly limited to STEM PhDs working in academia. Finally, the HERI dataset allows us to understand the entire population of non-tenure track faculty. It contains instructional faculty who hold many different kinds of degrees. While very informative of the universe of adjunct faculty today, it is a cross-sectional instrument (2010) and is less informative of over-time changes occurring to faculty. In this way, the three datasets in this study each play an important role in elucidating adjunct labor. Together, they help us establish a comprehensive understanding of non-tenure track faculty.

Variables
---------

This study examines a range of institutional characteristics and the roles adjuncts fulfill in these places. We examine how the instiutional landscape has shifted over time based on institutional level (less-than\_2-year, 2-year, 4-year), control (public, private, for-profit), degree-granting status, carnegie classification by research intensity, selectivity and presence of a tenure system. We also examine the growth of adjuncts in these institution types, as well as their growth in administrative/managerial positions, clinical (medical) positions, and postdoc jobs. Finally, we examine the reasons faculty work part-time in academia (e.g., family responsibilities, retirement transition, etc.). Whenever possible, we defined these terms in accordance with the Department of Education and the National Center for Educational Statistics (NCES). Our work also draws on adjunct typologies based on the work of Gappa and Leslie (1993). For a glossary of our definitions, please see the appendix.

The Rising Cost of Higher Education
===================================

To understand the rising costs of higher education, it is important to recognize fundamental demographic changes in the United States and the increasing portion of the population now attending institutions of higher learning. In the late 1950s, there were 2.5 million students enrolled in colleges and universities full-time and 1.25 million part-time students. These figures grew rapidly since they were first collected. Today, there are around 13 million full-time students and 8 million part-time students. These figures are expected to reach 14.4 and 8.8 million, respectively, by 2024. The growth over time has been approximately linear with subtle troughs and peaks corresponding to demographic expansions (e.g. baby-boomers, millennial expansion) in the broader American population.

``` r
EnrollTab %>%
  select(Year, Fulltime,Parttime, Projection) %>%
  gather(Status, Count, 2:3) %>%
  ggplot(aes(x=Year, y=Count, group=Status, colour=Status)) + 
  geom_line() +
  geom_line(size=1) +
  labs(title="Postsecondary Enrollment", subtitle= "By Full-time and Part-time Status",x="Year", y = "Students (in Millions)") +
  labs(caption = "Evans & Furstenberg. IPEDS 1959-2026 (IES-calculated Projections).") +
  scale_color_discrete("Student Status", labels = c('Full-time','Part-time'))
```

![Increases in Student Enrollment](graphs/Student_Enrollment-1.png)

Historically, higher education has been very dependent on government support and public financial support has increased yearly for decades. However, while the government's absolute subsidization of higher education has increased over time, it has not kept up with the expansion in the student population. As a result, the per-pupil rate of government support has slowly declined. This has forced institutions to pursue cost-saving measures and alternative sources of revenue. In large part, the public has shifted the financial burden of higher education from taxpayers onto the students themselves. This is evident from the conistent increase in tuition over the last decades.

``` r
tuition_table<-tuition_table[!(tuition_table$CONTROL=="All" | tuition_table$CONTROL=="Private"),] 
#tuition_table<-tuition_table[!(tuition_table$CONTROL=="PRIVATE" & tuition_table$YEAR>=1999),] 
tuition_table %>% 
  gather(TYPE, TUITION, 2:3) %>% 
  unite("INST",c(CONTROL, TYPE)) %>%
  ggplot(aes(x=YEAR, y=TUITION, group=INST, colour=INST)) + 
  geom_line() +
  geom_line(size=1) +
  scale_color_discrete("Institution Type", labels = c('Private For-profit Four-year','Private For-profit Two-year','Private Non-profit Four-year','Private Non-profit Two-year','Public Four-year Institutions','Public Two-year Institutions')) +
  labs(title="Real Cost of Tuition and Fees", subtitle= "(in 2015-2016 dollars)",x="Year", y = "Required Tuition and Fees (in US Dollars)") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=10), legend.title=element_text(size=10)) +
  labs(caption = "Evans & Furstenberg. IPEDS 1963-2015.")
```

![Tuition Costs](graphs/Real_Tuition-1.png)

From Figure 2, it is clear that tuition expenses and fees have generally increased over time across school types. This trend is most clear for public schools and private, not-for-profit schools. The increases in tuition at public, two-year schools have been quite gradual. After adjusting for inflation, it is about twice as expensive to attend a public, two-year institution today as it did in the 1960s. The cost to attend public four-year institutions, on the other hand, has risen more rapidly. In 1963-1964, tuition and fees was just over $7,000 annually. Inflation-adjusted tuition in 2015-2016 was over $19,000 and costs have been slowly accelerating over the last two decades. These figures all correct for inflation, implying a considerable increase in the burden placed on students. Historically, many students could work side jobs and pay their public school tuition without going into debt. As tuition has increased by a factor of three, the possibility of a debt-free higher education is reserved now mostly for students from affluent families.

Tuition has also increased consistently in private, non-profit schools. In the case of four-year institutions of this type, tuition has gone up about $800 every year for the last 15 years. Today, tuition typically is close to $45,000. The cost to attend two-year institutions (private, non-profit) is more modest, but still is considerable at $25,000 a year.

The for-profit sector, however, does not have a clear trajectory with regard to tuition. Both at two-year and four-year for-profit schools, tuition has tended to fluctuate between $22,000 and $30,000 after adjusting for inflation. This pattern is fairly predictable. For-profit schools depend less on government subsidy, so their pricing is less sensitive to changes in government support. This does not explain, however, why tuition in non-profit schools continues to rise, as they also receive less government support. Further research could clarify this paradox.

How are tuition dollars allocated?
----------------------------------

Postsecondary expenses have risen rapidly over the last decades. In 1987, American institutions spent 100 billion on higher education. In 2014, they spent nearly half a trillion dollars ($488 billion). At all time points, the biggest expense has been on instruction (including salary and benefits). $156 billion was spent on instruction in 2014 or about one-third of the entire budget. Other important higher education expenses go towards instructional support, hospital services, and research. The following figure depicts the expansion and distribution of expenses over the last decades.

While expenses have grown considerably, how those expenses are distributed remains proportional from year to year. As an example, across all years, the minimum budget share spent on instruction was 29.7% in 1994. The most, as a percentage of the budget, spent on instruction was in 1997 was 31.4%. Similarly, expenses on the salaries and wages of institutional executives and administrators (of portion of institutional supports) was around 4.5-5% in all time periods, with little variation.

``` r
Delta_table %>% 
  gather(EXPENSE, DOLLARS, c(2:11)) %>% 
  ggplot(aes(x=academicyear, y=DOLLARS, colour=EXPENSE, fill=EXPENSE)) + 
  geom_col(position = "stack") +
  scale_fill_discrete(name = "Expense Type\n", labels = c("Academic support","Auxiliary enterprises","Hospital services","Independent operations","Institutional support","Instruction","Public service","Research","Net Aid/Other","Student services")) +
  labs(title = "Higher Education Expenses over Time\n", x = "Year", y = "Expense (in billions)") +
  guides(colour=FALSE) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=10), legend.title=element_text(size=10)) +
  labs(caption = "Evans & Furstenberg. IPEDS Delta Tables 1987-2013.")
```

![Higher Education Expenses Over Time](graphs/Higher_Ed_Expenses_Over_time-1.png)

``` r
table<-Delta_table %>%
  select(-academicyear) %>%
  mutate(Total=rowSums(., na.rm=T), PCT_INSTRUCT=100*instruction01/Total, Year=1987:2013) %>%
  `colnames<-`(c("Academic Support","Auxiliary Enterprises","Hospitals","Independent Operations","Instructional Support","Instruction","Public Service","Research","Student Services","Net Aid/Other","Total Expenses","Total Pct. on Teaching","Year"))
table<-table[,c(13,1:12)]
kable(table)
```

|  Year|  Academic Support|  Auxiliary Enterprises|  Hospitals|  Independent Operations|  Instructional Support|  Instruction|  Public Service|  Research|  Student Services|  Net Aid/Other|  Total Expenses|  Total Pct. on Teaching|
|-----:|-----------------:|----------------------:|----------:|-----------------------:|----------------------:|------------:|---------------:|---------:|-----------------:|--------------:|---------------:|-----------------------:|
|  1987|              7.63|                   11.3|       9.34|                    2.94|                   10.2|         33.9|            3.48|      9.39|              5.01|           15.3|             108|                    31.3|
|  1988|              8.21|                   11.7|      10.79|                    2.84|                   10.8|         36.0|            3.83|     10.39|              5.43|           16.3|             116|                    31.0|
|  1989|              8.93|                   12.7|      12.17|                    2.98|                   11.6|         38.9|            4.22|     11.39|              5.76|           17.9|             126|                    30.8|
|  1990|              9.49|                   13.5|      13.03|                    3.20|                   12.7|         42.3|            4.74|     12.55|              6.42|           19.7|             138|                    30.7|
|  1991|             10.08|                   14.6|      14.78|                    3.37|                   13.8|         45.6|            5.12|     13.49|              7.05|           21.4|             149|                    30.6|
|  1992|             10.63|                   15.4|      16.61|                    3.57|                   14.6|         48.2|            5.54|     14.31|              7.55|           23.9|             160|                    30.1|
|  1993|             11.15|                   16.0|      17.74|                    3.67|                   15.4|         50.5|            5.98|     15.35|              8.20|           25.9|             170|                    29.7|
|  1994|             11.76|                   16.9|      18.50|                    3.39|                   16.0|         53.0|            6.29|     16.19|              8.60|           27.5|             178|                    29.7|
|  1995|             12.34|                   17.7|      18.67|                    3.54|                   17.0|         55.9|            6.74|     17.18|              9.08|           28.9|             187|                    29.9|
|  1996|             13.66|                   18.0|      17.90|                    1.82|                   18.4|         57.7|            6.93|     17.65|              9.51|           31.0|             193|                    30.0|
|  1997|             13.94|                   19.2|      16.74|                    1.26|                   18.8|         59.4|            7.31|     18.89|             10.08|           23.4|             189|                    31.4|
|  1998|             15.52|                   20.5|      20.50|                    2.11|                   21.0|         64.0|            7.71|     20.21|             11.21|           24.2|             207|                    30.9|
|  1999|             17.12|                   21.7|      21.62|                    3.28|                   22.8|         68.0|            8.29|     21.69|             12.19|           25.7|             222|                    30.6|
|  2000|             18.14|                   23.1|      24.18|                    3.32|                   23.8|         74.5|            8.88|     23.96|             13.68|           35.7|             249|                    29.9|
|  2001|             20.19|                   25.9|      24.28|                    4.10|                   26.5|         81.1|            9.84|     26.80|             14.91|           35.5|             269|                    30.1|
|  2002|             20.03|                   23.7|      24.57|                    3.90|                   27.1|         84.1|           10.13|     28.10|             15.93|           43.7|             281|                    29.9|
|  2003|             20.65|                   25.0|      25.90|                    4.49|                   28.7|         89.6|           10.48|     30.72|             16.94|           45.2|             298|                    30.1|
|  2004|             21.90|                   26.1|      26.58|                    4.98|                   30.2|         93.0|           10.90|     32.82|             18.07|           41.8|             306|                    30.4|
|  2005|             23.00|                   27.4|      29.22|                    4.88|                   31.5|         98.1|           11.50|     34.82|             19.48|           42.5|             322|                    30.4|
|  2006|             24.80|                   28.8|      30.27|                    4.95|                   33.4|        103.8|           11.71|     35.66|             21.11|           46.6|             341|                    30.4|
|  2007|             26.37|                   30.8|      32.45|                    5.52|                   35.9|        111.3|           12.21|     36.90|             22.80|           45.8|             360|                    30.9|
|  2008|             29.13|                   33.6|      35.04|                    5.84|                   39.7|        119.2|           13.11|     39.36|             24.68|           52.3|             392|                    30.4|
|  2009|             30.73|                   35.2|      38.28|                    6.36|                   41.7|        125.0|           13.73|     41.53|             26.49|           54.5|             413|                    30.2|
|  2010|             32.17|                   38.3|      41.28|                    6.45|                   42.3|        131.5|           14.17|     45.09|             28.25|           54.8|             434|                    30.3|
|  2011|             33.28|                   40.3|      43.71|                    6.59|                   44.2|        137.8|           14.72|     47.70|             29.92|           58.7|             457|                    30.2|
|  2012|             34.91|                   41.4|      48.10|                    6.72|                   45.4|        142.1|           14.91|     48.16|             31.56|           60.2|             474|                    30.0|
|  2013|             36.55|                   42.2|      50.42|                    6.73|                   47.2|        145.7|           14.85|     48.36|             32.45|           59.1|             484|                    30.1|

    ## Warning: Unknown or uninitialised column: 'PCT_INSTRUCT'.

    ## # A tibble: 0 x 1
    ## # ... with 1 variables: Year <int>

    ## Warning: Unknown or uninitialised column: 'PCT_INSTRUCT'.

    ## # A tibble: 0 x 1
    ## # ... with 1 variables: Year <int>

``` r
expensedata %>% 
  ggplot(aes(x=EXPENSE, y=ALL)) + 
  geom_bar(stat="identity", fill='dodgerblue4') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Expenses in Higher Education in 2014", x = "Expense Type", y = "Expense (in billions)") +
  labs(caption = "Evans & Furstenberg. NCES IPEDS Finance Component 2014.") +
  theme(panel.background = element_rect(fill = 'aliceblue', colour = 'black'))
```

![Distribution of Higher Education Expenses in 2014](graphs/Higher_Ed_Expenses_2014-1.png)

Public and Private institutions allocate resources in similar ways. In both cases, instruction is far and away the most sizeable cost. Public institutions, however, spend a bit more on public service and student aid. Private institutions channel a higher proportion of resources towards independent operations and institutional support. While the disribution of expenses is fairly similar, the absolute expenditures are different. Private institutions spend considerably less in the broader scope of higher education. This may be because public instutions educate greater numbers of students overall (and thus must spend more to accomplish this). Potentially, however, private institutions may be more efficient. Further research should clarify this. In any case, public institutions spent 315 billion overall and private institutions spent 173 billion in 2014.

``` r
expensedata[-1] <-expensedata[-1] %>% 
  as.matrix() %>% 
  prop.table(margin = 2) %>%
  `*`(100) %>%
  round(1) #%>%
#  `colnames<-`(c("Expense Type","Public Institutions","Private Institutions","All Institutions"))
kable(expensedata, caption = "Distribution of budget expenses in Public and Private colleges and universities in 2014")
```

| EXPENSE                       |  PUBLIC|  PRIVATE|   ALL|
|:------------------------------|-------:|--------:|-----:|
| Instruction                   |    31.6|     32.9|  32.0|
| Research                      |    10.3|     10.3|  10.3|
| Public service                |     4.3|      1.4|   3.3|
| Academic support              |     8.4|      9.0|   8.6|
| Student services              |     5.8|      8.4|   6.8|
| Institutional support         |    10.1|     13.1|  11.2|
| Auxiliary enterprises         |     9.3|      9.2|   9.3|
| Hospital services             |    10.8|     10.1|  10.5|
| Independent operations        |     0.5|      3.3|   1.5|
| Other expenses and deductions |     3.7|      1.8|   3.1|
| Net grant aid to students     |     5.1|      0.5|   3.5|

In short, IPEDS data show that the student population has grown rapidly since the late 1950s. However, as this population growth was so substantial, public subsidization of higher education has been unable to keep up in terms of per-pupil expenditures (SHEEO Report). As a result, institutions have passed an increasing share of the financial burden onto students in the form of higher tuition and fees. Government programs like Stafford loans and private financing have provided a means and mechanism for students to take on this burden, binding them financially in student debt, sometimes for decades. Market forces have compelled institutions to innovate and reduce costs whereever possible. As instruction constitutes a third of the budget, it has often been the target of many changes.

The Shifting Institutional Landscape of Adjunct Work
====================================================

Higher enrollment, financial constraints and a demand for contemporary skills has reworked the form and function of American colleges and universities over the last six decades. In the following section, we describe some of these important institutional shifts. Specifically, we examine how institutions have changed based on their level, control, degree-granting status, Carnegie classification and tenure system. Definitions of these components are offered below.

### Growth of Institutions by Level

By institutional level, we refer to the intensity and duration of academic programs offered by an institution. The Department of Education classifies institutions into one of three levels: 4-year or higher (4-year), 2-but-less-than-4-year (2-year), and less-than-2-year institutions. Four-year institutions tend to have the most intense program requirements and many are selective of their students. They also have strong research programs in many cases. Two-year institutions tend to have open enrollment policies and focus on career-oriented programs. These programs typically result in a certificate, a professional-technical degree, or an associate’s degree that is useful for transfering to a four-year institution. Instructional programs at four-year institutions are typically more general than 2-year schools. Less-than-two-year institutions fullfill a variety of needs (e.g., professional, social, etc.), but typically do not offer formal degrees.

``` r
Inst_Level_table %>% 
  gather(LEVEL, COUNT, 2:4) %>% 
  ggplot(aes(x=YEAR, y=COUNT, group=LEVEL, colour=LEVEL)) + 
  geom_line() +
  geom_line(size=1) +
  scale_color_discrete("Institution Level", labels = c('4-year',"less than 2-year",'2-year')) +
  labs(title="Growth in Postsecondary Institutions by Level", subtitle= "1989-2015", x="Year", y = "Number of Institutions") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=10), legend.title=element_text(size=10)) +
  labs(caption = "Evans & Furstenberg. NCES IPEDS Survey of Institutional Characteristics 1989-2015.")
```

![Growth of Postsecondary Institutions by Level](graphs/Inst_Growth_by_Level_over_Time-1.png)

As seen in Figure 5, the United States has added new postsecondary institutions throughout the entire timespan of this graph. Of all levels, the four-year sector has added the greatest number of institutions. There were only 1670 in 1980 and in 2015 there were 3161--a 90% increase. Two-year institutions have experienced similar rates of growth, going from 1018 in 1980 to 2195 in 2015 (115% growth). There is less information available on less-than-two-year institutions, but we know that their rate of growth has increased rapidly since the turn of the century. Since 2010 alone, the number of less-than-2-year schools has increased by 30%. Today, 40% of institutions are four-year and the remaining 60% of institutions are evenly split between two-year and less-than-two-year institutions.

### Growth by Institutional Control

There are also important changes to the institutional landscape in terms of institutional control. By control, we mean whether an institution is operated by publicly-elected or appointed officials (public control) or by privately-elected or appointed officials (private control). Institutions of private control derive their funding principally through private sources. There are two types of private control: non-profit and for-profit, depending on how the institution handles surplus revenue. Non-profits use revenue to advance the mission of the institution. For-profits channel revenue to owners.

``` r
Inst_Control_table %>% 
  gather(CONTROL, COUNT, 2:4) %>% 
  ggplot(aes(x=YEAR, y=COUNT, group=CONTROL, colour=CONTROL)) + 
  geom_line() +
  geom_line(size=1) +
  scale_color_discrete("Institutional Control", labels = c('For-Profit Institutions','Private Non-profit Institutions', 'Public Institutions')) +
  labs(title="Growth in Postsecondary Institutions by Control", subtitle= "1980-2015", x="Year", y = "Number of Institutions") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=10), legend.title=element_text(size=10)) +
  labs(caption = "Evans & Furstenberg. NCES IPEDS Survey of Institutional Characteristics 1980-2015.")
```

![Institutional Growth by Control Over Time](graphs/Inst_Growth_by_Control_over_Time-1.png)

It is clear from Figure 6 that the most dramatic change in institutional control is among private, for-profit institutions. In 1987, there were only 1102 for-profit postsecondary institutions. By 2015, there were 3454--an increase of over 200% in only thirty years. The number of public institutions and private, not-for-profit schools has remained more stable over time (growing very slowly compared to the for-profit sector). For-profit institutions include technical institutes, arts schools, nursing schools, many online programs, some law schools and business schools, to name a few areas. Some have expressed concern that the profit motive inherent to for-profit institutions is at odds with the mission of higher education. Others argue that the for-profit sphere fills in badly needed areas ignored by traditional institutions.

### Institutional Growth by Degree-granting Status

Degree-granting institutions offer programs that lead to a terminal degree at the associate’s level or higher. Degree-granting institutions also participate in the Title IV federal financial aid program. Many non-degree-granting institutions do not. Non-degree institutions typically offer certificates of competency, but such awards are not accredited.

``` r
Inst_Degree_table %>% 
  gather(DEGREE, COUNT, 2:3) %>% 
  ggplot(aes(x=YEAR, y=COUNT, group=DEGREE, colour=DEGREE)) + 
  geom_line() +
  geom_line(size=1) +
  scale_color_discrete("Degree-granting status", labels = c('Degree-granting Institutions','Non-degree-granting Institutions')) +
  labs(title="Growth in Postsecondary Institutions by Degree-granting Status", subtitle= "1996-2015", x="Year", y = "Number of Institutions") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=10), legend.title=element_text(size=10)) +
  labs(caption = "Evans & Furstenberg. NCES IPEDS Survey of Institutional Characteristics 1996-2015.")
```

![Institutional Growth by Degree Over Time](graphs/Inst_Growth_by_Degree_over_Time-1.png)

The educational landscape with regard to degree-granting status has remained fairly stable over the last decades. Both degree-granting and non-degree-granting instutions have increased slowly in number, but their rates of growth are comparable over time. Today, and historically, degree-granting institutions outnumber institutions without accreditation by about two to one.

### Institutional Growth by Carnegie Classification

A useful analytic framework for examining postsecondary institutions was developed by the Carnegie Foundation. The basic Carnegie classification framework separates degree-granting institutions into one of six categories, based principally on the type of degrees the institution typically awards. Doctoral institutions are research institutions awarding high numbers of doctorates. These instituitions are often distinguished further by their level of research intensity (where R1 refers to institutions with very high research intensity and R3 means moderate research intensity). Master's, Baccalaureate and Associate's institutions award predominately Master's, Bachelor's and Associate's degrees, respectively. Specialized institutions award a high number of degrees in one particular field or specialty (e.g., the arts, theology, health/medical training). Tribal institutions are schools participating in the American Indian Higher Education Consortium. All Carnegie-classified institutions are degree-granting, meaning that a considerable percentage (one-third) are excluded from Figure 8.

``` r
Inst_Carnegie_table %>% 
  gather(CARNEGIE, COUNT, 2:7) %>% 
  ggplot(aes(x=YEAR, y=COUNT, group=CARNEGIE, colour=CARNEGIE)) + 
  geom_line() +
  geom_line(size=1) +
  scale_color_discrete("Degree-granting status", labels = c('Associates Institutions','Baccalaureate Institutions','Masters Institutions','Doctoral Institutions','Specialized Institutions','Tribal Institutions')) +
  labs(title="Growth in Postsecondary Institutions by Carnegie Classification", subtitle= "1994-2014", x="Year", y = "Number of Institutions") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=10), legend.title=element_text(size=10)) +
  labs(caption = "Evans & Furstenberg. NCES IPEDS Survey of Institutional Characteristics 1994-2014.")
```

![Institutional Growth by Carnegie Classification Over Time](graphs/Inst_Growth_by_Carnegie_over_Time-1.png)

Each class of institution in the Carnegie system has grown since the early 1990s. The growth of Bachelor's, Master's and Specialized institutions has been comparable during this time period. There were approximately 500 institutions in each of these categories in the early 1990s and their numbers have increased by about 50% since that time. The growth of Tribal institutions and doctoral institutions has been more moderate. There were 232 doctorate institutions in 1994. In 2014, there were 290. Associate's institutions are the most prevalent institutional class and have also experienced the most growth in recent times. Their numbers increased from 1216 in 1994 to 1708 in 2014. At both time points, Associate's institutions have held about forty percent of the "market share" of degree-granting institutions.

### The Use of Tenure Systems in Academia

Finally, we examine the use of tenure systems in postsecondary education. Historically, virtually every institution held a body of permanent faculty with tenured contracts. It is well-known that the number of tenure systems as declined over time. In the following figure, we document this trend in schools by institutional sector (level and control).

``` r
Tenure_Sector_table %>%
  filter(UNIT=="INST") %>% 
  gather(TENURE, PCT, 2:15)  %>% 
  filter(TENURE %in% c("FORPROF","PUB4YRTOTAL","PUB2YR","NONPROF4YRTOTAL","NONPROF2YR")) %>%
  ggplot(aes(x=YEAR, y=PCT, group=TENURE, colour=TENURE)) + 
  geom_line() +
  geom_line(size=1) +
  scale_color_discrete("Institutional Sector", labels = c("For-profit", "Non-Profit 2-Year", "Non-profit 4-Year", "Public 2-Year", "Public 4-Year")) +
  labs(title="Percent of Institutions with Tenure Systems, by Sector", subtitle= "1993-2015", x="Year", y = "Percent of Institutions") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=10), legend.title=element_text(size=10)) +
  labs(caption = "Evans & Furstenberg. NCES IPEDS Survey of Institutional Characteristics 1993-2015.")
```

![Percent of Institutions with Tenure Systems, by Sector](graphs/Pct_Inst_w_Tenure_Sys_over_Time-1.png)

From Figure 9, it is clear that institutions vary greatly in their use (or non-use) of tenure systems. Only a small fraction of for-profit institutions utilize a tenure system for managing permanent employees. Public, four-year instiutions, on the other hand, commonly do utilize a tenure system. The use of tenure systems in other institutional types, however, does seem to be on a very slow decline. In general, tenure systems seem to be disappearing, although the rate of disappearance may have flattened out in recent times.

### Summary

The institutional landscape has changed greatly over the last decades. In this section, we analyzed some of the most important ways postsecondary institutions have grown and evolved in recent times. Of the many findings reported, there were a few principle findings that merit highlighting. First, postsecondary institutions have grown in every way we examined, without exceptions. They have grown in all levels (four-year, two-year, less-than-2-year) and among all forms of institutional control (public, private non-profit, for-profit). Degree-granting, non-degree-granting, and all Carnegie classes are increasing in number. In short, higher education is experiencing a great expansion and this growth is taking place across virtually all dimensions of the U.S. postsecondary infrastructure.

Second, some institutional types have grown more than others and it is worth highlighting those areas of prominent growth. Far and away, the most apparent expansion is the for-profit sector of higher education. In the 1980s, only a quarter of institutions were for-profit. Today, nearly half are. This is an extremely important change in how higher education is managed and where resources are channeled. Our study, however, is limited in important ways. We only conducted analysis at the institution-level and did not examine the number of students actually enrolled in for-profit schools. As such, it is impossible for us to say just how consequential the shift towards greater numbers of institutions may be for higher education. Further research in this vein is badly needed.

The other important area of prominent growth has been less-than-two-year institutions. Not only has their number increased in recent times, but their growth has been accelerating over the last decade and a half. Today, there are as many less-than-two-year institutions as there are traditional two-year institutions. Some of these programs result in a terminal degree, but many no doubt simply provide an unaccredited certificate. It is not clear what the reasons are for this growth, nor what the consequences will be. Nearly a third of institutions today are less-than-two-year institutions.

``` r
Delta_Crosstable %>%
  `*`(100) %>%
  round(1) %>%
  `colnames<-`(c("Public","Private Non-profit","For-profit")) %>%
  `rownames<-`(c("Four-year","Two-year","Less-than-two-year")) %>%
  kable(caption = "Crosstabulation of Level and Control (in percentages)", fig.align="center")
```

|                    |  Public|  Private Non-profit|  For-profit|
|--------------------|-------:|-------------------:|-----------:|
| Four-year          |    55.9|                10.9|        30.8|
| Two-year           |    34.7|                85.8|        14.3|
| Less-than-two-year |     9.5|                 3.3|        54.9|

A simple crosstabluation of institutional level and institutional control reveals how interrelated the for-profit sector is with less-than-two-year institutions. Examing row sums, 81% (18.6/(18.6+1 + 3.4)) of less-than-two-year institutions are for-profit. Or alternatively, 55% (54.9/(54.9+14.3+30.8)) of for-profit institutions are less-than-two-year (column sum). Clearly, the for-profit, 2-year sector is a growing and important player on the stage of higher education.

Reining in Postsecondary Expenses
=================================

In the previous sections we highlighted the growth of for-profit and less-than-two-year institutions (many of which are one in the same). We also documented the expansion of the student-age population and the decrease in per-pupil government support. This erosion of support has forced institutions to reevaluate how to best allocate resources. Budget cuts have been made in many areas, however, funds linked to instruction have often been a focus, as nearly one third of an institution's budget goes towards instruction.

One controversial way for colleges and universities to reduce costs has been to eliminate the traditional tenure system or, in any case, decrease the number of tenured faculty on the payroll. This is because tenured faculty earn considerably more and have more generous (and expensive) benefits on average. However, short-term contracts also have implications for workforce stability and organizational effectiveness. Earlier, we saw that the number of institutions with tenure systems has remained fairly stable across time (although the prevalence of a tenure system differs by institutional features). Now, we examine the tenure system at the individual level, charting how the proportion of non-tenured faculty within institutions differs among different institutional features and types.

Tenure and Full-time status of Faculty
--------------------------------------

To begin with, we examine the proportion of non-tenure track faculty across all institutions in the United States. We also factor in the full-time or part-time status of adjuncts, as this is known to be important to the tenure system. Before interpreting Figure 10, however, it is important to consider the improbable dip or shift occuring between 2011 and 2012 in this graph. It is unclear why such a pattern came about, however, it might be related to a major change made in how occupations were classified since 2012. The Department of Education made these changes in order to align the Human Resources component of IPEDS with the 2010 Standard Occupational Classification (SOC) System. In the following paragraphs, we will try to identify patterns as best we can.

``` r
Tenure_Sector_table %>%
  filter(UNIT=='FACULTY') %>% 
  gather(TENURE, PCT, 2:15) %>% 
  filter(TENURE %in% c("FORPROF","PUB4YRTOTAL","PUB2YR","NONPROF4YRTOTAL","NONPROF2YR")) %>%
  ggplot(aes(x=YEAR, y=PCT, group=TENURE, colour=TENURE)) + 
  geom_line() +
  geom_line(size=1) +
  scale_color_discrete("Institutional Sector", labels = c("For-profit", "Non-Profit 2-Year", "Non-profit 4-Year", "Public 2-Year", "Public 4-Year")) +
  labs(title="Percent of Full-time Faculty with Tenure, by Sector", subtitle= "1993-2015", x="Year", y = "Percent of Full-time Faculty") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=10), legend.title=element_text(size=10)) +
  labs(caption = "Evans & Furstenberg. NCES IPEDS Survey of Institutional Characteristics 1993-2015.")
```

![Percentage Tenured Faculty in Postsecondary Education (IPEDS)](graphs/IPEDS_Tenure_Pct_over_Time-1.png)

``` r
Tenure_Status_table %>%
  gather(STATUS, PCT, 2:5) %>%
  ggplot(aes(x=YEAR, y=PCT, group=STATUS, colour=STATUS)) +
  geom_line() +
  geom_line(size=1) +
  scale_color_discrete("Institutional Sector", labels = c('Full-time Non-Tenure/Track','Full-time Tenured','Full-time Tenure-Track','Part-time Faculty')) +
  labs(title="Percentage of Faculty Types in Postsecondary Education", subtitle= "2002-2015", x="Year", y = "Percent of Faculty") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=10), legend.title=element_text(size=10)) +
  labs(caption = "Evans & Furstenberg. NCES IPEDS Survey of Employees by Assigned Position (EAP), primary function/occupational activity 2002-2015.")
```

![Percentage of Faculty Types in Postsecondary Education (IPEDS)](graphs/IPEDS_Facclass_Pct_over_Time-1.png)

Figure 10 shows two general patterns over time. First, part-time work is on the rise in academia. This has been a well-recognized pattern since as early as the 1970s. However, the trend has also carried over well into the twenty-first century. In the figure, we see that the proportion of part-time faculty increased from 41.5% in 2002 to 42.2% in 2014. Admittedly, this is a small difference. However, changes made to the 2012 instruemnt (and after) probably mitigated the true growth of part-time labor during this period. Up until 2012, the proportion of part-time faculty was clearly climbing. In fact, 47% of faculty were part-time in 2011, just before the survey was changed. As a result, we believe that the trend may be stronger than indicated in the figure.

Secondly, the proportion of non-tenure track faculty is clearly increasing. 63% were non-tenure in 2002 (considering both full-time and part-time NTT facutly). In 2014, 66.9% of faculty were classified as non-tenure track. It is unclear how survey changes in 2011-2012 may have impacted this pattern, as the drop in part-timers may have been offset by the increase in full-timers. In any case, it is clear that the growth of non-tenure track faculty is a clear pattern.

Contracts of non-tenure track faculty take different forms. Some non-tenure track faculty hold multi-year contracts, which may give them the sense of permanence or belonging in their institutions. However, multi-year contracts are rare. Most adjuncts work under contracts renewed on an annual basis. This is true for full-time adjuncts and part-time adjuncts.

``` r
Contract_table %>% 
  gather(FTSTATUS,COUNT,1:2) %>%
  ggplot(aes(x=CLASS, y=COUNT, fill=FTSTATUS)) + 
  geom_bar(stat="identity", position="dodge") +
  scale_x_discrete(labels = c("Annual \n Contracts","Multi-year \n Contracts","Non-faculty or \n Non-tenure Inst.","Tenure/Track \n Contracts")) +
  scale_fill_manual("", values = c("blue", "red"), labels=c("Full-time", "Part-time")) +
  labs(title = "Instructional Faculty Contracts in 2013", y = "Number of Instructors", x="Faculty Class", caption = "Evans & Furstenberg. NCES IPEDS Finance Component (Delta Project) 2013.") +
  theme(panel.background = element_rect(fill = 'aliceblue', colour = 'black'))
```

![Faculty Contracts in 2013](graphs/Faculty_Contracts_2013-1.png)

In the following sections, we examine more closely the growth of non-tenure track faculty across important aspects of postsecondary institutions. Specifically, we examine the distribution of adjuncts in institutions based on their level, control, degree-granting status and Carnegie classification. We also look at the role of tenure status in administration and clinical/medical positions. We then turn to other datasets (SDR and HERI) that allow us to examine other important relationships.

### Growing Percentages of Non-tenure Track Faculty by Institutional Levels.

IPEDS shows that the percentage of non-tenure track faculty has increased in every institutional level (4-year, 2-year and less-than-2-year). The most apparent growth is in less-than-two-year institutions. In 2002, 70% of faculty in these institutions were non-tenure track. By 2007, tenured faculty had virtually disappeared from all less-than-two-year institutions. Two-year and four-year institutions have also experienced increases in their percentage of non-tenure track faculty members; albeit more moderate growth. Just over 60% percent of faculty in four-year institutions work under contingent contracts today. Eighty percent of faculty in two-year schools are non-tenure track.

``` r
Level_Ten_table %>%
  ggplot(aes(x=YEAR, y=PCTNTT, group=LEVEL, colour=LEVEL)) +
  geom_line() +
  geom_line(size=1) +
  scale_color_discrete("Institutional Level", labels = c('Two-Year Institutions','Four Year Institutions','Less than Two-Year Institutions')) +
  labs(title="Percent Non-tenured Faculty by Institutional Level", subtitle= "2002-2015", x="Year", y = "Percent Non-tenure") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=10), legend.title=element_text(size=10)) +
  labs(caption = "Evans & Furstenberg. \n NCES IPEDS Survey of Employees by Faculty Status, primary function/occupational activity 2002-2015. \n All U.S.-based, postsecondary employees with faculty status in every field")
```

![Percent Non-Tenure Track by Institutional Level](graphs/PCT_NTT_by_Level-1.png)

### Growing Percentages of Non-tenure Track Faculty by Institutional Control

The percentage of faculty working off the tenure track has also increased in public, private non-profit, and for-profit institutions. The pattern is most clear for faculty working in private, for-profit institutions. Just over 90% of for-profit faculty were off the tenure track at the turn of the century. Virtually all for-profit faculty today work off the tenure track. The percentage of non-tenure track faculty has been quite similar between public institutions and private, non-profit institutions. There has been a gradual increase in the percentage of non-tenure track faculty since the earliest time period we examined in our data. Around 60% were non-tenure track in 2002. About 65% are non-tenure today in these kinds of institutions.

``` r
Inst_Control_Ten_table %>%
  ggplot(aes(x=YEAR, y=PCTNTT, group=CONTROL, colour=CONTROL)) +
  geom_line() +
  geom_line(size=1) +
  scale_color_discrete("Institutional Control", labels = c('For-Profit Institutions','Non-Profit Institutions','Public Institutions')) +
  labs(title="Percent Non-tenured Faculty by Institutional Control", subtitle= "2002-2015", x="Year", y = "Percent Non-tenure") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=10), legend.title=element_text(size=10)) +
  labs(caption = "Evans & Furstenberg. \n NCES IPEDS Survey of Employees by Faculty Status, primary function/occupational activity 2002-2015. \n All U.S.-based, postsecondary employees with faculty status in every field")
```

![Percent Non-Tenure Track by Institutional Control](graphs/PCT_NTT_by_Control-1.png)

### Growing Percentages of Non-tenure Track Faculty by Institutional Degree-granting Status

There are clearly recognizable patterns among faculty working in degree-granting institutions and those not offering degrees (Figure 14). In 2002, over 70% of non-degree (institutional) faculty were off the tenure track. This figure reached its ceiling (100%) less than a decade later. Already by 2007, only a small fraction of faculty in non-degree-granting institutions were tenured or on the tenure track. The growth of adjuncts in degree-granting institutions has risen much more modestly over the last decade and a half. Non-tenure track faculty are now approaching two-thirds of the faculty workforce in degree-granting institutions.

``` r
Degree_Inst_Ten_table %>%
  ggplot(aes(x=YEAR, y=PCTNTT, group=DEGREE, colour=DEGREE)) +
  geom_line() +
  geom_line(size=1) +
  scale_color_discrete("Institutional Control", labels = c('Degree-Granting','Non-Degree Granting')) +
  labs(title="Percent Non-tenured Faculty by Degree-granting Status", subtitle= "2002-2015", x="Year", y = "Percent Non-tenure") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=10), legend.title=element_text(size=10)) +
  labs(caption = "Evans & Furstenberg. \n NCES IPEDS Survey of Employees by Faculty Status, primary function/occupational activity 2002-2015. \n All U.S.-based, postsecondary employees with faculty status in every field")
```

![Percent Non-tenure Track by Degree Granting Status](graphs/PCT_NTT_by_Degree_Granting_Status_over_Time-1.png)

### Growing Percentages of Non-tenure Track Faculty by Carnegie Class

It is clear from Figure 15 that the portion of non-tenure track faculty is increasing at every Carnegie type. Potentially, the proportion of non-tenure track faculty at Associate's institutions has leveled off over the last five years, but more data would be necessary to confirm this. Institutions granting greater numbers of higher degrees (Bachelor's, Master's and Doctorates) are employing higher proportions of faculty off the tenure track. Roughly half of faculty in these institutions were non-tenure track in 2002. That proportion has grown by 6-7 percent since 2002. Faculty at "Other" institutions also tend to be, increasingly, non-tenure track faculty. "Other" institutions include specialized institutions that focus on limited fields (e.g., Art and Music schools, some business schools and medical institutions). They also include schools associated with the American Indian Higher Education Consortium (Tribal schools). Nearly 80% of faculty work off the tenure track in these schools, an increase of around 10% since 2002.

``` r
Carnegie_Ten_table1 %>%
  ggplot(aes(x=YEAR, y=PCTNTT, group=CARNEGIE1, colour=CARNEGIE1)) +
  geom_line() +
  geom_line(size=1) +
  scale_color_discrete("Carnegie Status", labels = c('Associates Institutions','Baccalaureate/Masters Institutions','Research Institutions','Other')) +
  labs(title="Percent Non-tenured Faculty by Carnegie Classification", subtitle= "2002-2015", x="Year", y = "Percent Non-tenure") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=10), legend.title=element_text(size=10)) +
  labs(caption = "Evans & Furstenberg. \n NCES IPEDS Survey of Employees by Faculty Status, primary function/occupational activity 2002-2015. \n All U.S.-based, postsecondary employees with faculty status in every field")
```

![Percent Non-Tenure Track by Carnegie Status](graphs/PCT_NTT_by_Carnegie_Class_over_Time-1.png)

### Growing Percentages of Non-tenure Track Faculty by Research Intensity.

Often, the Carnegie classification of doctoral instiutions includes their level of research intensity. Doctoral institutions with high research activity are referred to as "R1" and doctoral institutions of moderate research intensity are "R3" institutions. There are also "R2" institutions in between. An important question is how the hiring of non-tenure track faculty may impact the research goals and capabilities of higher education. The following figure explores whether the significant change in faculty contracts is any differnt in institutions of different research intensities.

``` r
Carnegie_Ten_table2 %>%
  ggplot(aes(x=YEAR, y=PCTNTT, group=CARNEGIE2, colour=CARNEGIE2)) +
  geom_line() +
  geom_line(size=1) +
  scale_color_discrete("Carnegie Status", labels = c('Non-research Institution','Moderate Research Intensity (R3)','Very High Research Intensity (R1)','High Research Intensity (R2)')) +
  labs(title="Percent Non-tenured Faculty by Carnegie Research Classification", subtitle= "2002-2015", x="Year", y = "Percent Non-tenure") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=10), legend.title=element_text(size=10)) +
  labs(caption = "Evans & Furstenberg. \n NCES IPEDS Survey of Employees by Faculty Status, primary function/occupational activity 2002-2015. \n All U.S.-based, postsecondary employees with faculty status in every field")
```

![Percent Non-Tenure Track by Research Intensity](graphs/PCT_NTT_by_Carnegie_Research_Intensity_over_Time-1.png)

As seen in Figure 16, regardless of research intensity, the proportion of adjuncts is increasing in all research typologies. R1 and R2 institutions are quite similar with regard to the time trend. Adjuncts were a slight minority in 2002 and they are a slight majority today. The proportion of adjuncts in R3 institutions is increasing a bit more rapidly. There was a comparable proportion of adjuncts in R3 institutions (as in R1 and R2 institutions) in 2002. However, the increased hiring of adjuncts in R3 institutions has made them constitute over 60% of the faculty body today in those institutions. Non-research institutions have also increased the portion of their faculty working off the tenure track, however changes have been more modest. Currently, 70% of faculty at non-research institutions are contingent.

### Growing Percentages of Non-tenure Track Faculty in Selective Institutions

Some have expressed concern that students attending less selective postsecondary institutions are more likely to have non-tenure track faculty. Such an event has implications for the stratification of educational opportunity. Using data from IPEDS, we examine this question. We divided postsecondary institutions into those using admissions testing and those that did not ("non-selective"). Then, among the schools using entrance examinations, we divided them into "most selective","highly selective", "very selective","selective" and "less selective", according to whether the schools' entering freshmen were in the 95th, 85th, 75th, 50th or lower percentile, respectively.

``` r
Inst_Select_Pct_NTT_table %>%
  ggplot(aes(x=YEAR, y=PCTNTT, group=SAT_75THF, colour=SAT_75THF)) +
  geom_line() +
  geom_line(size=1) +
  scale_color_discrete("Selectivity", labels = c('Less Selective','Selective','Very Selective','Most Selective','Non-selective')) +
  labs(title="Percent Non-tenured Faculty by Institutional Selectivity", subtitle= "2002-2015", x="Year", y = "Percent Non-tenure") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=10), legend.title=element_text(size=10)) +
  labs(caption = "Evans & Furstenberg. \n NCES IPEDS Survey of Employees by Faculty Status, primary function/occupational activity 2002-2015. Admissions and Test Scores Component 2015. \n All U.S.-based, postsecondary employees with faculty status in every field. \n Non-Selective instiutions are those not requiring/reporting SAT or ACT composite scores of their entering freshmen class.")
```

![Percent Non-Tenure Track by Institutional Selectivity](graphs/PCT_NTT_by_Selectivity-1.png)

From Figure 17, we find that, among institutions that report or require entrance examinations (SAT Math/Reading or ACT composite), there was little variation in the percentage of non-tenure track faculty. Whether a school's entering freshmen were in the 95th percentile or some lower selective tier, adjuncts constituted about half the faculty. This portion has risen by about five percent over the course of thirteen years. Today, a slight majority of faculty are non-tenure track in schools using entrance examinations.

There is a difference, however, between the schools using entrance examinations and "non-selective schools." Non-selective schools were those institutions that did not require entrance exams and thus did not report them to the Department of Education. At each time point, we see that the proportion of non-tenure track faculty is about ten points higher in non-selective schools. Similar to selective schools, that proportion has increased over time with about the same rate of change. Today, over two-thirds of faculty are non-tenure track in non-selective schools.

Growth in Administrative Positions for Adjuncts
-----------------------------------------------

It is well known that the administrative overhead has expanded greatly in higher education over the last decades. Rather than allocate administrative decision-making to faculty in academic departments, formal positions were created to handle administration on a full-time basis. Many administrators draw larger salaries and more generous benefits packages than academic faculty and some have claimed that the rising cost of higher education is linked to having to pay for the expanding administrative overhead. In this logic, colleges and universities may increasingly utilize non-tenure track academics to handle administrative jobs and tasks. In Figure 18, we examine the role of non-tenure track faculty in adminstrative positions.

``` r
Admin_Tenure_table %>%
  gather(STATUS, PCT, 2:5) %>%
  ggplot(aes(x=YEAR, y=PCT, group=STATUS, colour=STATUS)) +
  geom_line() +
  geom_line(size=1) +
  scale_color_discrete("Faculty Class", labels = c('Full-time Non-faculty Mgmt','Full-time NTT','Full-time Tenure/Track','Part-time Mgmt')) +
  labs(title="Tenure and Faculty Status of Postsecondary \n Administrators and Management", subtitle= "2002-2015", x="Year", y = "Percent of Management") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=10), legend.title=element_text(size=10)) +
  labs(caption = "Evans & Furstenberg. NCES IPEDS Survey of Employees by Assigned Position (EAP), primary function/occupational activity 2002-2015.")
```

![Percent Non-Tenure Track by Administrative Role](graphs/PCT_NTT_by_Admin-1.png)

Figure 18 suggests that adjuncts do not have an increasingly important role in management. In fact, workers classified as "faculty" in general are not involved in administration. Only a fifth of administrators have the classification of "faculty." This may be because faculty simply are not used extensively in an administrative capacity. Alternatively, institutions may not classify academics as "faculty" once taking on extensive roles in administration or management outside of their home department. Clarification on this topic would be important to fully answer questions regarding the role of faculty adjuncts in higher education administration. In any case, IPEDS data suggests that the work of administration is principally done by individuals working full-time in a non-faculty capacity. There may even be a slight decrease in the role faculty in postsecondary administration over time, however, the time trend is not entirely clear. Part-time, non-tenure track faculty were excluded from Figure 18 because there are so few of them participate in management.

### Growth of Adjuncts in Clinical/Medical Positions

Another important question with regard to adjunct growth is whether this growth may be related to medical or clinical fields. It is widely known that medical fields regularly employ off-tenure track faculty to teach practical courses in nursing, medical research and medicine. The growth of adjuncts, then, may be the consequence of the staffing of medical fields.

``` r
Med_table %>%
  gather(STATUS, PCT, 2:3) %>%
  ggplot(aes(x=YEAR, y=PCT, group=STATUS, colour=STATUS)) +
  geom_line() +
  geom_line(size=1) +
  scale_color_discrete("Institution Type", labels = c('Medical','Non-medical')) +
  labs(title="Tenure Status of Postsecondary Faculty \n in Medical and Non-medical Schools", subtitle= "2002-2015", x="Year", y = "Percent Non-tenure Track Faculty") +
  theme(axis.text=element_text(size=10), axis.title=element_text(size=10), legend.text=element_text(size=10), legend.title=element_text(size=10)) +
  labs(caption = "Evans & Furstenberg. NCES IPEDS Survey of Employees by Assigned Position (EAP), primary function/occupational activity 2002-2015. \n All U.S.-based, postsecondary employees with faculty status in every field.")
```

![Percent Non-Tenure Track by Medical/Non-Medical Fields](graphs/PCT_NTT_by_Medical-1.png)

Figure 19 shows, generally, that adjuncts have a similar presence in both medical and non-medical fields. Either way, just over 60% of faculty were non-tenure track in 2002. There may have been some minor growth of adjuncts in medical and clinical positions compared to non-medical positions, however, the growth rates are pretty comparable. The more important story is that there are comparable percentages of non-tenure track faculty in both medical and non-medical fields.

Other Aspects of Non-tenure Track Faculty
-----------------------------------------

There are several important features for which data is unavailable in IPEDS. Specifically, it would be useful to know whether the faculty population is changing with regard to post-docs, retirement-age faculty, and reasons for part-time employment. And IPEDS cannot help us with regard to those topics. Fortunately, there are alternative instruments, SDR and HERI, that are helpful in elucidating these features. Like IPEDS, SDR is a longitudinal instrument; in fact, it is a panel dataset. However, it is important to recognize that the SDR sample consists only of doctorate recipients in STEM fields and does not generalize to the entire faculty population. Many adjuncts, we know, hold master's degrees and sometimes only a bachelors (or less) and they were excluded from the SDR sampling frame. Adjuncts also often work in the humanities and other non-STEM fields, meaning that those adjuncts were excluded as well from SDR. Thus, while SDR contains important information, our inferences are unfortunately only limited to doctorate-holding faculty in STEM.

Before investigating these important faculty features, however, it is worth a quick empirical comparison of non-tenure track faculty in our two longitudinal datasets. Figure 20 from SDR uses the same analytic categories as Figure 10 (IPEDS), only the SDR dataset is limited to STEM Ph.D.'s.

``` r
include_graphics(file.path(Graph, "SDR_Tenure_Pct_over_Time.png"))
```

<img src="./graphs/SDR_Tenure_Pct_over_Time.png" alt="Percentage of Faculty Types in Postsecondary Education (SDR)" width="250px" />
<p class="caption">
Percentage of Faculty Types in Postsecondary Education (SDR)
</p>

Comparing these figures (Figure 20 and Figure 10), we see similarities. In both figures, the percentages of part-time faculty and non-tenure track full-time faculty have generally increased over time. The portion of full-time tenured and tenure-track faculty has dropped over time in both datasets. There are some differences in the two figures, owing to different sampling frames. Among Ph.D. recipients in STEM (the SDR sample), only 20% of faculty work part-time today today. The portion of faculty working part-time is higher in IPEDS, owing to the fact that it contains information on faculty of lower credentials. Individuals without a Ph.D. have more difficulty landing full-time work. Considering the different sampling frames, these figures seem to be telling similar stories with regard to the institution of tenure and full-time/part-time work.

### Post-docs and Non-tenure Track faculty

A common belief is that the number of post-doc appointments may be growing in order to accomodate the growing numbers of doctorate recipients who are unable to find tenure-track or even full-time work. In Figure 21, we examine this concern, documenting how the proportion of Ph.D.'s working in post-doc positions has changed over the last decades.

``` r
include_graphics(file.path(Graph, "SDR_PostDoc_over_Time.png"))
```

<img src="./graphs/SDR_PostDoc_over_Time.png" alt="Proportion of Ph.D. Recipients Working as Postdocs" width="250px" />
<p class="caption">
Proportion of Ph.D. Recipients Working as Postdocs
</p>

Among Ph.D.'s working in academia, seven percent work in non-tenure track postdoc fellowships. This is perhaps a larger percentage than many would expect, as historically Ph.D. recipients have entered immediately into traditional academic career lines. Nonetheless, it is important to note that the trend appears to be moving in the opposite direction many would expect. Rather than increase in proportion, the share of postdocs in academia actually seems to be dropping. In fact, our data suggest that the number of postdoc positions may be dropping in an absolute sense. There were 1917 doctorate recipients in the SDR sample who defined their work as a "postdoc" in 1993. In 2013, that number dropped to 904. This result is fairly surprising and goes against common assumptions. Post-doc positions are not absorbing the increasing numbers of Ph.D.'s. They are increasingly less likely to be an option for newly minted doctorate recipients.

### Growth of retirement-age folks

Another common argument is that adjunct positions, increasingly, are used as a transition to retirement. In this reasoning, full-time faculty and some individuals from the private sector accept off-tenure positions in order to gradually reduce their workload before entering permanent retirement. As SDR contains age-related information, this question can be examined using the dataset.

``` r
include_graphics(file.path(Graph, "SDR_Seniors_over_Time.png"))
```

<img src="./graphs/SDR_Seniors_over_Time.png" alt="Proportion of Retirement-age Faculty" width="250px" />
<p class="caption">
Proportion of Retirement-age Faculty
</p>

This Figure 22 suggests that, indeed, the number of faculty working during retirement years (65+) is increasing over time. This is particularly the case for seniors opting to continue a full-time, tenured position well after their 65th birthday. Just over two percent of faculty worked as seniors in 1995. Six percent do so today. There has also been moderate growth among seniors working part-time off the tenure track and seniors working full-time, non-tenure track positions.

While older faculty (65+) continue to constitute only a small minority of postsecondary faculty, nearly ten percent of faculty are "retirement-age" today. Whether adjunct positions are actually being utilized as a trasition to retirement, there is less compelling evidence. Many faculty continue to work past their 65th birthday and most of them continue full-time work as well. If their workloads are decreasing, as some have argued, they in any case continue to work full-time. Senior (65+) faculty working part-time more closely embodies a faculty type transitioning to retirement. However, the proportion of faculty working in this capacity is very, very small--at least among doctorate recipients in STEM fields.

### Why Work Part-time as an Adjunct?

A final important question about the growth of adjuncts is tied to why exactly faculty work part-time in the first place. These are highly educated and trained indviduals with valuable skills in a wide range of fields. As mentioned earlier, one theory is that part-time work helps accomodate the growing number of Ph.D.'s who are unable to find full-time work. Other theories are that part-time work helps faculty transition to retirement, manage family responsibilities or hold a full-time career outside of academia. The SDR instrument specifically asked Ph.D. recipients working part-time why exactly they chose a part-time job. The following figure (Figure 23) casts light on the many reasons for doing so.

``` r
include_graphics(file.path(Graph, "SDR_PT_Reasons.png"))
```

<img src="./graphs/SDR_PT_Reasons.png" alt="Reasons for Part-time Work in Academia in 2013" width="250px" />
<p class="caption">
Reasons for Part-time Work in Academia in 2013
</p>

One important finding from this figure is that many adjuncts are not even interested in full-time work. There are also other, more specific, reasons for their part-time status. Nearly three in ten part-time respondents claimed their adjunct position is helping them transition to retirement (semi-retired). About one in four explained that adjunct work has helped them fulfill family responsibilities. Just over 15% claimed that they worked part-time in order to facilitate a full-time career outside of academia. Other jobs they hold may be teaching positions at other institutions or a career in the private sector. Nearly a third of part-time faculty stated that they worked part-time because a full-time job was unavailable to them. So, overall, part-time work is not typically due to an inability to find full-time work. It helps adjunct balance other goals and responsibilities.

``` r
include_graphics(file.path(Graph, "SDR_PT_reasons_over_Time.png"))
```

<img src="./graphs/SDR_PT_reasons_over_Time.png" alt="Reasons for Part-time Work in Academia 2006-2013" width="250px" />
<p class="caption">
Reasons for Part-time Work in Academia 2006-2013
</p>

These categories or reasons for part-time work can also be examined over time, beginning in 2006 when the SDR first began to collect this particular information. The results were graphed in Figure 24. Half of the reasons Ph.D.s give for working part-time have not changed over time. So, similar portions in each year said that working part-time was related to student status, family responsibilities, retirement transition or they had "no interest in full-time work." On the other hand, there have been changes in the portion of part-timers explaining that a full-time job was not available or they worked part-time in order to hold multiple jobs. These categories roughly correspond to Gappa and Leslie's "aspiring academic" and "Expert/freelancer" typologies, respectively. Using these titles, aspriring academics are clearly on the rise. They constituted only 10% of part-time faculty in 2006 but are nearly 30% today. The growth has been more moderate among "expert/freelancers." There were very few of these adjuncts in 2006, but they have grown to occupy 20% of adjuncts today. It is important to note that participants may have two or more reasons for working part-time. So participants may be cross-classified in this figure.

Where are the Adjuncts?
=======================

For this question, we can return to the IPEDS data source. Non-tenure track faculty work all over the United States, but they are over-respresented in certain parts of the country. Highly urbanized areas and states on the coast tend to have higher percentages of non-tenure track faculty. This includes Virginia, North Carolina, Oregon and Florida. The midwest and southwest also are generally more dependent on adjuncts. Although not a coastal state, highly-urbanized Arizona has the highest percentage of non-tenure track faculty (82%). Rural states tend to be less dependent on contingent faculty. Only 52% of faculty work off the tenure track in Wyoming and Rhode Island. Other states with lower employment of adjuncts include North Dakota, Montana and Kentucky. An important caveat is that part-time faculty are more likely to have been double counted than full-time faculty, as some of them hold multiple jobs. Because part-timers are more likely to be non-tenure track, non-tenure track faculty may be overrepresented in this graphic. This same caveat is true for the next map.

``` r
include_graphics(file.path(Graph, "US_States_Pct_NTT.png"))
```

<img src="./graphs/US_States_Pct_NTT.png" alt="Non-tenure Track Faculty in the United States" width="250px" />
<p class="caption">
Non-tenure Track Faculty in the United States
</p>

The employment of part-time faculty tends to follow similar patterns. Populous states and those in the southwest, midwest and New England gererally utilize higher proportions of part-time faculty. Arizona, again, has the highest percentage of part-time faculty (nearly two-thirds). Other states utilizing high percentages of part-time faculty include California, Delaware and Illinois. Rural states are less dependent on part-time labor. Only 12% of faculty work part-time in Wyoming. North Dakota, Oklahoma and Arkansas also have notably lower percentages of part-time faculty. The caveat mentioned earlier is also relevant here. Part-time faculty, more than full-time faculty, are more likely to have been double counted if they teach at multiple institutions.

``` r
include_graphics(file.path(Graph, "US_States_Pct_PT.png"))
```

<img src="./graphs/US_States_Pct_PT.png" alt="Part-time Faculty in the United States" width="250px" />
<p class="caption">
Part-time Faculty in the United States
</p>

Discussion
==========

Aspiring academics do it because full-time work is not available to them and for the career (stepping stone) and the income Voluntary adjuncts do it for the lifestyle.

Appendix
========

Glossary
--------

1.  **Level:** Whether an institution is 4 or more years; at least 2, but less than 4; or less than 2 years.
2.  **Control:** Whether an institution is public, private not-for-profit, or private for-profit
3.  **Degree-granting status:** Degree-grantinging institutions are postsecondary institutions eligible for Title IV federal financial aid programs and also grant an associate’s degree or higher. All institutions participating in Title IV financial aid programs must offer a program of at least 300 hours in length, be accredited by the U.S. Department of Education, have existed for a minimum oft 2 years, and have signed a participation agreement with the Department of Education.
4.  **Carnegie Classification of Research intensity (2010):** There are other Carnegie Classification schemas including basic, Undergraduate Instructional Profile, Graduate Instructional Program, Undergraduate Profile, Enrollment Profile, and Size and Setting.
5.  **Faculty Status:** According to IPEDS, "Faculty Persons identified by the institution as such and typically those whose initial assignments are made for the purpose of conducting instruction, research or public service as a principal activity (or activities). They may hold academic rank titles of professor, associate professor, assistant professor, instructor, lecturer or the equivalent of any of those academic ranks. Faculty may also include the chancellor/president, provost, vice provosts, deans, directors or the equivalent, as well as associate deans, assistant deans and executive officers of academic departments (chairpersons, heads or the equivalent) if their principal activity is instruction combined with research and/or public service. The designation as "faculty" is separate from the activities to which they may be currently assigned. For example, a newly appointed president of an institution may also be appointed as a faculty member. Graduate, instruction, and research assistants are not included in this category.""
6.  **Tenure status:** According to IPEDS "Tenure-Status of a personnel position with respect to permanence of the position.
    Tenure track - Personnel positions that lead to consideration for tenure. Not on tenure track - Personnel positions that are considered non-tenure earning positions."
7.  **Full-time or Part-time status:** According to IPEDS, full-time or part-time status are "defined by the institution. The type of appointment at the snapshot date determines whether an employee is full time or part time. The employee's term of contract is not considered in making the determination of full or part time."
8.  **Medical:** According to IPEDS, "Medical school staff - Staff employed by or employees working in the medical school component of a postsecondary institution or in a free standing medical school. Does not include staff employed by or employees working strictly in a hospital associated with a medical school or those who work in health or allied health schools or departments such as dentistry, veterinary medicine, nursing or dental hygiene."
