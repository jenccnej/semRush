### Download and use the ‘semRush’ package in order to:

-   #### Take advantage of your SEMRush subscription to obtain raw, high-quality SEO data through an API (<b>a SEMRush account API key is required</b>).

-   #### Create and customize over 50 different data report requests from the six types of `*_reports` functions provided in this package.

    -   ##### Report types currently included: `overview_reports`, `domain_reports`, `keyword_reports`, `backlinks_reports`, `url_reports`, and `traffic_analytics_reports`. See function documentation for additional details.

-   #### Modify requests using options to obtain results from global or regional databases, impose limits to the number of rows returned in each report, specify precisely which variable columns to return in your reports, select a display date or timeframe for some report types, and <em>more…</em>

-   #### Automatically downloads (using GET requests) and parses HTML data tables, and returns the data to the R environment as a “tibble” object.

-   #### Conveniently access and store the data reports from the R environment for further processing with your preferred data analysis and visualization libraries.

-   #### Includes the ability to check your SEMRush account API unit balance and monitor the usage of API unit expenditure over a session.

-   #### All functions include documentation, examples, and links to relevant information about how to format report request options (e.g, regional database codes, export column codes, and defintions)

<div>
<div style="margin-bottom:50px;">
</div>


##### <b>Installation:</b> Copy and paste the code below to install the `semRush` package now using the `devtools::intall_github` function and a link to this GitHub repository.

    # Not run  
    #install.packages("devtools")
    install_github("ericvc/semRush")

###### <em><b>Console ouput</b></em> - calling the library from the R environment will load its namespace along with other required packages.

    >library(semRush)
    Loading required package: assertthat
    Loading required package: stringr
    Loading required package: httr
    Loading required package: tidyr
    Loading required package: rvest
    Loading required package: xml2


<div>
<div style="margin-bottom:50px;">
</div>

##### <b>Example</b>: Request a domain overview (all) (‘domain\_ranks’) report for a single domain. According to the SEMRush [API documentation](https://www.semrush.com/api-analytics/#domain_ranks), “this report provides live or historical data on a domain’s keyword rankings in both organic and paid search in all regional databases.”

    library(semRush)

    #Enter your SEMRush account API key
    key <- ""

    report <- overview_reports(
      type = "domain_ranks",
      key = key,
      domain = "cran.r-project.org",
      display_limit = 5,
      export_columns = c("Db","Dt","Dn","Rk","Or","Ot","Oc","Ad","At","Ac","Sh","Sv","FKn","FPn")
    )

    print(report)
    # A tibble: 5 x 12
      Database   Date Domain  Rank Organic.Keywords Organic.Traffic Organic.Cost Adwords.Keywords Adwords.Traffic
      <chr>     <int> <chr>  <int>            <int>           <int>        <int>            <int>           <int>
    1 ph       2.02e7 r-pro…   340             9061          504295        74456                0               0
    2 ge       2.02e7 r-pro…   881              571            8286          798                0               0
    3 et       2.02e7 r-pro…   950             1170            2542          141                0               0
    4 kh       2.02e7 r-pro…  1002              640            5108          443                0               0
    5 is       2.02e7 r-pro…  1017              643            2217            1                0               0
    # … with 3 more variables: Adwords.Cost <int>, PLA.keywords <int>, PLA.uniques <int>


<div>
<div style="margin-bottom:50px;">
</div>

##### <b>Example:</b> Request an organic results (‘phrase\_organic’) report for a keyword phrase. According to the SEMRush [API documentation](https://www.semrush.com/api-analytics/#phrase_organic), “this report lists domains that are ranking in Google’s top 100 organic search results with a requested keyword.”

    library(semRush)

    #Enter your SEMRush account API key
    key <- ""

    ## Generate 'phrase_organic' report
    report <- keyword_reports(
        type = "phrase_organic",
        key = key,
        phrase = "r software",
        database = "us",
        display_limit=10,
        export_colums = c("Dn","Ur")
    )

    print(report)

       Domain                        Url                                                                       
       <chr>                         <chr>                                                                     
    1  r-project.org                 https://www.r-project.org/                                                
    2  wikipedia.org                 https://en.wikipedia.org/wiki/R_(programming_language)                    
    3  rstudio.com                   https://rstudio.com/                                                      
    4  epa.gov                       https://archive.epa.gov/nheerl/arm/web/pdf/irss_2.6.pdf                   
    5  umich.edu                     https://www.icpsr.umich.edu/icpsrweb/content/shared/ICPSR/faqs/what-is-r… 
    6  psu.edu                       https://online.stat.psu.edu/statprogram/tutorials/statistical-software/r  
    7  datamentor.io                 https://www.datamentor.io/r-programming/                                  
    8  utoledo.edu                   https://libguides.utoledo.edu/stats-software/R                            
    9  statmethods.net               https://www.statmethods.net/r-tutorial/index.html                         
    10 predictiveanalyticstoday.com  https://www.predictiveanalyticstoday.com/r-software-environment/        


<div>
<div style="margin-bottom:50px;">
</div>
