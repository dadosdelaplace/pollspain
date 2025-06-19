
# ----- packages -----

library(tidyverse)
library(glue)
library(lubridate)
library(readr)


# ----- D'Hondt -----

hondt <- tibble()
hondt <-
  tibble(method = "Hondt", method_family = "divisors", region = "Spain",
         type_elec = c("congress", "local", "EU"), constituency = c("prov", "mun", "national"),
         constituency_type = "multi-member",
         candidate_ranking = "fixed by party",
         threshold = c(0.03, 0.05, 0),
         party_list = "closed",
         biblio = "https://infoelectoral.interior.gob.es/es/proceso-electoral/visitas-virtuales/metodo-dhont/"
         ) |>
  bind_rows(
    tibble(
      method = "Hondt", method_family = "divisors", region = "Belgium",
      type_elec = "congress", constituency = "prov",
      constituency_type = "multi-member",
      candidate_ranking = "partially by voter ranking",
      threshold = 0.05,
      party_list = "semi-open",
      biblio = "https://www.osce.org/files/f/documents/7/1/569376.pdf"
      )) |>
  bind_rows(
    tibble(
      method = "Hondt", method_family = "divisors", region = "Finland",
      type_elec = "congress",  constituency = "prov",
      constituency_type = "multi-member",
      candidate_ranking = "voter ranking",
      threshold = 0,
      party_list = "open",
      biblio = c("https://electoral-reform.org.uk/how-do-finlands-elections-work/",
                 "https://vaalit.fi")
    )) |>
  bind_rows(
    tibble(
      method = "Hondt", method_family = "divisors", region = "Poland",
      type_elec = "congress",  constituency = "prov",
      constituency_type = "multi-member",
      candidate_ranking = "voter ranking",
      threshold = 0.05,
      party_list = "closed",
      biblio = "https://pkw.gov.pl"
    )) |>
  bind_rows(
    tibble(
    method = "Hondt", method_family = "divisors", region = "Portugal",
    type_elec = "congress", constituency = "electoral district",
    constituency_type = "multi-member",
    candidate_ranking = "fixed by party",
    threshold = 0.00, party_list = "closed",
    biblio = "https://www.cne.pt"
  )) |>
  bind_rows(
    tibble(
      method = "Hondt", method_family = "divisors", region = "Netherlands",
      type_elec = "congress", constituency = "national",
      constituency_type = "multi-member",
      candidate_ranking = "voter ranking",
      threshold = 0.00, party_list = "open",
      biblio = "https://www.kiesraad.nl"
  )) |>
  bind_rows(
    tibble(
      method = "Hondt", method_family = "divisors", region = "Chile",
      type_elec = "congress", constituency = "district",
      constituency_type = "multi-member",
      candidate_ranking = "voter ranking",
      threshold = 0.00, party_list = "open",
      biblio = "https://www.servel.cl"
    )) |>
  bind_rows(
    tibble(
      method = "Hondt", method_family = "divisors", region = "Argentina",
      type_elec = "congress", constituency = "prov",
      constituency_type = "multi-member",
      candidate_ranking = "fixed by party",
      threshold = 0.03, party_list = "closed",
      biblio = "https://www.argentina.gob.ar/interior/justiciaelectoral"
    )) |>
  bind_rows(
    tibble(
      method = "Hondt", method_family = "divisors", region = "United States (1790–1830)",
      type_elec = "congress", constituency = "national",
      threshold = 0.00,
      party_list = NA,
      biblio = "https://www.ams.org/publicoutreach/feature-column/fcarc-apportion2"
    ))


# ----- webster -----

webster <- tibble()
webster <-
  tibble(method = "modified Sainte-Lague", method_family = "divisors", region = "Sweden",
         type_elec = "congress", constituency = "electoral district",
         constituency_type = "multi-member",
         candidate_ranking = "fixed by party",
         threshold = 0.04, # 12% by district otherwise
         party_list = "open",
         biblio = "http://archive.ipu.org/parline/reports/2303_B.htm"
         ) |>

# Webster

webster_nz <- tibble(
  method = "Webster",
  region = "New Zealand",
  type_elec = "national",
  constituency = "single-member electorates + national compensatory",
  threshold = 0.05,
  party_list = "closed",
  biblio = "https://www.ourcommons.ca/content/Committee/421/ERRE/Brief/BR8391757/br-external/2PedenR-e.pdf"
)

webster_norway <- tibble(
  method = "Webster",
  region = "Norway",
  type_elec = c("national", "local"),
  constituency = "multi-member constituencies + national compensatory",
  threshold = 0.04,
  party_list = "open",
  biblio = c("https://www2.goshen.edu/~dhousman/ugresearch/Kaasa%202016.pdf", "https://electoral-reform.org.uk/how-do-elections-work-in-norway/")
)


#Hill

hill_us <- tibble(
  method = "Huntington-Hill",
  region = "United States (1940–present)",
  type_elec = "national",
  constituency = "state",
  threshold = NA, # No legal threshold; apportionment based on state population
  party_list = NA,
  biblio = "https://www.ams.org/publicoutreach/feature-column/fcarc-apportion2"
)

#Es bastante técnico y específico al caso de Estados Unidos, especialmente por el límite fijo de 435 escaños impuesto desde 1929

# Hamilton

hamilton_russia <- tibble(
  method = "Hamilton",
  region = "Russia",
  type_elec = "national",
  constituency = "national party list",
  threshold = 0.07,
  party_list = "closed",
  biblio = "http://www.russiavotes.org/duma/duma_election_law.html"
)


# Hagenbach-Bischoff

switzerland <- tibble(
  method = "Hagenbach-Bischoff"
  region = "Switzerland",
  type_elec = "national",
  constituency = "cantonal multi‑member districts",
  threshold = 0,
  party_list = "open",
  biblio = "https://www.swissinfo.ch/eng/democracy/2019-parliamentary-elections_how-the-electoral-system-influences-success/44806622"
)


# Mixtos

brazil_mixed <- tibble(
  method = "Hamilton + Hondt",
  region = "Brazil",
  type_elec = "national",
  constituency = "federal unit",
  threshold = 0.1,
  party_list = "open",
  biblio = "https://digibug.ugr.es/bitstream/handle/10481/75822/TFM.pdf"
)

denmark_mixed <- tibble(
  method = "Hondt + Webster",
  region = "Denmark",
  type_elec = "national",
  constituency = c("constituency", "national compensatory"),
  threshold = 0.02,
  party_list = "open",
  biblio = "https://www.elections.im.dk/local-elections/the-electoral-system-in-denmark-local-and-regional-government-elections"
)


# Dean and Adams

#Estos métodos son de interés principalmente teórico o académico en el campo del apportionment.
#A veces se evalúan en simulaciones para ver cómo cambiaría la distribución de escaños si se reemplazara el método actual.
#Nunca han sido usados oficialmente, pero sí han sido considerados como alternativas en EE.UU., especialmente en el contexto de estudios de reparto de escaños tras los censos


#First Past the Post+

uk <- tibble(
  method = "FPTP",
  region = "United Kingdom",
  type_elec = "national",
  constituency  = "single‑member constituencies",
  threshold = 0,
  party_list = "closed",
  biblio = "https://consoc.org.uk/the-constitution-explained/electoral-systems/"
)

canada <- tibble(
  method = "FPTP",
  region = "Canada",
  type_elec = "national",
  constituency = "single‑member constituencies",
  threshold = 0,
  party_list = NA,
  biblio = "https://dev.populationstatistics.com/country-rankings/electoral-systems-by-country"
)

# Others (a comentar con Javi)

italy <- tibble(
  method = "FPTP + Hamilton (parallel)", #The national elections use a mixed single vote into a parallel voting system, with 36.825397% of seats allocated using a first-past-the-post electoral system and 63.174603% using a proportional method
  region  = "Italy",
  type_elec = "national",
  constituency = "single-member districts",
  threshold = 0.03,
  party_list = "closed",
  biblio = c("https://www.lemonde.fr/en/international/article/2022/09/24/italian-elections-mixed-voting-block-voting-and-multiple-candidacies-how-does-the-electoral-system-work_5998104_4.html", "https://en.camera.it/4?scheda_informazioni=28")
)


germany <- tibble(
  method = "Plurality voting system + Webster", #first vote and second vote. MMP
  region = "Germany",
  type_elec = "national",
  constituency = "single‑member + regional lists",
  threshold = 0.05,
  party_list = "closed",
  biblio = "https://aceproject.org/main/english/es/esy_de.htm"
)



# Join

apportionment_methods <- bind_rows(
  hondt_spain,
  hondt_belgium,
  hondt_finland,
  hondt_us,
  hondt_netherlands,
  hondt_portugal,
  webster_us,
  webster_sweden,
  webster_nz,
  webster_norway,
  hill_us,
  hamilton_russia,
  hamilton_us1,
  hamilton_us2,
  switzerland,
  brazil_mixed,
  denmark_mixed,
  uk,
  canada,
  italy,
  germany
)
