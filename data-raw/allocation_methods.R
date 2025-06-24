
# ----- packages -----

library(tidyverse)
library(glue)
library(lubridate)
library(readr)


# ----- D'Hondt -----
# 8 regions: Spain, Belgium, Finland, Poland,
#            Netherlands, Chile, Argentina, Portugal
# + EEUU (in the past)
hondt <- tibble()
hondt <-
  tibble(method = "Hondt", method_family = "divisors (PR)", region = "Spain",
         type_elec = c("congress", "local", "EU"), constituency = c("prov", "mun", "national"),
         constituency_type = "multi-member",
         candidate_ranking = "fixed by party",
         threshold = c(0.03, 0.05, 0),
         party_list = "closed",
         notes = "Spanish system is totally closed and multimember",
         biblio = "https://infoelectoral.interior.gob.es/es/proceso-electoral/visitas-virtuales/metodo-dhont/"
         ) |>
  bind_rows(
    tibble(
      method = "Hondt", method_family = "divisors (PR)", region = "Belgium",
      type_elec = "congress", constituency = "prov",
      constituency_type = "multi-member",
      candidate_ranking = "partially by voter ranking",
      threshold = 0.05,
      party_list = "semi-open",
      notes = "Semi-open system: voters may modify the order of candidates, but party order prevails unless candidates receive sufficient preference votes",
      biblio = "https://www.osce.org/files/f/documents/7/1/569376.pdf"
      )) |>
  bind_rows(
    tibble(
      method = "Hondt", method_family = "divisors (PR)", region = "Finland",
      type_elec = "congress", constituency = "prov",
      constituency_type = "multi-member",
      candidate_ranking = "voter ranking",
      threshold = 0,
      party_list = "open",
      notes = "Open system: voters select a candidate directly, and only personal votes determine the final order of candidates within parties",
      biblio = c("https://electoral-reform.org.uk/how-do-finlands-elections-work/",
                 "https://vaalit.fi")
    )) |>
  bind_rows(
    tibble(
      method = "Hondt", method_family = "divisors (PR)", region = "Poland",
      type_elec = "congress",  constituency = "prov",
      constituency_type = "multi-member",
      candidate_ranking = "voter ranking",
      threshold = 0.05,
      party_list = "semi-open",
      notes = "Semi-open system: party list order strongly determines outcomes due to low impact of preference votes",
      biblio = "https://pkw.gov.pl"
    )) |>
  bind_rows(
    tibble(
    method = "Hondt", method_family = "divisors (PR)", region = "Portugal",
    type_elec = "congress", constituency = "electoral district",
    constituency_type = "multi-member",
    candidate_ranking = "fixed by party",
    threshold = 0.00, party_list = "closed",
    notes = "Close sistem: voters select a party list, and seats are allocated according to fixed party order",
    biblio = "https://www.cne.pt"
  )) |>
  bind_rows(
    tibble(
      method = "Hondt", method_family = "divisors (PR)", region = "Netherlands",
      type_elec = "congress", constituency = "national",
      constituency_type = "multi-member",
      candidate_ranking = "voter ranking",
      threshold = 0.00, party_list = "open",
      notes = "Open system: no threshold but there exists a low preference vote threshold (~0.67% of national vote). Voters choose individual candidates, and those meeting the threshold can overtake list order",
      biblio = "https://www.kiesraad.nl"
  )) |>
  bind_rows(
    tibble(
      method = "Hondt", method_family = "divisors (PR)", region = "Chile",
      type_elec = "congress", constituency = "electoral district",
      constituency_type = "multi-member",
      candidate_ranking = "voter ranking",
      threshold = 0.00, party_list = "open",
      notes = "Open system: voters rank candidates within party lists; districts elect multiple members",
      biblio = "https://www.servel.cl"
    )) |>
  bind_rows(
    tibble(
      method = "Hondt", method_family = "divisors (PR)", region = "Argentina",
      type_elec = "congress", constituency = "prov",
      constituency_type = "multi-member",
      candidate_ranking = "fixed by party",
      threshold = 0.03, party_list = "closed",
      notes = "Closed system: candidate order fixed by parties; provinces serve as multi-member districts",
      biblio = "https://www.argentina.gob.ar/interior/justiciaelectoral"
    )) |>
  bind_rows(
    tibble(
      method = "Hondt", method_family = "divisors (PR)", region = "United States (1790–1830)",
      type_elec = "congress", constituency = "national",
      threshold = 0.00,
      party_list = NA,
      biblio = "https://www.ams.org/publicoutreach/feature-column/fcarc-apportion2"
    )) |>
  bind_rows(
    tibble(
      method = "Hondt", method_family = "divisors (PR)", region = "South Africa",
      type_elec = "congress", constituency = "national + prov",
      constituency_type = "multi-member",
      candidate_ranking = "fixed by party",
      threshold = NA,
      party_list = "closed",
      notes = "400 seats: half allocated nationally, half by province, using proportional representation with D’Hondt",
      biblio = "https://www.elections.org.za/"
    )
  ) |>
  bind_rows(
    tibble(
      method = "Hondt",
      method_family = "divisors (PR)",
      region = "Czech Republic",
      type_elec = "congress",
      constituency = "electoral district",
      constituency_type = "multi-member",
      candidate_ranking = "fixed by party",
      threshold = 0.05,
      party_list = "closed",
      notes = "Uses 14 electoral districts; higher thresholds for coalitions",
      biblio = "https://www.idea.int/data-tools/country-view/97"
    )
  )





# ----- Webster / Sainte-Lague -----
# 1 region: Norway
# + Latvia, Sweden (modified Sainte-Lague)
webster <-
  tibble(method = "modified Sainte-Lague", method_family = "divisors (PR)", region = "Sweden",
         type_elec = "congress", constituency = "electoral district",
         constituency_type = "multi-member",
         candidate_ranking = "fixed by party",
         threshold = 0.04,
         party_list = "open",
         notes = "threshold of 4% nationally or 12% in a district",
         biblio = "http://archive.ipu.org/parline/reports/2303_B.htm"
         ) |>
  bind_rows(
    tibble(
      method = "modified Sainte-Lague",
      method_family = "divisors (PR)",
      region = "Denmark",
      type_elec = "congress",
      constituency = "electoral district",
      constituency_type = "multi-member",
      candidate_ranking = "fixed by party",
      threshold = 0.02,
      party_list = "open",
      notes = "135 seats by district; 40 national compensatory seats for parties over 2% threshold",
      biblio = "https://www.ipu.org/resources/parliaments/institution/denmark"
    )
  ) |>
  bind_rows(
    tibble(method = "Webster", method_family = "divisors (PR)", region = "Norway",
           type_elec = c("congress", "local"), constituency = "electoral district",
           constituency_type = "multi-member",
           candidate_ranking = "voter ranking",
           threshold = 0.04,
           party_list = "open",
           notes = "PR with national compensatory seats; 4% national threshold",
           biblio = c("https://www2.goshen.edu/~dhousman/ugresearch/Kaasa%202016.pdf", "https://electoral-reform.org.uk/how-do-elections-work-in-norway/"))
  ) |>
  bind_rows(
    tibble(method = "modified Sainte-Lague", method_family = "divisors (PR)", region = "Latvia",
           type_elec = "congress", constituency = "electoral district",
           constituency_type = "multi-member",
           candidate_ranking = "partially by voter ranking",
           threshold = 0.05,
           party_list = "open",
           notes = "Open-list proportional representation with 5% national threshold. Voters can give positive and negative marks, which may alter the order of candidates on the list",
           biblio = "https://www.cvk.lv/en/elections/saeima"
    )
  )


# ----- Hill -----
# EEUU
hill <-
  tibble(method = "Huntington-Hill", method_family = "divisors (PR)",
        region = "United States",
        type_elec = "none (federal apportionment only)", constituency = "national",
        constituency_type = NA,
        candidate_ranking = NA,
        threshold = NA,
        party_list = NA,
        notes = "Used every 10 years solely to apportion the 435 seats in the U.S. House of Representatives among the 50 states based on population. It is not used for intrastate elections",
        biblio = "https://www.ams.org/publicoutreach/feature-column/fcarc-apportion2"
)


# ----- Imperiali -----
imperiali <-
  tibble(method = "Imperiali",
    method_family = "divisors (PR)",
    region = "Italy",
    type_elec = "local",
    constituency = "municipal districts",
    constituency_type = "multi-member",
    candidate_ranking = "fixed by party",
    threshold = NA,
    party_list = "closed",
    notes = "Imperiali method used in municipal elections; favors larger parties more than D'Hondt.",
    biblio = "https://www.normattiva.it/uri-res/N2Ls?urn:nir:stato:decreto.legislativo:2000;267"
  )

# ----- FPTP -----
fptp <-
  tibble(
    method = "First-Past-The-Post (FPTP)",
    method_family = "plurality/majority",
    region = "United Kingdom",
    type_elec = "congress",
    constituency = "electoral district",
    constituency_type = "single-member",
    candidate_ranking = NA,
    threshold = 0.00,
    party_list = NA,
    notes = "Used for electing members of the House of Commons.",
    biblio = c("https://consoc.org.uk/the-constitution-explained/electoral-systems/", "https://www.electoral-reform.org.uk/voting-systems/types-of-voting-system/first-past-the-post/")) |>
  bind_rows(
    tibble(
      method = "First-Past-The-Post (FPTP)", method_family = "plurality/majority", region = "Canada",
      type_elec = "congress", constituency = "electoral district",
      constituency_type = "single-member", candidate_ranking = NA,
      threshold = 0.00,
      party_list = NA,
      notes = "Used for federal parliamentary elections",
      biblio = "https://www.elections.ca/")
  ) |>
  bind_rows(
    tibble(
      method = "First-Past-The-Post (FPTP)",
      method_family = "plurality/majority",
      region = "India",
      type_elec = "congress",
      constituency = "electoral district",
      constituency_type = "single-member",
      candidate_ranking = NA,
      threshold = 0.0,
      party_list = NA,
      notes = "Used for elections to the Lok Sabha",
      biblio = "https://eci.gov.in/"
    )
  ) |>
  bind_rows(
    tibble(
      method = "First-Past-The-Post (FPTP)",
      method_family = "plurality/majority",
      region = "United States",
      type_elec = "congress",
      constituency = "electoral district",
      constituency_type = "single-member",
      candidate_ranking = NA,
      threshold = 0.0,
      party_list = NA,
      notes = "Used in House elections; one member elected per district",
      biblio = "https://www.fec.gov/"
    )
  ) |>
  bind_rows(
    tibble(
      method = "First-Past-The-Post (FPTP)",
      method_family = "plurality/majority",
      region = "Bangladesh",
      type_elec = "congress",
      constituency = "electoral district",
      constituency_type = "single-member",
      candidate_ranking = NA,
      threshold = 0.0,
      party_list = NA,
      notes = "Used to elect members of the Jatiya Sangsad (National Parliament)",
      biblio = "https://www.ecs.gov.bd/"
    )
  ) |>
  bind_rows(
    tibble(
      method = "First-Past-The-Post (FPTP)",
      method_family = "plurality/majority",
      region = "Zambia",
      type_elec = "congress",
      constituency = "electoral district",
      constituency_type = "single-member",
      candidate_ranking = NA,
      threshold = 0.0,
      party_list =NA,
      notes = "Used for electing members of the National Assembly by majority vote in single-member districts",
      biblio = "https://aceproject.org/epic-en/CDCountry?country=ZM"
    )
  )


# ----- Quotas -----
# quotas: Hare, Lowndes, Droop, Hangenbach Bischoff
switzerland <-
  tibble(
  method = "Hagenbach-Bischoff",
  method_family = "quotas (PR)",
  region = "Switzerland",
  type_elec = "congress",
  constituency = "prov (canton)",
  constituency_type = "multi-member",
  candidate_ranking = "voter ranking",
  threshold = NA,
  party_list = "open",
  notes = "Used for federal elections to the National Council with open lists and voter-modifiable candidate order",
  biblio = "https://www.bk.admin.ch/"
)



# ----- stv -----

stv <-
  tibble(
    method = "Single Transferable Vote (STV)",
    method_family = "preference transfer (PR)",
    region = "Ireland",
    type_elec = "congress",
    constituency = "electoral district",
    constituency_type = "multi-member",
    candidate_ranking = "voter ranking",
    threshold = NA,
    party_list = "no lists (individual candidates)",
    notes = "Uses Droop quota with preferential votes; multi-member districts",
    biblio = "https://www.oireachtas.ie/en/how-parliament-is-run/elections-and-voting/") |>
  bind_rows(
    tibble(
      method = "Single Transferable Vote (STV)",
      method_family = "preference transfer (PR)",
      region = "Malta",
      type_elec = "congress",
      constituency = "electoral district",
      constituency_type = "multi-member",
      candidate_ranking = "voter ranking",
      threshold = NA,
      party_list = "no lists (individual candidates)",
      notes = "Uses Droop quota with preferential votes; 13 districts elect 5 MPs each",
      biblio = "https://electoral.gov.mt/Elections/General"
    )
  ) |>
  bind_rows(
    tibble(
      method = "Single Transferable Vote (STV)",
      method_family = "preference transfer (PR)",
      region = "Australia",
      type_elec = "senate",
      constituency = "national",
      constituency_type = "multi-member",
      candidate_ranking = "voter ranking",
      threshold = NA,
      party_list = "open",
      notes = "Uses STV with quota for multi-member Senate elections; voters rank candidates",
      biblio = "https://www.aec.gov.au/voting/counting/senate-count.htm"
    )
  ) |>
  bind_rows(
    tibble(
      method = "Alternative Vote (Instant-Runoff Voting)",
      method_family = "majoritarian",
      region = "Australia",
      type_elec = "congress",
      constituency = "electoral district",
      constituency_type = "single-member",
      candidate_ranking = "voter ranking",
      threshold = NA,
      party_list = NA,
      notes = "Majoritarian system with ranked ballots. If no majority in first round, preferences redistributed until a candidate achieves majority",
      biblio = "https://www.aec.gov.au/Voting/How_to_vote/Voting_HOR.htm"
    )
  )






# ----- Mixtos -----



mixed <-
  tibble(method = "FPTP + Webster", method_family = "mixed-member proportional (MMPR)",
         region = "New Zealand",
         type_elec = "congress", constituency = "electoral district + national",
         constituency_type = "single-member + multi_member",
         candidate_ranking = "fixed by party",
         threshold = 0.05,
         party_list = "closed",
         notes = "5% threshold or at least one constituency seat. First seats are elected by FPTP (one per electoral district), the rest of them are allocated using Webster method through national party list",
         biblio = "https://www.ourcommons.ca/content/Committee/421/ERRE/Brief/BR8391757/br-external/2PedenR-e.pdf") |>
  bind_rows(
    tibble(
      method = "Two-Round Majority (prefilter + FPTP)",
      method_family = "mixed-member",
      region = "France",
      type_elec = "congress",
      constituency = "electoral district",
      constituency_type = "single-member",
      candidate_ranking = NA,
      threshold = NA,
      party_list = NA,
      notes = "Two-round majoritarian system. A candidate wins in the first round if they obtain over 50% of votes and at least 25% of registered voters. If not, a runoff is held among candidates with at least 12.5% of registered voters (or top two). In the second round, the candidate with the most votes wins",
      biblio = "https://www.idea.int/data-tools/country-view/103/40"
    )
  ) |>
  bind_rows(
    tibble(
      method = "Parallel (FPTP + Hondt)",
      method_family = "mixed parallel",
      region = "Japan",
      type_elec = "congress",
      constituency = "district + regional list",
      constituency_type = "mixed",
      candidate_ranking = "mixed",
      threshold = NA,
      party_list = "closed",
      notes = "465 seats: 289 elected by FPTP in single-member districts, 176 by D'Hondt in regional proportional blocks. No nationwide threshold",
      biblio = "https://www.idea.int/data-tools/country-view/153/40"
    )
  ) |>
  bind_rows(
    tibble(
      method = "Mixed parallel (FPTP + modified compensatory PR)",
      method_family = "mixed hybrid",
      region = "South Korea",
      type_elec = "congress",
      constituency = "district + national list",
      constituency_type = "mixed",
      candidate_ranking = "mixed",
      threshold = NA,
      party_list = "closed",
      notes = "300 seats: 253 by FPTP in single-member districts, 47 by proportional list (30 with compensatory formula)",
      biblio = "https://www.idea.int/data-tools/country-view/143/40"
    )
  ) |>
  bind_rows(
    tibble(
      method = "FPTP + modified Sainte-Lague modified",
      method_family = "mixed-member proportional (MMPR)",
      region = "Germany",
      type_elec = "congress",
      constituency = "district + state list",
      constituency_type = "mixed",
      candidate_ranking = "mixed",
      threshold = 0.05,
      party_list = "closed",
      notes = "598+ seats: half by FPTP in districts, half by party list with modified Sainte-Laguë divisor. 5% threshold or 3 seats by FPTP to qualify",
      biblio = "https://www.bundeswahlleiter.de"
    )
  ) |>
  bind_rows(
    tibble(
      method = "Mixed parallel (FPTP + Hondt)",
      method_family = "mixed parallel",
      region = "Italy",
      type_elec = "congress",
      constituency = "district + national list",
      constituency_type = "mixed",
      candidate_ranking = "party fixed",
      threshold = 0.03,
      party_list = "closed",
      notes = "Roughly 37% of seats by FPTP, 61% by D'Hondt proportional representation, rest reserved for overseas",
      biblio = "https://www.interno.gov.it"
    )
  ) |>
  bind_rows(
    tibble(
      method = "Mixed parallel (FPTP + D'Hondt)",
      method_family = "mixed parallel",
      region = "Mexico",
      type_elec = "congress",
      constituency = "district + regional list",
      constituency_type = "mixed",
      candidate_ranking = "party fixed",
      threshold = 0.03,
      party_list = "closed",
      notes = "500 seats: 300 by FPTP in single-member districts, 200 by D'Hondt in regional lists. 3% threshold for PR allocation.",
      biblio = "https://www.ine.mx"
    )
  ) |>
  bind_rows(
    tibble(
      method = "Mixed parallel (FPTP + Hare quota)",
      method_family = "mixed parallel",
      region = "Russia",
      type_elec = "congress",
      constituency = "district + national list",
      constituency_type = "mixed",
      candidate_ranking = "party fixed",
      threshold = 0.05,
      party_list = "closed",
      notes = "450 seats: 225 by FPTP in single-member districts, 225 by national list using Hare quota. 5% threshold",
      biblio = "https://www.cikrf.ru"
    )
  )


# ----- collect -----

apportionment_methods <-
  hondt |>
  bind_rows(webster) |>
  bind_rows(hill) |>
  bind_rows(imperiali) |>
  bind_rows(fptp) |>
  bind_rows(stv) |>
  bind_rows(mixed)

# ----- UTF-8 -----

apportionment_methods <-
  apportionment_methods |>
  mutate(across(where(is.character), \(x) enc2utf8(x)))


# ----- use data -----

usethis::use_data(apportionment_methods, overwrite = TRUE,
                  compress = "xz")
