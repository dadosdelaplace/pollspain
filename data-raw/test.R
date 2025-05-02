# import
depop_mun <-
  tibble("cod_INE_mun" = ...,
         "old_mun" =
           c("Darrical", "Villar del Maestre", "Vicolozano",
             "Hinojar del Rey", "Sedano", "Pedrosa del Rey",
             "Carbajosa de Armuña", "Fornillos de Fermoselle",
             "Oza de los Ríos", "Cesura",
             "Neira de Jusa", "Cerdedo", "Cotobad",
             "Iruña (Trespuentes)", "Beninar", "Aínsa",
             "Alcorlo", "Avellanosa del Páramo",
             "Torrecilla del Ducado", "Pesquera de Ebro",
             "Puras de Villafranca", "Castellanos de Villiquera",
             "Mata de Armuña", "Villorobe", "Fresnedo",
             "Palmerola", "Gátova", "Carpio de Medianero",
             "Alamedilla del Berrocal", "Urraca Miguel",
             "Narrillos de San Leonardo", "Bernuy-Salinero"),
         "mun" =
           c("Alcolea", "Villar y Velasco", "Ávila",
             "Huerta del Rey", "Valle de Sedano", NA,
             "Castellanos de Villiquera", "Villar del Buey",
             "Oza-Cesuras", "Oza-Cesuras",
             "Baralla", "Cerdedo-Cotobade", "Cerdedo-Cotabade",
             "Iruña de Oca", "Berja", "Aínsa-Sobrarbe",
             "La Toba", "Santibáñez-Zarzaguda",
             "Sienes", "Valle de Sedano",
             "Belorado", "Castellanos de Villiquera",
             "Castellanos de Villiquera", "Villasur de Herreros", "Cubillos del Sil",
             "Les Llosses", "Gátova", "Diego del Carpio",
             "Ávila", "Ávila", "Ávila", "Ávila"))
# CASTELLANOS DE VILLIQUERA --> 093 a 185
# Gatova --> ¡de castellón a valencia wtf!


poll_data_1982 <-
  import_poll_station_data(type_elec = "congress", year = 1982,
                           month = 10, short_version = FALSE)
