bda <- read_excel("data/anuario/dados_ajustados.xlsx") |> janitor::clean_names()
length(2009:2021)

bda |> group_by(pais) |> count(sort = TRUE) |> View()

bda |> group_by(pais) |> count(sort = TRUE) |> write.table("clipboard", 
            quote = FALSE,
            sep = "\t",
            na = "",
            dec = ",",
            row.names = FALSE)

bdpg <- read_excel("data/anuario/pais_grupo.xlsx") |> janitor::clean_names()
          
