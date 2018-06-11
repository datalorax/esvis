files <- list.files(pattern = "README-")

file.copy(files, file.path("docs", files), overwrite = TRUE)

rm <- glue::glue("sed -i -e 's/!\\[\\]({file})//g' README.md", 
                file = files)
purrr::walk(rm, system)

file.remove(c(files, "README.md-e"))
