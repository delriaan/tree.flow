library(purrr); 
library(magrittr); 
library(data.table);

arg_list <- list(name = 'a', def = quote(values >= 30), desc = "First desc")
arg_list2 <- list(list(name = 'b', def = quote(values < 100), desc = "Next desc"), list(name = 'c', def = quote((values < 100)|and(values >= 100, cost <= 10000)), desc = "Last desc"))
arg_tabular <- reduce(list(arg_list, arg_list2[[1]], arg_list2[[2]]), rbind) %>% as.data.table()

func <- function(..., type = rep.int("list", ...length()), rem = rep.int(TRUE, ...length())){ 
  pmap(list(rlang::list2(...), type, rem), ~{
    purrr::when(
      ..2
      , . == "list" ~{ rlang::new_box(append(..1, list(rem = ..3))) }
      , . == "nlist" ~{ .val = ..3; map(..1, ~{ .x$rem <- .val; .x }) }
      , ~ { rlang::new_box(cbind(..1, rem = ..3)) }
    )
  }) %>% purrr::flatten() %>% rbindlist();
}

inspect <- func(list(name = 'a', def = quote(values >= 30), desc = "First desc"), arg_list2, arg_tabular, type = c("list", "nlist", "tabular"), rem = c(T,F,F))

inspect[[1]]
inspect[[2]]
inspect[[3]]
inspect[[4]]
