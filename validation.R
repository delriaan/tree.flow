# ::::: UNIT TESTS :::::
library(magrittr);
library(future);
library(data.table);
library(kableExtra); 
library(htmltools);
library(htmlwidgets);
library(stringi);
library(visNetwork);
library(purrr);
library(decision.support.tree)
#
unit.test <- function(label, action, test, action.env = globalenv(), test.env = action.env, reset = FALSE){
#' Create a Unit Test
#' 
#' \code{unit.test()} creates an active binding in the environment specified containing an invokeable function taking arguments passed from the call to each unit test binding.
#' 
#' Global objects \code{test.plan} and \code{test.results} are created at the first invocation of \code{unit.test()} or if they are missing in the environmet:
#' \itemize{
#'  \item test.plan is an environment and holds all unit tests that are created.  Attribute 'test.results' is attached to the environment
#'  \item test.results is an attribute of \code{test.plan}, and is incrementally populated at each execution of a unit test.
#' }
#'
#'
#' @param label (string) A label for the test
#' @param action An expression to execute 
#' @param test An expression to test some condition of the output of \code{action}
#' @param action.env (environment) The environment in which \code{action} will take place
#' @param test.env (environment | \code{action.env}) The environment in which \code{test} will take place
#' @param reset (logical) When \code{TRUE}, global object \code{test.plan} is recreated
#' 
#' @return None
#'
  if (reset | !exists("test.plan", envir = globalenv())){ assign("test.plan", setattr(new.env(), "test.results", list(c(NA))) , envir = globalenv()) }
  
  if (exists(label, envir = test.plan)){ rm(list = label, envir = test.plan) }
  
  makeActiveBinding(
    sym = paste(stringi::stri_pad_left(length(ls(test.plan)), width = 2, pad = "0"), label, sep = "_")
    , fun = function(.actEnv = action.env, .testEnv = test.env, .label = label){ 
        cat(sprintf("Executing unit test: %s", .label), sep = "\n");
      
        eval(action, envir = .actEnv);
      
        result = eval(test, envir = .testEnv);
      
        list(label = .label, action = action, test = test, result = result); 
      }
    , env = test.plan
    );
}
#
execute.plan <- function(q_idx, ...){
#' Execute the Unit Test Plan
#' 
#' All 'idx' variables should be positive integers as these are indexes. 'q_idx' may be '0' as this is accepted for iterators.
#' 
#' @param q_idx (integer) The 1-based index to which iterator 'test.queue' is set
#' @param ... (integers) Iterator indices to execute: other indices are skipped
  
  if (!missing(q_idx)){ test.queue$state$i = max(c(1, q_idx)) - 1 }
  
  # Index filter
  idx_fltr = if (missing(...)){ 1:test.queue$length } else { c(...) };
  idx_fltr %<>% .[. > 0];
  
  while(and(!(test.queue$state$i + 1) %in% idx_fltr, test.queue$state$i < test.queue$length)){ 
    test.queue$state$i = test.queue$state$i + 1 
  }
  iterators::nextElem(test.queue) %>% eval(envir = globalenv());
}
#
# CREATE UNIT TESTS ====
unit.test(
  "Create Tree"
  , quote({
      my.tree <<- { decision.support.tree$new(
        name = "My Decision Tree"
        , init.values = as.data.table(datasets::mtcars)[, mdl := sample(outer(LETTERS, letters, paste0), .N)][]
        , init.key = "mdl"
        )} 
    })
  , quote(exists("my.tree"))
 );

# :: make decision rules
.quote <- quote({ (length(my.tree$rule.book) == 6) & identical(names(my.tree$rule.book), paste("Rule", LETTERS[1:6], sep = " ")) });
#
unit.test("Ruleset_as_multi_call_list"  , quote({ # Rules as individual lists ...
  my.tree$manage.rules(
    list(name = "Rule A", def = quote(mpg >= 15)                       , desc = "Rule Description")
    , list(name = "Rule B", def = quote(mpg < 30)                      , desc = "Rule Description")
    , list(name = "Rule C", def = quote(mpg >= 20)                     , desc = "Rule Description")
    , list(name = "Rule D", def = quote((mpg %% 7) == 0)               , desc = "Rule Description")
    , list(name = "Rule E", def = quote((mpg >= 20) | (mpg == 310))    , desc = "Rule Description")
    , list(name = "Rule F", def = quote((mpg < 21) | (sqrt(mpg) < 30)) , desc = "Rule Description")
    , rtype = "list")
  }), .quote);
#
  
unit.test("Ruleset_as_nested_list"      , quote({ # Rules as nested lists ...
  list(
      rule_1 = list(name = "Rule A", def = quote(mpg >= 15)                     , desc = "Rule Description")
    , rule_2 = list(name = "Rule B", def = quote(mpg < 30)                      , desc = "Rule Description")
    , rule_3 = list(name = "Rule C", def = quote(mpg >= 20)                     , desc = "Rule Description")
    , rule_4 = list(name = "Rule D", def = quote((mpg %% 7) == 0)               , desc = "Rule Description")
    , rule_5 = list(name = "Rule E", def = quote((mpg >= 20) | (mpg == 310))    , desc = "Rule Description")
    , rule_6 = list(name = "Rule F", def = quote((mpg < 21) | (sqrt(mpg) < 30)) , desc = "Rule Description")
    ) %>% (my.tree$manage.rules)(rem = as.list(rep.int(FALSE, length(.))), rtype = "nlist") 
  }), .quote);
#

unit.test("Ruleset_as_workspace_tabular", quote({ # Rules as tabular dataset
    list(
      list(name = "Rule A", def = quote(mpg >= 15)                     , desc = "Rule Description")
    , list(name = "Rule B", def = quote(mpg < 30)                      , desc = "Rule Description")
    , list(name = "Rule C", def = quote(mpg >= 20)                     , desc = "Rule Description")
    , list(name = "Rule D", def = quote((mpg %% 7) == 0)               , desc = "Rule Description")
    , list(name = "Rule E", def = quote((mpg >= 20) | (mpg == 310))    , desc = "Rule Description")
    , list(name = "Rule F", def = quote((mpg < 21) | (sqrt(mpg) < 30)) , desc = "Rule Description")
    ) %>% reduce(rbind) %>% (my.tree$manage.rules)(rem = rep.int(FALSE, nrow(.)), rtype = "tabular")
}), .quote);
#
  
unit.test("Ruleset_as_read_file_tabular", quote({ # Rule set read from file as tabular dataset
  fread(file = "ruleset.txt", header = TRUE, check.names = TRUE, fill = TRUE, blank.lines.skip = TRUE) %>% 
    (my.tree$manage.rules)(rem = rep.int(FALSE, nrow(.)), rtype = "tabular", .debug = TRUE)
}), .quote);
#

unit.test(
  "Make_Branches"
  , quote({
      my.tree$add.branches(my.tree$root, true.label = "A", false.label = "B", rule = "Rule A");
      my.tree$goto("A")$`#`$add.branches(true.label = "C", false.label = "D", rule = "Rule D");
      my.tree$add.branches(branch = "B", true.label = "E", false.label = "F", rule = "Rule F");
      my.tree$goto("C")$`#`$add.branches(true.label = "G", false.label = "H", rule = "Rule C");
      my.tree$goto("D")$`#`$add.branches(true.label = "I", false.label = "J", rule = "Rule C");
    })
  , quote({ identical(setdiff(names(my.tree$list.branches()), c(LETTERS[1:10], "root")), character(0)) })
  );
#
  
unit.test(
  "Populate_Tree_0"
  , quote(my.tree$populate(branch = "root", .debug = TRUE))
  , quote({
      my.tree$plot(
        igraph.args = list(physics = TRUE)
        , layout.args = list(visHierarchicalLayout = list(direction = "UD", sortMethod = "directed"))
        ) %>% 
        visNetwork::visNodes(title = map(my.tree$list.branches(), ~.x$leaf.count) %>% unlist(use.names = FALSE)) %>% print
      askYesNo("Enter the result of the test: ", prompts = "Pass/Fail/Other")
    })
  );
  
#
unit.test(
  "Manage_Populate_Tree_1"
  , quote({ 
      my.tree$goto("B")$rule 
      my.tree$manage.rules(
        data.table(
          name = c("Rule F", "Rule D")
          , def = c(quote((mpg < 20) | (sqrt(mpg) > 20)), quote((mpg %% 7) == 0))
          , desc = c("Rule F Description <MODIFIED>", "Rule Description <MODIFIED>")
          )
        )
      my.tree$set.branch(new.rule = "Rule F", new.name = "BBC");
      # Re-populate tree after management []
      my.tree$populate(); 
    })
  , quote({
      my.tree$plot(
        igraph.args = list(physics = TRUE)
        , layout.args = list(visHierarchicalLayout = list(direction = "UD", sortMethod = "directed"))
        ) %>% 
        visNetwork::visNodes(title = map(my.tree$list.branches(), ~.x$leaf.count) %>% unlist(use.names = FALSE)) %>% print
      askYesNo("Enter the result of the test: ", prompts = "Pass/Fail/Other")
    })
  );

#
test.queue <- iterators::iter(purrr::map(ls(test.plan), ~{
    substitute(setattr(test.plan, "test.results", { x = attr(test.plan, "test.results"); x[[i]] <- test.plan[[i]]$result; na.omit(x); }), list(i = .x))
  }) %>% as.vector());
  
ls(test.plan); 

# EXECUTE TEST PLANS ====
rm(list = purrr::keep("my.tree", exists, envir = globalenv()))
assign("i", 0, envir = test.queue$state);

for (k in 1:length(ls(test.plan))){ execute.plan() }

# ADDITIONAL TESTS ====
attr(test.plan, "test.results") %>% { data.table(test.name = names(.), test.result = .) }
my.tree$rule.book %>% map(~.x$def %>% as.character())
my.tree$goto('C'); my.tree$leaves
my.tree$del.branch('C')

my.tree$goto('C'); my.tree$leaves

my.tree$graph %>% igraph::neighborhood()
my.tree$goto('A') %$% ls()
my.tree$populate()

my.tree$graph %>% igraph::neighborhood()


my.tree$plot(refresh= TRUE)
my.tree$.__enclos_env__$private$vizArgs
