#' Tree Flow
#' @description Tree Flow is an R6 class that facilitates the creation and navigation of user-defined decision rules. 
#' \itemize{
#' \item 'Rules' are the \emph{prescriptive} decision points observed looking visually at a typical flow diagram.  The class provides methods for managing rules (i.e., adding, redefining, removing)
#' \item Each branch of a split is structured in an environment containing the following members:
#'   \describe{
#'    \item{$values}{ The values beloning to the current branch: ultimately a subset of the values of "root"}
#'    \item{$name}{ The name associated with the current branch}
#'    \item{$aliases}{ The alternative names associated with the current branch}
#'    \item{$leaf.nodes}{ The distinct count of \code{values:key}}
#'    \item{$rule}{ The quote expression to be applied to $values during tree population}
#'    \item{$prev}{ A pointer to the previous branch (\code{$prev})}
#'    \item{$false}{ A pointer to the child branch containing values that evaluate as \code{FALSE} when the branch rule is applied}
#'    \item{$true}{ A pointer to the child branch containing values that evaluate as \code{TRUE} when the branch rule is applied}
#'    \item{$`#`}{ A pointer to the class environment: this allows certain global methods to be accessed via chaining with the '$' accessor.}
#'  } 
#'  \cr\cr Removing a branch (i.e., setting the branch to \code{NULL}) removes all child branches as well
#' }
#' @export
tree.flow <- { R6::R6Class(
  classname = "tree.flow" 
  , public  = { list(
      #' @field tree.name The name of the tree
      tree.name = NULL, 
      
      #' @field root The root of the tree
      root = NULL, 
      
      #' @field rule.book The list of decision rules
      rule.book = list(), 
      
      #' @field id The index of the current branch
      id = 0, 
      
      #' @field tree.key The \code{\link[data.table]{data.table}} key column name
      tree.key = NULL, 
      
      #' @field graph The \code{\link[igraph]{igraph-package}} representation of the tree
      graph = NULL, 
      
      # initialize() ----
      #' @description 
      #' Initialize a new tree
      #' 
      #' @param init.values The values with which the tree is to be populate: must be a list (each member of the list must be the same length) or named dimensional object
      #' @param init.key The element or column name from \code{init.values} to serve as the \code{\link[data.table]{data.table}} key to be used for counting elements in a branch
      #' @param name The desired name of the decision support tree 
      initialize = function(init.values, init.key, name){ 
        # Check for input of class "list", "data.frame", "data.table", "tibble"
        if (length(intersect(class(init.values), c("list", "data.frame", "data.table", "tibble"))) == 0 ){
          stop("'init.values' must be a list or named dimensional object, and a key ('init.key') must be supplied.");
          } else {
              if (sapply(init.values, length) %>% unique %>% length > 1){ stop("'init.values' must be of the same length")}  
              if (is.null(init.key)){ stop("When 'init.values' is a list or named dimensional object, a value for 'init.key' must be provided.")}
            }
      
        self$tree.name = name;
        self$root = new.env();
        
        self$root$name    = "root"; 
        self$root$aliases = "Root"; 
          private$envs[["root"]] = self$root; 
        self$root$id      = 1;
        self$root$prev    = self$root; 
        self$root$`#`     = self$root;  
        self$root$values  = as.data.table(init.values) %>% setkeyv(init.key);
          self$tree.key   = init.key
          self$root$leaf.count = self$root$values[[self$tree.key]] %>% unique %>% length;
        self$root$is_end  = TRUE;
        self$root$rule    = NULL;
        self$root$false   = NULL;
        self$root$true    = NULL;
        self$root$make.decision = self$make.decision;
          
        private$vizArgs <- list(igraph.args = NULL, layout.args = list(visHierarchicalLayout = list()), actions = NULL);
        private$graph.engine <- substitute({ 
          .branches = self$list.branches();
          .graph = str2lang(sprintf(
            "igraph::graph_from_literal(%s)"
            , map(.branches, pluck, "name") %>% stringi::stri_replace_all_fixed(">", "+", vectorize_all = FALSE) %>% paste(collapse = ", ")
            )) %>% eval();
          
          igraph::V(.graph)$title <- map(.branches, pluck, "leaf.count") %>% unlist(use.names = FALSE) %>% sprintf(fmt = "Unique values: %s");
          igraph::V(.graph)$level <- igraph::distances(.graph, mode = "out") %>% .[1,] + 1;
          igraph::E(.graph)$label <- map(.branches, ~ sprintf("%s [%s]", deparse(.x$prev$rule$def), identical(.x$prev$true, .x) )) %>% unlist(use.names = FALSE);
          igraph::E(.graph)$title <- map(.branches, pluck, "rule", "desc") %>% unlist(use.names = FALSE);
          
          assign("graph", .graph, envir = self)
        })
      }, 

      # manage.rules() ----
      #' @description 
      #' Register and manage decision rules by operating on the class member \code{$rule.book}, the list of rules for use by branches.  It is up to the user to ensure that the rule expression is appropriate for the data type of the values used to initialize the class (argument \code{init.values} of class method \code{$new()})
      #'  
      #' @details 
      #' \emph{\code{this.rule}}
      #' \itemize{
      #'  \item \code{name}: A label for the rule
      #'  \item \code{def}: A expression operating on branch \code{values}, or a function accepting branch \code{value} as its first argument.  Either option must return a logical vector when evaluated or invoked.
      #'  \item \code{desc}: The plain-language description of the rule)
      #' }\cr When more than one rule is supplied to \code{'...'}, arguments \code{rem} and \code{rtype}, must be the same length as the number of supplied rules; otherwise, the first value in each argument is recycled to the length of \code{'...'}.
      #' 
      #' \emph{\code{rtype}}
      #' \itemize{
      #'  \item \code{list}    : \code{list(name = , def = , desc = )}
      #'  \item \code{nlist}   : \code{list(list(name = , def = , desc = ), list(name = , def = , desc = ), ...)}; a maximum depth of two is supported
      #'  \item \code{tabular} : \code{data{table, frame}(name = c(...), def = c(...), desc = c(...))}
      #' }
      #' 
      #' @param ... \code{\link[rlang]{dots_list}}: One or more lists with members \code{name}, \code{def}, and \code{desc}
      #' @param rem When \code{TRUE}, the rule is removed from the class member \code{$rule.book} (see section 'Details')
      #' @param rtype Denotes the type of structure for the argument \code{this.rule}; \code{list}, \code{"nlist"} (nested lists), and \code{tabular} (2D objects) are supported (see section 'Details').
      #' @param .debug (logical) When \code{TRUE}, additional execution information is produced
      #' 
      #' @return The class, invisibly
      manage.rules = function(..., rem = FALSE, rtype = "list", .debug = FALSE){ 
        if (...length() != length(rem)){ rem <- rep.int(rem[1], ...length()) }
        if (...length() != length(rtype)){ rtype <- rep.int(rtype[1], ...length()) }
        
        this.rule = list(rlang::list2(...), rtype = rtype, rem = rem) %>% 
          pmap(~{ 
            purrr::when(
              ..2
              , . == "list" ~{ rlang::new_box(append(..1, list(rem = ..3))) }
              , . == "nlist" ~{ .val = ..3; map(..1, ~{ .x$rem <- .val; .x }) }
              , ~ { rlang::new_box(cbind(..1, rem = ..3)) }
            )}) %>% purrr::flatten() %>% reduce(rbind);
        
        sub.fn = function(rule, action){
          if (.debug){ print(rule); print(action); }
          
          rule <- as.list(rule);
          
          # Check for missing arguments to the current function 
          check.args = c(missing(rule), !as.logical(action));
          
          if (check.args[1]){ 
            print(self$rule.book); 
            message("No rule provided: please provide at least one valid rule"); 
            return(invisible(self)); 
          } else if (check.args[2]){ 
            # Add/Update a rule if all arguments present, or abort if not
            if (any(is.null(rule[c("def", "desc", "name")]))){
              message("ERROR: All rules must include fields 'def', 'desc', and 'name'");
              return(invisible(self));
            }
            
            # Rule definitions must be saved as expressions, so check for strings (as would be the case for rules read from text files)
            if (is.character(rule$def)){ rule$def <- parse(text = rule$def) }
            self$rule.book[[rule$name]] <- rule;
          } else { # Remove a rule
            self$rule.book[[rule$name]] <- NULL;
            }
        }
        
        pwalk(as.data.table(this.rule), function(...){ sub.fn(rule = list(...)[c("def", "desc", "name")], action = list(...)[[which(...names() == "rem")]]) });
        
        return(invisible(self));
      }, 
      
      # add.branches() ----
      #' @description 
      #' Add empty objects to the current branch's 'true' and 'false' objects. The class object pointing to the current branch is set to the branch being modified.  Do not use this method to update branch values: this method will not overwrite an existing branch.  Use methods \code{$set.branch.name()} and \code{$set.branch.rule()}. 
      #' 
      #' @param branch The branch environment to which child branches are being added: either an object or the target branch's name.  
      #' @param false.label The assignment object "name" in the "false" branch
      #' @param true.label The assignment object "name" in the "true" branch
      #' @param final.label The value to assign the terminating node indicator in the current branch
      #' @param rule A string indicating which registered rule is to be assigned to the branch before adding child
      #' @param aliases A character vector of additional names by which the branch may be understood (e.g., "name_2", "name_3", etc.)
      #' 
      #' @return The branch, invisibly
      add.branches = function(
        branch        = self$curnt
        , false.label = "false"
        , true.label  = "true"
        , final.label = "END"
        , rule        = quote((TRUE))
        , aliases     = NULL
        ){
          branch = if (is.character(branch)){ private$envs[[branch]] } else { branch }
          
          # Check for duplicate beginnings of names
          true.label = {
            if (and(true.label != branch$name, !(true.label %in% names(private$envs)))){ true.label } else { 
                message("Names are identical or 'new.name' belongs to an existing branch: no action taken") 
                return(invisible(self$curnt))
                }
            }
          false.label = {
            if (and(false.label != branch$name, !(false.label %in% names(private$envs)))){ false.label } else { 
                message("Names are identical or 'new.name' belongs to an existing branch: no action taken") 
                return(invisible(self$curnt))
                }
            }
          
          # Set the tracking index to the branch being augmented
          private$curnt.idx = branch$id;
          
          # Set objects of the current branch 
          branch$rule     = self$rule.book[[rule]]; 
          branch$final    = final.label;
          branch$is_end   = FALSE;
          branch$aliases  = c(branch$aliases, aliases);
          branch$make.decision = self$make.decision;
          
          formals(branch$make.decision) <- substitute(alist(branch = .branch), list(.branch = branch)) %>% eval;
          
          branch$`#` <- self;
          
          # Add the 'true' and 'false' child branches for the current branch
          map(c("true", "false"), ~{
            label = eval(as.symbol(paste0(.x, ".label")));
            
            branch[[.x]] = new.env();
            branch[[.x]]$prev = branch; # Easiest way to navigate to the previous decision branch
            branch[[.x]]$`#` <- self;
            branch[[.x]]$is_end = TRUE;
            branch[[.x]]$name = sprintf("%s -> %s", self$curnt$name, label[[1]]);
            branch[[.x]]$aliases = c(label);
              private$envs[[label[[1 ]]]] <- branch[[.x]];
              
            # Placeholders
            branch[[.x]]$rule = NULL;
            branch[[.x]]$false = NULL;
            branch[[.x]]$true = NULL;
            
            branch[[.x]]$id = length(private$envs);
          });
          
          return(invisible(self$curnt));
        }, 
      
      # set.branch() ----
      #' @description 
      #' Replace the rule of the indicated branch
      #' 
      #' @param branch The target branch (by name or id) or, when NULL, the current branch 
      #' @param new.name The desired name to use for the branch 
      #' @param new.rule The desired rule to use for the branch 
      #' 
      #' @return The branch, invisibly
      set.branch = function(branch = self$curnt, new.name = self$curnt$name, new.rule = self$curnt$rule){
        branch = if (is.character(branch)){ private$envs[[branch]] } else { branch }
        
        { if (is.character(new.rule)){ self$rule.book[[new.rule]] } else { new.rule }} %>% {
          if (!identical(., branch$rule)){ branch$rule = . } else { message("Rules are identical: no action taken") }
        }
      
        if (and(new.name != branch$name, !(new.name %in% names(private$envs)))){ branch$name <- new.name; } else { 
            message("Names are identical or 'new.name' belongs to an existing branch: no action taken") 
            return(invisible(self$curnt))
          }
        
        private$curnt.idx = branch$id;
        return(invisible(self$curnt));
      }, 
      
      # del.branch() ----
      #' @description 
      #' Delete a branch: child branches are consequently removed
      #' 
      #' @param branch The target branch (by name or id) or, when NULL, the current branch 
      #' @param confirm (logical | TRUE) When \code{TRUE}, deletion confirmation is provided
      #' 
      #' @return Invisibly, the class environment with the current branch set to the root
      del.branch = function(branch = self$curnt, confirm = TRUE){
        branch = if (is.character(branch)|is.integer(branch)){ private$envs[[branch]] } else { branch }
        private$curnt.idx <- branch$prev$id;
        
        .action = quote({ 
          # Clear out the current branch from $private$envs and sabotage the parent's link
          branch$prev$is_end == TRUE;
          private$envs[[ branch$id ]] <- NA;
          
          if (branch$prev$true %>% identical(branch)){ branch$prev %$% rm(true) }
          if (branch$prev$false %>% identical(branch)){ branch$prev %$% rm(false) }
          
          if (!branch$is_end){ 
            # Move to the next non-Null branches on the current path
            if (!is.null(branch$true$id )){ self$del.branch(branch = branch$true$id , confirm = FALSE)}
            if (!is.null(branch$false$id)){ self$del.branch(branch = branch$false$id, confirm = FALSE)}
          }
          branch %$% rm(prev)
        });
        
        .nm = branch$name;
        .yn = NULL;
        
        if (confirm){ .yn <- askYesNo(sprintf("Delete '%s'?", .nm)) == TRUE }
        if ((.yn %||% TRUE) == TRUE){ eval(.action) }
        
        private$envs <- private$envs %>% .[!is.na(.)];
        
        eval(private$graph.engine);
        
        return(invisible(self));
      }, 
      
      # goto() ----
      #' @description 
      #' Jump to an arbitrary branch in the tree, setting the "current branch" pointer to the target branch
      #' 
      #' @param name The name (not alias) of the target branch 
      #' @param id The id value of the target branch 
      #' @param .verbose When \code{TRUE}, the current branch's ID and name are echoed
      #' 
      #' @return The current branch, invisibly
      goto = function(name = NULL, id = private$envs[[name]], .verbose = FALSE){ 
        idx = if (is.null(name)){ if (is.null(id)) self$curnt$id else id } else { name }
        
        private$curnt.idx = private$envs[[idx]]$id;
        if(.verbose){ with(self$curnt, message(sprintf("Current branch: [%s] %s", id, name))) }
        
        return(invisible(self$curnt));
      }, 
      
      # list.branches() ----
      #' @description 
      #' Print the structure of the private list of branches
      #' 
      #' @return The list of branches, invisibly
      list.branches = function(){ 
        print(private$envs %>% str); 
        return(invisible(private$envs));
      }, 
      
      # make.decision() ----
      #' @description 
      #' Apply a branch's rule to it's stored values and populate downstream 'true' and 'false' child branches. The branch is returned invisibly.
      #' 
      #' @param branch A class branch object, defaulting to the currently pointed to branch
      #' @param .debug Print debugging information 
      #' @return The branch, invisibly
      make.decision = function(branch = self$curnt, .debug = FALSE){
        count_leaves = function(i){ unique(i) %>% length }
        
        branch = if (is.character(branch)){ private$envs[[branch]] } else { branch }
        private$curnt.idx = branch$id;
        
        no_leaves = (branch$leaf.count == 0);
        
        # Overwrite the signature (logical vector) of the existing branch function via substituted `alist()
        result = if (no_leaves) { FALSE } else { branch$values[, eval(branch$rule$def) %>% as.vector] }
        
        if (.debug){ print(ls(branch) %>% mget(envir = branch)) }
        
        if (is.null(branch$true)){ return(invisible(branch)) }
        # Set leaf counts for current and populated child branches
        branch$true$values  = if (no_leaves) { branch$values } else { branch$values[(result) , .SD, keyby = c(self$tree.key)] }
        branch$false$values = if (no_leaves) { branch$values } else { branch$values[(!result), .SD, keyby = c(self$tree.key)] }
        
        invisible(map(c("true", "false"), ~{ 
          branch[[.x]]$leaf.count = if (no_leaves){ 0 } else { branch[[.x]]$values[[self$tree.key]] %>% count_leaves }
          }));
        
        return(invisible(branch));
      }, 
      
      # populate() ----
      #' @description 
      #'  Iteratively populate the tree by rendering decisions only on branches that have non-NULL "true" and "false" values.  
      #'  
      #' @details
      #'  For a given branch, the values that map to \code{TRUE} are copied to the current branch's corresponding member \code{$true} and vice-versa.  The values of the current branch are preserved for exploration branch-by-branch.  
      #'  
      #'  This is performed sequentially and recursively using a "first matching rule wins" method: this means that if the set of values for a branch \emph{could} be mapped to \code{TRUE} in multiple branches of the same depth, it will only map to \emph{one} branch due to the propagation of values described before. 
      #'  
      #'  \strong{Population begins at the root by default.}  Once populated a corresponding \code{\link[igraph]{igraph-package}} member \code{$graph} is created
      #'  
      #' @param branch The target branch (by name or id) or, when NULL, "root".  Iteration begins at this point in the tree.
      #' @param .debug Print debugging information 
      #' 
      #' @return The class environment, invisibly
      populate = function(branch = "root", .debug = FALSE){ 
        branch = self$make.decision(private$envs[[branch]], .debug = .debug); 
        
        map(c("true", "false"), ~if (!is.null(branch[[.x]]$id)){ self$populate(branch = branch[[.x]]$id, .debug = .debug) });
        
        invisible(eval(private$graph.engine))
        
        return(invisible(self));
      }, 
    
      # plot() ----
      #' @description 
      #' Creates an visualization of the tree using \code{\link[visNetwork]{visIgraph}}.  The branch list is converted into a graph via \code{\link[igraph]{graph_from_literal}}
      #' 
      #' @details For \code{...}, the expressions are executed in the order given.\cr 
      #' \itemize{
      #'  \item Use \code{g} as the symbol for the igraph object when needed (e.g., \code{ V(g) <- ... })
      #'  \item By default, vertices are supplied with a \emph{title} and \emph{level} attributes; edges with \emph{title} and \emph{label} attributes
      #'  \item \itemize{
      #'    \item Assignment to graph objects can use \code{<-}. The graph is automatically on each pass.  
      #'    \item To updated the entire graph, use the following: \code{ g <<- ... }
      #'    }
      #'  \item Library calls should be prefixed with \code{"igraph::"} unless the library has been loaded and attached
      #' }
      #' 
      #' The class member \code{$graph} is not updated unless the \code{ self$graph <<- ...} is used. 
      #' 
      #' @param ... \code{\link[rlang]{dots_list}}: igraph expressions that will customize the graph before sending to \code{\link[visNetwork]{visIgraph}}
      #' @param igraph.args A named list of arguments to send to \code{\link[visNetwork]{visIgraph}}
      #' @param layout.args A named list of length \code{1L} given as \code{list("layout name" = <argument list>)} (see \url{https://datastorm-open.github.io/visNetwork/layout.html} for details
      #' @param refresh (logical | FALSE) When \code{TRUE}, the preceding arguments are defaulted to existing values in \code{$private$vizArgs}
      plot = function(..., igraph.args = NULL, layout.args = list(visHierarchicalLayout = list()), refresh = FALSE){ 
        g <- self$graph;
        
        if (!refresh){ 
          private$vizArgs$layout.args <- layout.args;
          private$vizArgs$igraph.args <- igraph.args;
          private$vizArgs$actions <- rlang::exprs(...);
        }
        
        .actions <- rlang::exprs(...);
        if (length(.actions) > 0){ for (i in .actions){ eval(i) }}
        
        .viz = do.call(visNetwork::visIgraph, args = rlang::list2(igraph = g, !!!private$vizArgs$igraph.args));
        
        .layout.func = str2lang(sprintf("visNetwork::%s", names(private$vizArgs$layout.args))) %>% eval();
        
        do.call(.layout.func, args = rlang::list2(graph = .viz, !!!(private$vizArgs$layout.args[[1]])));
      }
    )
  }
  , private = list(envs = list(), vizArgs = NULL, graph.engine = NULL, curnt.idx = 1 )
  , active  = { list(
      #' @field total.branches Count the number of branches
      total.branches = function(){ return(length(private$envs) - 1) }, 
      
      #' @field depth Calculate tree depth
      depth = function(){ return(which(c(1:1000) > self$total.branches %>% log(2)) %>% min) }, 
       
      #' @field curnt Returns the current branch
      curnt = function(){ return(invisible(private$envs[[private$curnt.idx]])) }, 
      
      #' @field parent Goto the parent branch: this is similar to \code{$goto()} except it only moves to the parent branch of the current branch
      parent = function(){ 
          message(sprintf(
            "Current branch: [%s] %s"
            , private$envs[[private$curnt.idx]]$id
            , private$envs[[private$curnt.idx]]$name
            ));
          
          private$curnt.idx = self$curnt$prev$id;
          
          return(invisible(self$curnt));
        }, 
      
      #' @field leaves Return the content of the current branch object \code{values}
      leaves = function(){ self$curnt$values },
      
      #' @field tree Return the environment list for the tree
      tree = function(){ private$envs }
    )
  })
}
