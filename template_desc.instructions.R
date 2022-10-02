# Copy this file into the package project root; edit as desired before executing
this.desc <- desc::desc();
this.desc$print();

people = {
	list(
		Chionesu = person(
			given = "Chionesu"
			, family = "George"
			, email = "cgeorge@alliancehealthplan.org"
			, role = c("aut", "cre")
		), Heather = person(
				given = "Heather"
				, family = "Copley"
				, email = "hcopley@alliancehealthplan.org"
				, role = c("aut", "cre")
			)
			, DScience = person(
				given = "Alliance Health Data Science Department"
				, email = "DScience@alliancehealthplan.org"
			)
		);
}
this.desc$set("Author", people$Chionesu)
this.desc$set("Maintainer", people$DScience)
this.desc$set("License", "Unicorn Overlords")
this.desc$set("Description", )
this.desc$set("Title", "Decision Support Tree")
this.desc$set_version("1.0.0")

# Set the dependencies
sapply(
  c("R6","purrr","furrr","data.table","magrittr","foreach","future","doFuture","igraph","visNetwork","htmlwidgets")
  , function(i, x){ x(package = i, type = "Imports") }
  , this.desc$set_dep
  )

this.desc$write()
this.desc$print()

