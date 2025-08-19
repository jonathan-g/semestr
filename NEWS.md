# semestr 0.4.0

* Add support for GitHub classroom for homework and lab assignments.
* Consult `config` table in database to decide whether to issue
  warnings for empty lab, homework, and reading tables in the 
  database.
* Consult `uses_gh_classroom` variable in `config` table to decide
  whether to include text about the GitHub Classroom assignment in
  homework and lab assignment pages.
* Include a field `rd_empty_grp` in `reading_groups` table to allow
  consistency checks for empty reading assignments.

# semestr 0.3.0

* Add support for standalone reading handouts, modeled on lab assignment
  documents.
* Refactor handling of paths to put paths for .Rmd content files, the 
  resulting .html and .pdf files, and URLs (relative to the web root)
  to access them. These are now specified in the database in a new table
  called "file_paths".

# semestr 0.2.2

* Add support for expansion codes in markdown content to reference future
  and past classes and reading assignments using new functions 
  `lookup_future_class`, `lookup_past_class`, `lookup_future_reading`,
  and `lookup_past_reading`.

# semestr 0.2.1

* Add support for self-study exercises
* Updated badges
* Fixed pkgdown configuration
* Add control for toc depth in RMarkdown output.

# semestr 0.2.0

* Add database column `hw_self_assess` to the table `homework_items` and 
  adjust homework assignment formatting to separate self-study items that
  students should not turn in from items they should turn in.

# semestr 0.1.4

* Use relative paths for PDF output when rendering PDF files. 
  This addresses a problem with knitr or Pandoc putting paths into 
  \include_graphics that include illegal characters for LaTeX.

# semestr 0.1.3

* Make PDFs for reading assignments.

# semestr 0.1.2

* Fix some problems with homework formatting.
* Add NEWS.md to track changes.

# semestr 0.1.1

* Fix formatting of lab assignments. 
* Fix formattting for EES 4760.

# semestr 0.1.0

* Refactor to use new database schema for 2021.

# semestr 0.0.0

* First working version
