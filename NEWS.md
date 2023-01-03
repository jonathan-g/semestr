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
