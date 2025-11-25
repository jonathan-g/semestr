# Update LaTeX style files

Update LaTeX style files in content folders, copying from planning
folder.

## Usage

``` r
update_latex_styles(
  root_dir = NULL,
  content_path = "content",
  planning_path = "planning"
)
```

## Arguments

- root_dir:

  The root directory for the project (Should be the root for a HUGO
  project).

- content_path:

  Where to look for content (e.g., `.Rmd` and `.md` files).

- planning_path:

  Where to look for planning files (typically, in the same directory
  where the semester database is stored.).
