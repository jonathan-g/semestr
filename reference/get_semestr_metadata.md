# Get the metadata for the currently loaded semester

Pulls the metadata for the currently loaded semester from the package's
`.globals` environment

## Usage

``` r
get_semestr_metadata()
```

## Value

A named list with the metadata

## Examples

``` r
get_semestr_metadata()
#> $type2idx
#>      class        lab   homework   due date       exam    holiday      event 
#>    "class"      "lab" "homework" "due_date"     "exam"  "holiday"    "event" 
#> 
#> $type2col
#>     class       lab  homework  due date      exam   holiday     event 
#>   "class"     "lab"      "hw"     "due"    "exam" "holiday"     "evt" 
#> 
#> $idx2type
#>      class        lab   homework   due_date       exam    holiday      event 
#>    "class"      "lab" "homework" "due date"     "exam"  "holiday"    "event" 
#> 
#> $col2type
#>      class        lab         hw        due       exam    holiday        evt 
#>    "class"      "lab" "homework" "due date"     "exam"  "holiday"    "event" 
#> 
#> $idx2col
#>     class       lab  homework  due_date      exam   holiday     event 
#>   "class"     "lab"      "hw"     "due"    "exam" "holiday"     "evt" 
#> 
#> $col2idx
#>      class        lab         hw        due       exam    holiday        evt 
#>    "class"      "lab" "homework" "due_date"     "exam"  "holiday"    "event" 
#> 
#> $prefixes
#>    class      lab homework due date     exam  holiday    event 
#>    "CLS"    "LAB"     "HW"    "DUE"   "EXAM"    "VAC"    "EVT" 
#> 
#> $bases
#>    class homework      lab due date     exam  holiday    event 
#>     1000     2000     3000     4000     5000     6000     7000 
#> 
#> $rev_base
#>       1000       2000       3000       4000       5000       6000       7000 
#>    "class" "homework"      "lab" "due date"     "exam"  "holiday"    "event" 
#> 
#> $mods
#> canceled  make_up 
#>      100      200 
#> 
#> $rev_mods
#>        100        200 
#> "canceled"  "make_up" 
#> 
```
