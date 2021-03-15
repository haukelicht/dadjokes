# dadjokes: R package to tell dad jokes

Function `tell_joke()` queries the dad jokes API at https://www.fatherhood.gov and prints joke(s) to the console.

## Installation

```r
remotes::install_github("haukelicht/dadjokes")
```

## Usage

```r
library(dadjokes)

# tell a dad joke 
tell_joke()

# tell multiple dad jokes 
tell_joke(n.jokes = 3)

# return joke(s) as character vector
jokes <- tell_joke(as.character = TRUE)

# don't shuffle jokes
tell_joke(random = FALSE)
```
