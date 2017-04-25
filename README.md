
<!-- README.md is generated from README.Rmd. Please edit that file -->
emo(ji)
=======

The goal of emo(ji) is to make it very easy to insert emoji into RMarkdown documents.

Emoji data from <https://github.com/muan/emojilib/>.

Installation
------------

You can install emo from github with:

``` r
# install.packages("devtools")
devtools::install_github("emo/hadley")
```

Example
-------

You can either refer to emoji by their name (which is unique):

``` r
emo::ji("poop")
```

💩

Or by a keyword. Keywords are not unique so emo::ji will pick one for you at random.

``` r
emo::ji("face")
```

😄

``` r
emo::ji("face")
```

😲

``` r
emo::ji("face")
```

😣

Often you'll use inline like `r emo::ji("smile")`
