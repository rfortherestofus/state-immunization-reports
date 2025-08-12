# State Immunization Reports

### How the report works

The report file (`report.qmd`) relies on a Typst template that lives in `typst-template.typ` and uses many functions defined in the latest for easier styling. For instance, the following snippet adds a source section at the bottom right in the current element, and adds some vertical margin:

````qmd
```{=typst}
#source("Source: SchoolVaxView")
#v(7pt)
```
````

Current functions:

- `#source()`: adds the source (some text) at the bottom right of an element
- `#connected-boxes()`: 2 boxes horizontally connected by a blue line
- `#blueline()`: adds an horizontal blue line with 100% width
- `#status-boxes()`: 2 boxes on top of each other, with text inside each of them
- `#chart-title()`: title for a group of charts

Other functions used (such as `#v()`) are built-in with Typst.

A very small amount of HTML is used for grey background sections. Note that the vast majority of html/css is **not** supported here. Background colors are [one of the few things](https://quarto.org/docs/advanced/typst/typst-css.html#supported-elements-and-properties) Quarto natively translate to typst.

`render.R` generates all PDF (one per state) and store them in `reports/`, as well as create a zip with all the PDFs at the root of the directory.
