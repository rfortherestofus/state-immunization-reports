#let article(
  title: none,
  date: none,
  doc,
) = {
  set text(lang: "en", region: "US", font: ("Roboto", "Arial"), size: 12pt)

  set page(paper: "us-letter", margin: (x: 0.5in, bottom: 1in, top: 0.5in), number-align: center, footer: {
    block(width: 100%, [
      #pad(top: 2pt, grid(
        columns: (1fr, auto, 1fr),
        align(left)[#text(upper(title))], align(center)[], align(right)[#text(date)],
      ))
    ])
    rect(
      width: 100%,
      height: 100%,
      outset: (x: 10%),
      fill: blue,
    )
  })

  show heading: it => {
    let sizes = (
      "1": 17pt, // Heading level 1
      "2": 15pt, // Heading level 2
      "3": 13pt, // Heading level 3
      "4": 11pt, // Heading level 4
    )
    let level_str = str(it.level)
    let size = if level_str in sizes { sizes.at(level_str) } else { 10pt }
    set text(size: size)

    it
  }

  doc
}
