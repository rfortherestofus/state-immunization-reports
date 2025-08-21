#import "@preview/titleize:0.1.1": titlecase

#let blueline() = {
  line(length: 100%, stroke: 2pt + rgb("#68ACE5"))
}

#let source(color: black, body) = {
  align(right)[#text(body, style: "italic", font: "Bitter", size: 9pt, fill: color)]
}

#let status-boxes(top-text: "", bottom-text: "") = {
  let bluerect = box(
    width: 100%,
    height: 0.7in,
    fill: rgb("#002D72"),
    inset: 6pt,
    align(center + horizon)[
      #text(fill: white, weight: "bold", size: 9pt)[#top-text]
    ],
  )

  let redrect = box(
    width: 100%,
    height: 0.7in,
    fill: white,
    inset: 6pt,
    align(center + horizon)[
      #text(fill: black, size: 14pt)[#bottom-text]
    ],
  )

  stack(dir: ttb, bluerect, redrect, spacing: 0pt)
}

#let connected-boxes(text1: "", text2: "") = {
  let box-style = (
    width: auto,
    fill: rgb("#002D72"),
    inset: (y: 10pt, x: 20pt),
  )

  let left = box(
    ..box-style,
    align(center + horizon)[
      #text(fill: white, weight: "bold", font: "Bitter", text1)
    ],
  )

  let right = box(
    ..box-style,
    align(center + horizon)[
      #text(fill: white, weight: "bold", font: "Bitter", text2)
    ],
  )

  let connector = align(center + horizon)[
    #line(
      length: 61pt,
      stroke: (paint: rgb("#68ACE5"), thickness: 3pt),
    )
  ]

  // Now we return three elements: left box, connector, right box
  (left, connector, right)
}

#let chart-title(body) = {
  v(7pt)
  align(center)[#text(
    body,
    fill: rgb("#002D72"),
    font: "Gentona",
    weight: "medium",
  )]
}

#let to-string(it) = {
  if type(it) == str {
    it
  } else if type(it) != content {
    str(it)
  } else if it.has("text") {
    it.text
  } else if it.has("children") {
    it.children.map(to-string).join()
  } else if it.has("body") {
    to-string(it.body)
  } else if it == [ ] {
    " "
  }
}

#let article(
  title: none,
  date: none,
  state: none,
  doc,
) = {
  let state = if state == none { "texas" } else { to-string(state) }
  let state_flag = "assets/flags/" + str.replace(lower(state), " ", "_") + ".svg"
  let formatted_title = title + " in " + state
  let formatted_title = upper(formatted_title)

  set text(lang: "en", region: "US", font: ("Gentona", "Roboto", "Arial"), size: 11pt, weight: "light")

  set page(
    paper: "us-letter",
    margin: (x: 0.8in, bottom: 1in, top: 0.5in),
    footer: {
      rect(
        width: 100%,
        height: 0.75in,
        outset: (x: 15%),
        fill: rgb("#68ACE5"),
        pad(top: 16pt, block(width: 100%, fill: rgb("#68ACE5"), [
          #grid(
            columns: (3fr, auto, 1fr),
            align(left)[#text(formatted_title, fill: white, weight: 600, font: "Bitter")],
            align(center)[],
            align(right)[#text(upper(date), fill: white, weight: 600, font: "Bitter")],
          )
        ])),
      )
    },
  )

  show heading: it => {
    let sizes = (
      "1": 14pt, // Heading level 1
      "2": 14pt, // Heading level 2
      "3": 13pt, // Heading level 3
      "4": 11pt, // Heading level 4
    )
    let level = str(it.level)
    let size = if level in sizes { sizes.at(level) } else { 10pt }
    let heading_color = if level == "1" { rgb("#002D72") } else { black }

    set text(size: size, fill: heading_color, font: "Bitter")

    if level == "1" { upper(it) } else { it }
  }

  // header cover page (title, logo, flag, etc)
  stack(
    place(dx: 0in, dy: 1.5in, align(block(width: 5.5in, [
      #text(fill: rgb("#002D72"), weight: "bold", size: 20pt, font: "Bitter", formatted_title)]))),
    place(dx: 5.5in, dy: 1.35in, align(block([
      #image(state_flag, width: 20%)]))),
    place(dx: 0.2in, dy: 0.3in, align(block(width: 5in, [
      #image("assets/logo.png", width: 70%)
    ]))),
    place(dx: 0in, dy: 1.2in, align(block([
      #blueline()
    ]))),
  )

  v(2.1in) // hardcoded margin before main content of the doc
  blueline()

  doc
}
