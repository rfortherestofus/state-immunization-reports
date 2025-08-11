#import "@preview/titleize:0.1.1": titlecase

#let blueline() = {
  line(length: 100%, stroke: 2pt + rgb("#68ACE5"))
}

#let source(body) = {
  align(right)[#text(body, style: "italic", font: "Bitter")]
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
  let state_flag = "assets/flags/" + lower(to-string(state)) + ".svg"
  let formatted_title = title + " in " + state
  let formatted_title = upper(formatted_title)

  set text(lang: "en", region: "US", font: ("Gentona", "Roboto", "Arial"), size: 9pt)

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
      "1": 17pt, // Heading level 1
      "2": 15pt, // Heading level 2
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
      #text(fill: rgb("#002D72"), weight: "bold", size: 25pt, font: "Bitter", formatted_title)]))),
    place(dx: 5.5in, dy: 1.35in, align(block([
      #image(state_flag, width: 20%)]))),
    place(dx: 0.2in, dy: 0.3in, align(block(width: 5in, [
      #image("assets/logo.png", width: 70%)
    ]))),
    place(dx: 0in, dy: 1.2in, align(block([
      #blueline()
    ]))),
  )

  v(2.3in) // hardcoded margin before main content of the doc
  blueline()

  doc
}
