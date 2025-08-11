#import "@preview/titleize:0.1.1": titlecase

#let blueline() = {
  line(length: 100%, stroke: 2pt + rgb("#68ACE5"))
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

  set text(lang: "en", region: "US", font: ("Bitter", "Gentona", "Roboto", "Arial"), size: 12pt)

  set page(
    paper: "us-letter",
    margin: (x: 0.5in, bottom: 1in, top: 0.5in),
    footer: {
      rect(
        width: 100%,
        height: 0.6in,
        outset: (x: 10%),
        fill: rgb("#68ACE5"),
        pad(top: 12pt, block(width: 100%, fill: rgb("#68ACE5"), [
          #grid(
            columns: (3fr, auto, 1fr),
            align(left)[#text(formatted_title, fill: white, weight: 600)],
            align(center)[],
            align(right)[#text(upper(date), fill: white, weight: 600)],
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
    let level_str = str(it.level)
    let size = if level_str in sizes { sizes.at(level_str) } else { 10pt }
    set text(size: size)

    it
  }

  // header cover page
  stack(
    place(dx: 0in, dy: 1.5in, align(block(width: 5.5in, [
      #text(fill: rgb("#002D72"), weight: "bold", size: 25pt, formatted_title)]))),
    place(dx: 5.7in, dy: 1.35in, align(block([
      #image(state_flag, width: 20%)]))),
    place(dx: 0.2in, dy: 0.3in, align(block(width: 5in, [
      #image("assets/logo.png", width: 70%)
    ]))),
    place(dx: 0in, dy: 1.2in, align(block([
      #blueline()
    ]))),
  )

  v(2.3in)
  blueline()

  doc
}
