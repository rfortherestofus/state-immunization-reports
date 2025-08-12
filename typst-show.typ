#show: doc => article(
  $if(title)$
    title: [$title$],
  $endif$
  $if(date)$
    date: [$date$],
  $endif$
  $if(params.state)$
    state: [$params.state$],
  $endif$
    doc,
)