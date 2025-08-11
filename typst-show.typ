#show: doc => article(
  $if(title)$
    title: [$title$],
  $endif$
  $if(date)$
    date: [$date$],
  $endif$
    doc,
)