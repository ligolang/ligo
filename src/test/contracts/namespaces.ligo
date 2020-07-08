
type cards is record
  cards : cards
end

const cards : cards = record [cards = cards]

const cards : cards = cards with record [cards = cards]

const cards : cards = cards.cards
