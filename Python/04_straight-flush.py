import sys

from collections import namedtuple


Card = namedtuple('Card', ['suit', 'rank'])


def read_cards(line):
    cards = []
    for s in line.split():
        suit = s[-1]
        rank = convert_rank(s[:-1])
        cards.append(Card(suit, rank))
    return cards


def convert_rank(rank):
    if rank == 'A':
        return 14
    if rank == 'K':
        return 13
    if rank == 'Q':
        return 12
    if rank == 'J':
        return 11
    if rank == 'T':
        return 10
    return int(rank)


def is_straight_flush(cards):
    suits = {card.suit for card in cards}
    if len(suits) != 1:
        return False

    ranks = sorted([card.rank for card in cards])
    differences = [a - b for a, b in zip(ranks[1:], ranks)]
    return (differences == [1, 1, 1, 1] or differences == [1, 1, 1, 9])


def main():
    data = sys.stdin.read().splitlines()
    cards = read_cards(data[0])
    result = is_straight_flush(cards)
    print('YES' if result else 'NO')


if __name__ == '__main__':
    main()
