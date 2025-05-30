from rosemary import Item, update


def test_name():
    item = Item('Cheese', days_left=5, quality=5)
    update(item)
    return item.name == 'Cheese'


def test_name_aged_brie():
    item = Item('Aged Brie', days_left=5, quality=5)
    update(item)
    return item.name == 'Aged Brie'


def test_name_diamond():
    item = Item('Diamond', days_left=5, quality=5)
    update(item)
    return item.name == 'Diamond'


def test_name_tickets():
    item = Item('Tickets', days_left=5, quality=5)
    update(item)
    return item.name == 'Tickets'


def test_normal_item_decreases_days_left():
    item = Item('Bread', days_left=3, quality=5)
    update(item)
    return item.days_left == 2


def test_normal_item_quality_decrease():
    item = Item('Bread', days_left=5, quality=6)
    update(item)
    return item.quality == 5


def test_normal_item_quality_0():
    item = Item('Bread', days_left=4, quality=0)
    update(item)
    return item.quality == 0


def test_normal_item_days_left_0_quality():
    item = Item('Cheese', days_left=0, quality=5)
    update(item)
    return item.quality == 3


def test_normal_item_days_left_0_quality_1():
    item = Item('Cheese', days_left=0, quality=1)
    update(item)
    return item.quality == 0


def test_diamond_days_decrease():
    item = Item('Diamond', days_left=10, quality=100)
    update(item)
    return item.days_left == 10


def test_diamond_quality_100():
    item = Item('Diamond', days_left=10, quality=100)
    update(item)
    return item.quality == 100


def test_aged_brie_day_left_decrease():
    item = Item('Aged Brie', days_left=5, quality=5)
    update(item)
    return item.days_left == 4


def test_aged_brie_quality_increase():
    item = Item('Aged Brie', days_left=5, quality=5)
    update(item)
    return item.quality == 6


def test_aged_brie_quality_50():
    item = Item('Aged Brie', days_left=5, quality=50)
    update(item)
    return item.quality == 50


def test_tickets_days_left_0_quality():
    item = Item('Tickets', days_left=0, quality=10)
    update(item)
    return item.quality == 0


def test_tickets_days_left_more_than_10_quality():
    item = Item('Tickets', days_left=11, quality=10)
    update(item)
    return item.quality == 11


def test_tickets_days_left_more_than_10_quality_50():
    item = Item('Tickets', days_left=11, quality=50)
    update(item)
    return item.quality == 50


def test_tickets_days_left_10_quality():
    item = Item('Tickets', days_left=10, quality=10)
    update(item)
    return item.quality == 12


def test_tickets_days_left_6_quality():
    item = Item('Tickets', days_left=6, quality=10)
    update(item)
    return item.quality == 12


def test_tickets_days_left_10_quality_49():
    item = Item('Tickets', days_left=10, quality=49)
    update(item)
    return item.quality == 50


def test_tickets_days_left_5_quality():
    item = Item('Tickets', days_left=5, quality=10)
    update(item)
    return item.quality == 13


def test_tickets_days_left_1_quality():
    item = Item('Tickets', days_left=1, quality=10)
    update(item)
    return item.quality == 13


def test_tickets_days_left_5_quality_48():
    item = Item('Tickets', days_left=5, quality=48)
    update(item)
    return item.quality == 50
