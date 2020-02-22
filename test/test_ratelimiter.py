import pytest
from unittest.mock import patch
from datetime import datetime, timedelta
from Briareus.Actions.Actors.RateLimiter import RateLimiter



def test_rate_nolimits():
    rl = RateLimiter()
    for each in range(30):
        r = rl.allowed('foo', ['one', 'two', 'three'])
        assert r == ['one', 'two', 'three']


def test_rate_hourly_limit():
    rl = RateLimiter(hourly=5)
    for each in range(5):
        r = rl.allowed('foo', ['one', 'two', 'three'])
        assert r == ['one', 'two', 'three']

    r = rl.allowed('foo', ['one', 'two', 'three'])
    assert r == []
    r = rl.allowed('foo', ['one', 'two', 'three', 'four'])
    assert r == ['four']

    this_now = datetime.now()
    half_hour_later = datetime.now() + timedelta(minutes=30)
    with patch('Briareus.Actions.Actors.RateLimiter.datetime') as dt:
        dt.now.return_value = half_hour_later
        r = rl.allowed('foo', ['one', 'two', 'three', 'four'])
        assert r == ['four']

    hour_later = datetime.now() + timedelta(minutes=61)
    with patch('Briareus.Actions.Actors.RateLimiter.datetime') as dt:
        dt.now.return_value = hour_later
        r = rl.allowed('foo', ['one', 'two', 'three', 'four'])
        assert r == ['one', 'two', 'three', 'four']

    r = rl.allowed('foo', ['one', 'two', 'three', 'four'])
    assert r == ['four']



def test_rate_hourly_limit_one():
    rl = RateLimiter(hourly=5)
    for each in range(5):
        r = rl.allowed('foo', ['one', 'five'])
        assert r == ['one', 'five']
    r = rl.allowed('foo', ['one', 'two', 'three'])
    assert r == ['two', 'three']


def test_rate_daily_limit():
    rl = RateLimiter(daily=9)
    for each in range(9):
        r = rl.allowed('foo', ['one', 'two', 'three'])
        assert r == ['one', 'two', 'three']
    r = rl.allowed('foo', ['one', 'two', 'three'])
    assert r == []


def test_rate_limits_and_status():
    this_now = datetime(year=1984, day=5, month=5, hour=8, minute=30)
    rl = RateLimiter(hourly=3, daily=13, unlimited=['nine', 'U'])

    with patch('Briareus.Actions.Actors.RateLimiter.datetime') as dt:
        dt.now.return_value = this_now
        r = rl.allowed('foo', ['one', 'two', 'three'])  # 'one': 1, 'two': 1, 'three': 1
        assert r == ['one', 'two', 'three']
        r = rl.allowed('bar', ['three', 'four'])  # 'one': 1, 'two': 1, 'three': 2, 'four': 1
        assert r == ['three', 'four']
        r = rl.allowed('dog', ['one', 'three'])  # 'one': 2, 'two': 1, 'three': 3, 'four': 1
        assert r == ['one', 'three']
        r = rl.allowed('woof', ['one', 'three'])  # 'one': 3, 'two': 1, 'three': 4, 'four': 1
        assert r == ['one']
        r = rl.allowed('woof', ['one', 'three'])  # 'one': 4, 'two': 1, 'three': 5, 'four': 1
        assert r == []
        r = rl.allowed('woof', ['one', 'two', 'three']) # 'one': 5, 'two': 2, 'three': 6, 'four': 1
        assert r == ['two']

        s = rl.status()
        assert len(s['recent']['allowed']['one']) == 3
        assert len(s['recent']['allowed']['two']) == 2
        assert len(s['recent']['allowed']['three']) == 3
        assert len(s['recent']['allowed']['four']) == 1

        assert len(s['recent']['suppressed']['one']) == 2
        assert len(s['recent']['suppressed']['two']) == 0
        assert len(s['recent']['suppressed']['three']) == 3
        assert len(s['recent']['suppressed']['four']) == 0

        assert len(s['this_month']['allowed']) == 0
        assert len(s['this_month']['suppressed']) == 0

    half_hour_later = this_now + timedelta(minutes=30)
    with patch('Briareus.Actions.Actors.RateLimiter.datetime') as dt:
        dt.now.return_value = half_hour_later
        r = rl.allowed('foo', ['one', 'two', 'three', 'four'])  # 'one': 6, 'two': 3, 'three': 7, 'four': 2
        assert r == ['two', 'four']

        s = rl.status()
        assert len(s['recent']['allowed']['one']) == 3
        assert len(s['recent']['allowed']['two']) == 3
        assert len(s['recent']['allowed']['three']) == 3
        assert len(s['recent']['allowed']['four']) == 2

        assert len(s['recent']['suppressed']['one']) == 3
        assert len(s['recent']['suppressed']['two']) == 0
        assert len(s['recent']['suppressed']['three']) == 4
        assert len(s['recent']['suppressed']['four']) == 0

        assert len(s['this_month']['allowed']) == 0
        assert len(s['this_month']['suppressed']) == 0

    two_hours_later = this_now + timedelta(hours=2)
    with patch('Briareus.Actions.Actors.RateLimiter.datetime') as dt:
        dt.now.return_value = two_hours_later
        r = rl.allowed('foo', ['one', 'two', 'three', 'four'])  # 'one': 1, 'two': 1, 'three': 1, 'four': 1
        assert r == ['one', 'two', 'three', 'four']
        r = rl.allowed('foo', ['two', 'four', 'six'])  # 'one': 1, 'two': 2, 'three': 1, 'four': 2, 'six': 1
        assert r == ['two', 'four', 'six']

        for each in range(25):
            rl.allowed('25', ['nine', 'U', 'five'])

        S = rl.status()
        assert len(s['recent']['allowed']['one']) == 4
        assert len(s['recent']['allowed']['two']) == 5
        assert len(s['recent']['allowed']['three']) == 4
        assert len(s['recent']['allowed']['four']) == 4
        assert len(s['recent']['allowed']['five']) == 3
        assert len(s['recent']['allowed']['six']) == 1
        assert len(s['recent']['allowed']['nine']) == 25
        assert len(s['recent']['allowed']['U']) == 25

        assert len(s['recent']['suppressed']['one']) == 3
        assert len(s['recent']['suppressed']['two']) == 0
        assert len(s['recent']['suppressed']['three']) == 4
        assert len(s['recent']['suppressed']['four']) == 0
        assert len(s['recent']['suppressed']['five']) == 22
        assert len(s['recent']['suppressed']['six']) == 0
        assert len(s['recent']['suppressed']['nine']) == 0
        assert len(s['recent']['suppressed']['U']) == 0

        assert len(s['this_month']['allowed']) == 0
        assert len(s['this_month']['suppressed']) == 0

    five_days_later = this_now + timedelta(days=5, hours=1)
    with patch('Briareus.Actions.Actors.RateLimiter.datetime') as dt:
        dt.now.return_value = five_days_later
        r = rl.allowed('foo', ['one', 'two', 'three', 'four'])  # 'one': 1, 'two': 1, 'three': 1, 'four': 1
        assert r == ['one', 'two', 'three', 'four']
        r = rl.allowed('foo', ['one', 'two', 'three'])  # 'one': 2, 'two': 2, 'three': 2, 'four': 1
        assert r == ['one', 'two', 'three']
        r = rl.allowed('foo', ['two', 'four', 'six'])  # 'one': 2, 'two': 3, 'three': 2, 'four': 2, 'six': 1
        assert r == ['two', 'four', 'six']


        s = rl.status()
        assert len(s['recent']['allowed']['one']) == 2
        assert len(s['recent']['allowed']['two']) == 3
        assert len(s['recent']['allowed']['three']) == 2
        assert len(s['recent']['allowed']['four']) == 2
        assert len(s['recent']['allowed']['five']) == 0
        assert len(s['recent']['allowed']['six']) == 1
        assert len(s['recent']['allowed']['nine']) == 0
        assert len(s['recent']['allowed']['U']) == 0

        assert len(s['recent']['suppressed']['one']) == 0
        assert len(s['recent']['suppressed']['two']) == 0
        assert len(s['recent']['suppressed']['three']) == 0
        assert len(s['recent']['suppressed']['four']) == 0
        assert len(s['recent']['suppressed']['five']) == 0
        assert len(s['recent']['suppressed']['six']) == 0
        assert len(s['recent']['suppressed']['nine']) == 0
        assert len(s['recent']['suppressed']['U']) == 0

        assert s['this_month']['allowed']['one'] == { 5:3, 4:1 }
        assert s['this_month']['allowed']['two'] == { 5:3, 4:2 }
        assert s['this_month']['allowed']['three'] == { 5:3, 4:1 }
        assert s['this_month']['allowed']['four'] == { 5:2, 4:2 }
        assert s['this_month']['allowed']['five'] == { 4:3 }
        assert s['this_month']['allowed']['six'] == { 4:1 }
        assert s['this_month']['allowed']['nine'] == { 4:25 }
        assert s['this_month']['allowed']['U'] == { 4:25 }

        assert s['this_month']['suppressed']['one'] == { 5:3 }
        assert s['this_month']['suppressed']['two'] == { }
        assert s['this_month']['suppressed']['three'] == { 5:4 }
        assert s['this_month']['suppressed']['four'] == { }
        assert s['this_month']['suppressed']['five'] == { 4:22 }
        assert s['this_month']['suppressed']['six'] == { }
        assert s['this_month']['suppressed']['nine'] == { }
        assert s['this_month']['suppressed']['U'] == { }

    weeks_later = this_now + timedelta(days=29, hours=4)
    with patch('Briareus.Actions.Actors.RateLimiter.datetime') as dt:
        dt.now.return_value = weeks_later
        r = rl.allowed('foo', ['one', 'two', 'three', 'four'])  # 'one': 1, 'two': 1, 'three': 1, 'four': 1
        assert r == ['one', 'two', 'three', 'four']
        r = rl.allowed('foo', ['two', 'four', 'six'])  # 'one': 1, 'two': 2, 'three': 1, 'four': 2, 'six': 1
        assert r == ['two', 'four', 'six']

        s = rl.status()
        assert len(s['recent']['allowed']['one']) == 1
        assert len(s['recent']['allowed']['two']) == 2
        assert len(s['recent']['allowed']['three']) == 1
        assert len(s['recent']['allowed']['four']) == 2
        assert len(s['recent']['allowed']['five']) == 0
        assert len(s['recent']['allowed']['six']) == 1
        assert len(s['recent']['allowed']['nine']) == 0
        assert len(s['recent']['allowed']['U']) == 0
        assert len(s['recent']['allowed']) == 8

        assert len(s['recent']['suppressed']['one']) == 0
        assert len(s['recent']['suppressed']['two']) == 0
        assert len(s['recent']['suppressed']['three']) == 0
        assert len(s['recent']['suppressed']['four']) == 0
        assert len(s['recent']['suppressed']['five']) == 0
        assert len(s['recent']['suppressed']['six']) == 0
        assert len(s['recent']['suppressed']['five']) == 0
        assert len(s['recent']['suppressed']['U']) == 0
        assert len(s['recent']['suppressed']) == 8

        assert s['this_month']['allowed']['one'] == { 29:4, 24:2 }
        assert s['this_month']['allowed']['two'] == { 29:5, 24:3 }
        assert s['this_month']['allowed']['three'] == { 29:4, 24:2 }
        assert s['this_month']['allowed']['four'] == { 29:4, 24:2 }
        assert s['this_month']['allowed']['five'] == { 29:3 }
        assert s['this_month']['allowed']['six'] == { 29:1, 24:1 }
        assert s['this_month']['allowed']['nine'] == { 29:25 }
        assert s['this_month']['allowed']['U'] == { 29:25 }
        assert len(s['this_month']['allowed']) == 8

        assert s['this_month']['suppressed']['one'] == { 29:3 }
        assert s['this_month']['suppressed']['two'] == { }
        assert s['this_month']['suppressed']['three'] == { 29:4 }
        assert s['this_month']['suppressed']['four'] == { }
        assert s['this_month']['suppressed']['five'] == { 29:22 }
        assert s['this_month']['suppressed']['six'] == { }
        assert s['this_month']['suppressed']['nine'] == { }
        assert s['this_month']['suppressed']['U'] == { }
        assert len(s['this_month']['suppressed']) == 8
