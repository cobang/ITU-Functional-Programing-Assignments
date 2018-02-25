from math import floor


def day_of_week(y, m, d):
    # https://www.geeksforgeeks.org/zellers-congruence-find-day-date/
    # According to acticle on Geek For Geeks, we should subtract 1, when we add 12 to month
    if m <= 2:
        m += 12
        y -= 1

    j = y // 100
    k = y % 100
    t1 = floor(13 * (m + 1) / 5)
    t2 = floor(k / 4)
    t3 = floor(j / 4)
    z = (d + t1 + k + t2 + t3 + 5 * j) % 7
    return z


def sundays1(start, end):
    n = 0
    for y in range(start, end + 1):
        for m in range(1, 13):
            if day_of_week(y, m, 1) == 1:
                n += 1
    return n


def leap(y):
    return (y % 4 == 0) and (y % 100 != 0) or (y % 400 == 0)


def days_in_month(m, y):
    if m == 2:
        days = 29 if leap(y) else 28
    elif (m == 4) or (m == 6) or (m == 9) or (m == 11):
        days = 30
    else:
        days = 31
    return days


def sundays2(start, end):
    n = 0
    # If we set 2 to weekday as default value, result will correct only when start year starts with tuesday.
    # To fix this, I prefer to calculate this value.
    weekday = day_of_week(start, 1, 1) - 1

    for y in range(start, end + 1):
        for m in range(1, 13):
            # I changed order of if statement and other statements because in older version we are calculating
            # the upcoming months start day.
            # For example: sundays(2000, 2005) will also count Jan 2006 (Sunday).
            if weekday % 7 == 0:
                n += 1

            days = days_in_month(m, y)
            weekday += days % 7

    return n


print(sundays1(1901, 2000))
print(sundays2(1901, 2000))
