
import datetime
from lunarcalendar import Converter, Solar, Lunar, DateNotExist

# https://pypi.org/project/LunarCalendar/


# solar = Solar(1956, 2, 29)
# print(solar)
# lunar = Converter.Solar2Lunar(solar)
# print(lunar)

# solar = Solar(2020, 2, 11)
# print(solar)
# lunar = Converter.Solar2Lunar(solar)
# print(lunar)

# solar = Converter.Lunar2Solar(lunar)
# print(solar)
# print(solar.to_date(), type(solar.to_date()))

month = 1
day = 1
for year in range(2020, 2081):
    lunar = Lunar(year, mon, day, isleap=False)
    solar = Converter.Lunar2Solar(lunar)
    date = solar.to_date()
    year = str(date.year)
    month = '0' + str(date.month) if date.month < 10 else str(date.month)
    day = '0' + str(date.day) if date.day < 10 else str(date.day)

    print('* 날짜가 부정확 할 수 있으니, 네이버 캘린더로 확인')
    print('  SCHEDULED: <{}-{}-{}>'.format(year, month, day))
    print()
