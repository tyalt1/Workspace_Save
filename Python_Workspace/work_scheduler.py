#!/usr/bin/env python

from datetime import date, time, timedelta

class text_writer(object):
    '''File Wrapper for writing a simple textfile.

    _file = The file opened.
    '''
    
    def __init__(self, name):
        self._file = open(name + '.txt', 'a')

    def __del__(self):
        self._file.close()
        del self._file

    def write(self, write_string):
        self._file.write( str(write_string) )

class Shift(object):
    '''Holds information about shift.

    _shift_date = A date object holding the day that shift is on.
    _working = Bool if you are working that day or not.
    _shift_start_time = A time object holding when your shift starts.
    _shift_end_time = A time object holding when your shift ends.
    '''
    
    def __init__(self, current_day):
        self._shift_date = current_day

    def set_time(self):
        ans = input("Are you working "+self._shift_date.strftime("%m/%d/%y, %A")+"? y/n? ")

        if ans[0].lower() == 'y':
            self._working = True
            
            shift_start = input("What time does your shift start? ")
            colon = shift_start.find(":") #colon will be -1 if there is no colon. Assume they just entered hour.
            if colon == -1:
                self._start_time = time(int(shift_start), 0)
            else:
                self._start_time = time(int(shift_start[:colon]), int(shift_start[colon+1:]))

            shift_end = input("What time does your shift finish? ")
            colon = shift_end.find(":") #colon will be -1 if there is no colon. Assume they just entered hour.
            if colon == -1:
                self._end_time = time(int(shift_end), 0)
            else:
                self._end_time = time(int(shift_end[:colon]), int(shift_end[colon+1:]))
        else:
            self._working = False
            
    def __str__(self):
        if self._working:
            return self._shift_date.strftime("%m/%d/%y, %A:\t") + "Your shift is from {:%I:%M %p} to {:%I:%M %p}.".format(self._start_time, self._end_time)
        else:
            return self._shift_date.strftime("%m/%d/%y, %A:\t") + "Free."

    def get_hour(self):
        if not self._working:
            return 0
        else:
            return float(self._end_time.hour-self._start_time.hour) + float(self._end_time.minute-self._start_time.minute)/60

class WorkWeek(object):
    '''Holds 7 shifts for a week.

    _week = A list of the 7 shifts this week.
    _payrate = Hourly payrate. Used for computing pay.
    _start_date = A date object that holds the date of the first shift.

    _strval = Holds string of all the shifts formated.
    _total_hours = Hold total number of hours.
    '''
    
    def __init__(self):
        self._payrate = float( input("How much do you get paid an hour? $") )

        self._set_start_date()
        self._set_times()

        self._strval = ""
        self._total_hours = 0.0

    def _set_start_date(self):
        year_month_day = input("What day does your work week start? YYYY-MM-DD ")
        year = int( year_month_day[:year_month_day.find("-")] )

        month_day = year_month_day[year_month_day.find("-")+1:]
        month = int( month_day[:month_day.find("-")] )
        day = int( month_day[month_day.find("-")+1:])

        self._start_date = date(year, month, day)

    def _set_times(self):
        self._week = []
        
        for i in range(7):
            self._week.append( Shift(self._start_date + timedelta(i)) )
            self._week[i].set_time()

    def __str__(self):
        if self._strval == "":
            for x in self._week:
                self._strval += str(x) + "\n"

        return self._strval

    def get_hours(self):
        if self._total_hours <= 0:
            self._total_hours = 0
            for x in self._week:
                self._total_hours += x.get_hour()

        return self._total_hours

    '''
    Includes Taxes
        State	            4.80%
        Federal	            4.60%
        Social Security	    6.20%
        Medicare            1.45%

        Total  17.05% or 0.1705
    '''
    def get_pay(self):
        pay = self.get_hours() * self._payrate
        return pay - pay * 0.1705
    
w = WorkWeek()
print("\n")
print(w)

hours_str = "You are working " + str(w.get_hours()) + " hours this week."
print(hours_str)

pay_str = "You are getting paid $" + str("%.2f" % w.get_pay()) + " this week."
print(pay_str)

f = text_writer("work_schedule")
f.write( str(w) )
f.write( "\n" )
f.write( hours_str + "\n" + pay_str + "\n" )
f.write("==================================================\n")
