__author__ = 'Tyler'

# This pay rate and tax percentage is based of job at AMC Theaters.
PAY_RATE = 8.70  # Dollars per hour made. (Hardcoded for now)
TAX_PERCENTAGE = 0.1705  # Percentage of Tax taken out of paycheck. (Hardcoded for now)

from tkinter import *
from tkinter.filedialog import askopenfile
from datetime import date, time, timedelta

from os.path import expanduser
HOME_DIR = expanduser('~')


class WorkDay:
    def __init__(self, day_date, start_time, end_time):
        self.day_date = day_date
        self.start_time = start_time
        self.end_time = end_time
        self.is_working = bool(end_time.hour - start_time.hour != 0)

    def __str__(self):
        return_string = "{:<28}".format("{0:%m/%d/%y, %A}".format(self.day_date))
        if self.is_working:
            return_string += "Your shift is from {:%I:%M %p} to {:%I:%M %p}.".format(self.start_time, self.end_time)
        else:
            return_string += "Free."
        return return_string

    def __float__(self):
        if not self.is_working:
            return 0.0
        else:
            return float((self.end_time.hour - self.start_time.hour)+(self.end_time.minute - self.start_time.minute)/60.0)

    def __int__(self):
        if not self.is_working:
            return 0
        else:
            return int((self.end_time.hour - self.start_time.hour)+(self.end_time.minute - self.start_time.minute)/60.0)


class WorkWeek:
    def __init__(self):
        self.week = [None] * 7

    def __str__(self):
        return_string = ""
        for shift in self:
            return_string += str(shift) + '\n'

        return return_string

    def __float__(self):
        return_num = 0
        for shift in self:
            return_num += float(shift)

        return float(return_num)

    def __int__(self):
        return_num = 0
        for shift in self:
            return_num += float(shift)

        return int(return_num)

    def __len__(self):
        return 7

    def __getitem__(self, key):
        return self.week[key]

    def __setitem__(self, key, value):
        self.week[key] = value

    def __delitem__(self, key):
        self.week[key] = None

    def __iter__(self):
        return iter(self.week)


class DialogWindow:
    """
    @brief Window that holds WorkWeek object.
    @class DialogWindow

    dialog The Tk Toplevel object.
    work_week The copy of the WorkWeek object built in the main window.
    write_file The file to write to. Might be None.

    hour_count Holds string with hours of shift.
    pay_count Holds string with pay before taxes.
    paytax_count Holds string with pay after taxes.
    """
    def __init__(self, work_week, master, write_file):
        self.dialog = Toplevel(master)
        self.work_week = work_week
        self.write_file = write_file
        self.dialog.minsize(width=500, height=375)

        for shift in self.work_week:
            Label(self.dialog, text=str(shift)).pack()
        Label(self.dialog, text='').pack()
        
        self.hour_count = 'You are working '+str(float(self.work_week))+' hours this week.'
        Label(self.dialog, text=self.hour_count).pack()
        
        pay = float(self.work_week) * PAY_RATE
        self.pay_count = 'You are making '+'${:.2f}'.format(pay)+' this week before taxes.'
        self.paytax_count = 'You are making '+'${:.2f}'.format(pay - pay * TAX_PERCENTAGE)+' this week after taxes.'
        Label(self.dialog, text=self.pay_count).pack()
        Label(self.dialog, text=self.paytax_count).pack()
        try:
            file_name = self.write_file.name[self.write_file.name.rfind('/')+1:]
        except AttributeError:
            file_name = 'File (Choose File)'

        Button(self.dialog, text='Write to '+file_name, command=self.write_to_file).pack()
        Button(self.dialog, text='Done', command=self.dialog.destroy).pack()

    def __del__(self):
        try:
            self.write_file.close()
        except AttributeError:
            return

    def write_to_file(self):
        try:
            if self.write_file is None:
                self.write_file = askopenfile(mode='a', initialdir=HOME_DIR, filetypes=[('Text File', '*.txt'), ('All Files', '*')])

            file_write = lambda text='': self.write_file.write(str(text)+'\n')
            file_write(self.work_week)
            file_write(self.hour_count)
            file_write(self.pay_count)
            file_write(self.paytax_count)
            file_write('=' * 75)
            
            self.write_file.flush()
        except AttributeError:
            return

    def start_loop(self):
        self.dialog.mainloop()


class MainWindow:
    """
    @brief The main GUI window.
    @class MainWindow

    master Main tkinter object.
    writeFile File to write schedule to.

    menuBar Main menu bar.
    fileMenu File cascade in menuBar.

    dateFrame Frame that holds fields for entering first date.
    firstDay Dictionary that contains month, day, and year entry fields.

    timeFrame Frame that holds fields for entering start and end times.
    dayVars List of StingVars Used to label days.
    hoursStart List of starting hours.
    minutesStart List of starting minutes.
    hoursEnd List of ending hours.
    minutesEnd List of ending minutes.
    """
    def __init__(self):
        # Init Root
        self.master = Tk()

        self.writeFile = None

        # Window Properties
        self.master.title('Schedule')
        self.master.minsize(width=1200, height=200)

        # Menu Bar
        self.menuBar = Menu(self.master)
        self.master.config(menu=self.menuBar)
        # File Sub-Menu
        self.fileMenu = Menu(self.menuBar)
        self.menuBar.add_cascade(label='File', menu=self.fileMenu)
        self.fileMenu.add_cascade(label='Open', command=self.open_write_file)
        self.fileMenu.add_cascade(label='Refresh', command=self.set_to_default)
        self.fileMenu.add_separator()
        self.fileMenu.add_cascade(label='Exit', command=quit)

        # Body
        # Date Frame
        self.dateFrame = Frame(self.master)
        self.dateFrame.pack()
        self.firstDate = dict(month=Entry(self.dateFrame, width=2),
                              day=Entry(self.dateFrame, width=2),
                              year=Entry(self.dateFrame, width=4))
        Label(self.dateFrame, text='First Day of Work Week: ').grid(row=0, column=0)
        self.firstDate['month'].grid(row=0, column=1)
        self.firstDate['month'].insert(0, str(date.today().month))
        Label(self.dateFrame, text='/').grid(row=0, column=2)
        self.firstDate['day'].grid(row=0, column=3)
        self.firstDate['day'].insert(0, str(date.today().day))
        Label(self.dateFrame, text='/').grid(row=0, column=4)
        self.firstDate['year'].grid(row=0, column=5)
        self.firstDate['year'].insert(0, str(date.today().year))
        Button(self.dateFrame, text='Set Date', command=self.set_date).grid(row=0, column=6)

        # Time Frame
        self.timeFrame = Frame(self.master)
        self.timeFrame.pack()

        self.dayVars = [None] * 7
        self.hoursStart = [None] * 7
        self.minutesStart = [None] * 7
        self.hoursEnd = [None] * 7
        self.minutesEnd = [None] * 7
        index = 0  # Used to control column of Entries and spacings
        for i in range(7):
            # Row=0 Labels for each day.
            self.dayVars[i] = StringVar()
            Label(self.timeFrame, textvariable=self.dayVars[i]).grid(row=1, column=index, sticky=W, columnspan=3)
            self.dayVars[i].set('Day '+str(i+1))

            # Row=1 Hour and Minute Entry objects for stating time.
            self.hoursStart[i] = Entry(self.timeFrame, width=2, justify=RIGHT)
            self.hoursStart[i].grid(row=2, column=index)
            self.hoursStart[i].insert(0, '00')

            Label(self.timeFrame, text=':').grid(row=2, column=index+1)

            self.minutesStart[i] = Entry(self.timeFrame, width=2, justify=RIGHT)
            self.minutesStart[i].grid(row=2, column=index+2)
            self.minutesStart[i].insert(0, '00')

            if i != 6:
                Label(self.timeFrame, text='').grid(row=2, column=index+3, padx=10)

            # Row=2 Hour and Minute Entry objects for finishing time.
            self.hoursEnd[i] = Entry(self.timeFrame, width=2, justify=RIGHT)
            self.hoursEnd[i].grid(row=3, column=index)
            self.hoursEnd[i].insert(0, '00')

            Label(self.timeFrame, text=':').grid(row=3, column=index+1)

            self.minutesEnd[i] = Entry(self.timeFrame, width=2, justify=RIGHT)
            self.minutesEnd[i].grid(row=3, column=index+2)
            self.minutesEnd[i].insert(0, '00')

            if i != 6:
                Label(self.timeFrame, text='').grid(row=3, column=index+3, padx=20)

            index += 4

        # Button
        Button(self.master, text='Compute Schedule', command=self.make_schedule).pack()

    def __del__(self):
        try:
            self.writeFile.close()
        except AttributeError:
            return

    def make_schedule(self):
        """
        Spawns new dialog window that holds the newly made schedule.
        """
        # Reset date and get start date.
        try:
            year = int(self.firstDate['year'].get())
            if year < 1900:
                raise ValueError
        except ValueError:
            self.firstDate['year'].delete(0, END)
            self.firstDate['year'].insert(0, str(date.today().year))
            year = date.today().year
        try:
            month = int(self.firstDate['month'].get())
            if month > 12 or month < 1:
                raise ValueError
        except ValueError:
            self.firstDate['month'].delete(0, END)
            self.firstDate['month'].insert(0, str(date.today().month))
            month = date.today().month
        try:
            day = int(self.firstDate['day'].get())
            if day > 31 or day < 1:
                raise ValueError
        except ValueError:
            self.firstDate['day'].delete(0, END)
            self.firstDate['day'].insert(0, str(date.today().day))
            day = date.today().day

        current_day = date(year, month, day)

        # Build Work Week
        work_week = WorkWeek()
        one_day = timedelta(1)  # Day object used to increment current_day
        for i in range(7):
            try:
                start_time = time(int(self.hoursStart[i].get()), int(self.minutesStart[i].get()))
            except ValueError:
                start_time = time(0, 0)
            try:
                end_time = time(int(self.hoursEnd[i].get()), int(self.minutesEnd[i].get()))
            except ValueError:
                end_time = time(0, 0)
            work_week[i] = WorkDay(current_day, start_time, end_time)
            current_day += one_day

        # Build Display Window
        DialogWindow(work_week, self.master, self.writeFile).start_loop()

    def set_date(self):
        """
        Reads the date from the fields and sets the dates of the week.
        Called by the Set Date Button.
        """
        try:
            year = int(self.firstDate['year'].get())
            if year < 1900:
                raise ValueError
        except ValueError:
            self.firstDate['year'].delete(0, END)
            self.firstDate['year'].insert(0, str(date.today().year))
            year = date.today().year

        try:
            month = int(self.firstDate['month'].get())
            if month > 12 or month < 1:
                raise ValueError
        except ValueError:
            self.firstDate['month'].delete(0, END)
            self.firstDate['month'].insert(0, str(date.today().month))
            month = date.today().month

        try:
            day = int(self.firstDate['day'].get())
            if day > 31 or day < 1:
                raise ValueError
        except ValueError:
            self.firstDate['day'].delete(0, END)
            self.firstDate['day'].insert(0, str(date.today().day))
            day = date.today().day

        first_day = date(year, month, day)

        for day in self.dayVars:
            day.set("{:%A %m/%d/%y}".format(first_day))
            first_day += timedelta(1)

    def set_to_default(self):
        """
        Resets start date to today's date and resets all day times to 00:00.
        Called when Refresh cascade is clicked.
        """
        self.firstDate['month'].delete(0, END)
        self.firstDate['month'].insert(0, str(date.today().month))
        self.firstDate['day'].delete(0, END)
        self.firstDate['day'].insert(0, str(date.today().day))
        self.firstDate['year'].delete(0, END)
        self.firstDate['year'].insert(0, str(date.today().year))

        for time_list in [self.hoursStart, self.minutesStart, self.hoursEnd, self.minutesEnd]:
            for entry in time_list:
                entry.delete(0, END)
                entry.insert(0, '00')

    def open_write_file(self):
        self.writeFile = askopenfile(mode='a', initialdir=HOME_DIR, filetypes=[('Text File', '*.txt'), ('All Files', '*')])

    def start_loop(self):
        self.master.mainloop()

window = MainWindow()
window.start_loop()
