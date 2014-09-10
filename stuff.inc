procedure EnglishDates;
begin
  {$IFDEF MSWINDOWS} {$IF CompilerVersion >= 23}
  {$IF CompilerVersion >= 25} {$LEGACYIFEND ON} {$IFEND}
  FormatSettings.LongMonthNames[1] := 'January';
  FormatSettings.LongMonthNames[2] := 'February';
  FormatSettings.LongMonthNames[3] := 'March';
  FormatSettings.LongMonthNames[4] := 'April';
  FormatSettings.LongMonthNames[5] := 'May';
  FormatSettings.LongMonthNames[6] := 'June';
  FormatSettings.LongMonthNames[7] := 'July';
  FormatSettings.LongMonthNames[8] := 'August';
  FormatSettings.LongMonthNames[9] := 'September';
  FormatSettings.LongMonthNames[10] := 'October';
  FormatSettings.LongMonthNames[11] := 'November';
  FormatSettings.LongMonthNames[12] := 'December';
  FormatSettings.LongDayNames[1] := 'Sunday';
  FormatSettings.LongDayNames[2] := 'Monday';
  FormatSettings.LongDayNames[3] := 'Tuesday';
  FormatSettings.LongDayNames[4] := 'Wednesday';
  FormatSettings.LongDayNames[5] := 'Thursday';
  FormatSettings.LongDayNames[6] := 'Friday';
  FormatSettings.LongDayNames[7] := 'Saturday';
  {$ELSE}
  LongMonthNames[1] := 'January';
  LongMonthNames[2] := 'February';
  LongMonthNames[3] := 'March';
  LongMonthNames[4] := 'April';
  LongMonthNames[5] := 'May';
  LongMonthNames[6] := 'June';
  LongMonthNames[7] := 'July';
  LongMonthNames[8] := 'August';
  LongMonthNames[9] := 'September';
  LongMonthNames[10] := 'October';
  LongMonthNames[11] := 'November';
  LongMonthNames[12] := 'December';
  LongDayNames[1] := 'Sunday';
  LongDayNames[2] := 'Monday';
  LongDayNames[3] := 'Tuesday';
  LongDayNames[4] := 'Wednesday';
  LongDayNames[5] := 'Thursday';
  LongDayNames[6] := 'Friday';
  LongDayNames[7] := 'Saturday';
  {$IFEND} {$ENDIF}
end;