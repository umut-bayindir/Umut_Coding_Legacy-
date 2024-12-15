identification division.
program-id. roman-numeral.
*> program to convert roman numerals to their decimal equivalent

environment division.


input-output section.
*> I used this code to tell Cobol that I am inputing a file
file-control.
   select input-file
      assign to dynamic input_file_name
      organization is line sequential.
  
      

data division.


file section.
   fd input-file.
   01 input-data.
      02 in-r      pic x(15).
    
      



working-storage section.
01 file-info.
       05 file-size        pic x(8) comp-x.
       05 file-date.
          10 f-day         pic x comp-x.
          10 f-month       pic x comp-x.
          10 f-year        pic xx comp-x.
       05 file-time.
          10 f-hours       pic x comp-x.
          10 f-minutes     pic x comp-x.
          10 f-seconds     pic x comp-x.
          10 f-hundredths  pic x comp-x.
77 input_file_name  pic x(30).
77 eof-switch   pic 9    value 1.
77 switch       pic 9.
77 n 	        pic s9(2)  comp.
77 sum1         pic s9(8)  comp.
77 i            pic s9(2)  comp.
77 prev         pic s9(4)  comp.
77 d            pic s9(4)  comp.
*> got rid of the filler here for the padding of the input file
01 array-area.
   02 r         pic x(1)   occurs 16 times.
01 input-data-record.
   02 in-r      pic x(15).
   
01 output-title-line.
   02 filler    pic x(28)  value
                    "  roman number equivalents  ". 
01 output-underline-1.
   02 filler    pic x(30)  value
                    "------------------------------". 
01 output-column-headings.
   02 filler    pic x(14)  value
                    "  roman number". 
   02 filler    pic x(16)  value
                    "     dec. equiv.". 
01 output-underline-2.
   02 filler    pic x(30)  value
                    " ---------------------------- ". 
01 output-table-record.
   02 filler    pic x      value space.
   02 out-r     pic x(15).
   02 filler    pic x(3)   value spaces.
   02 v         pic z(9).
01 output-error-mess.
   02 filler    pic x      value space.
   02 out-er-r  pic x(15).
   02 filler    pic x(24)  value
                    "   illegal roman numeral". 

  
procedure division.

*> the legend here to make it easier for user to understand
perform get_file_name.
display " "
open input input-file.
display "------------------------------"
display " roman number conversion."
display " ---------------------------- "
display " conversion table value roman to decimal"
display " I = 1 "
display " V = 5"
display " X = 10"
display " L = 50"
display " C = 100"
display " D = 500"
display " M = 1000"

read input-file into input-data-record
   at end move zero to eof-switch.
perform proc-body
   until eof-switch is equal to zero.
close input-file.
display "  ---------------------------- "
stop run.
proc-body.
   move in-r in input-data-record to array-area.
   move 1 to n.
   perform search-loop
      until r(n) is equal to space.
   compute n = n - 1.
   perform conv.
   if switch is equal to 1
      move sum1 to v
      move array-area to out-r
      display output-table-record
   end-if.
   read input-file into input-data-record
     at end move zero to eof-switch.

search-loop.
   compute n = n + 1.
  
   
conv.
   move zero to sum1.
   move 1001 to prev.
   move 1 to switch.
   perform conversion-loop
      varying i from 1 by 1
      until i is greater than n or
         switch is equal to 2.

   
*> algorithmn change to give the correct output
conversion-loop.
   if r(i) is equal to "I"
      move 1 to d
      add d to sum1
      if d > prev
         compute sum1 = sum1 - 2 * prev
      end-if
      move d to prev
   else if r(i) is equal to "V"
      move 5 to d
      add d to sum1
      if d > prev
         compute sum1 = sum1 - 2 * prev
      end-if
      move d to prev
   else if r(i) is equal to "X"
      move 10 to d
      add d to sum1
      if d > prev
         compute sum1 = sum1 - 2 * prev
      end-if
      move d to prev
   else if r(i) is equal to "L"
      move 50 to d
      add d to sum1
      if d > prev
         compute sum1 = sum1 - 2 * prev
      end-if
      move d to prev
   else if r(i) is equal to "C"
      move 100 to d
      add d to sum1
      if d > prev
         compute sum1 = sum1 - 2 * prev
      end-if
      move d to prev
   else if r(i) is equal to "D"
      move 500 to d
      add d to sum1
      if d > prev
         compute sum1 = sum1 - 2 * prev
      end-if
      move d to prev
   else if r(i) is equal to "M"
      move 1000 to d
      add d to sum1
      if d > prev
         compute sum1 = sum1 - 2 * prev
      end-if
      move d to prev
   else move 2 to switch
        move array-area to out-er-r
        display output-error-mess 
   end-if.
   *> compute sum1 = sum1 + d.
 
*> cobol way to get user to input a file
get_file_name.
 display "welcome to the roman numeral conversion:"
 display " enter filename to convert "
    accept input_file_name from console.
    call "CBL_CHECK_FILE_EXIST" using input_file_name file-info.
    if return-code not equal zero
        display "error: file " input_file_name(1:20) " does not exist"
         perform get_file_name.

