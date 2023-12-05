variable tmp0
variable tmp1
variable tmp2
variable total
variable cursor
variable startofline
variable endofline
variable line
includefile1 day1part2.txt
includefile2 day1part2.txt

file1 startofline writeaddr
file1 endofline writeaddr

define increment
  dup readaddr one plus swp writeaddr
end

define decrement
  dup readaddr one minus swp writeaddr
end

define end_to_newline
  mark
    endofline readaddr readbyte isnewline
    three bitand skip
      endofline increment goto
  drop
end

define mult
  tmp0 writeaddr
  tmp1 writeaddr
  zero tmp2 writeaddr
  mark
    tmp0 readaddr iszero
    ten bitand skip
      tmp2 readaddr tmp1 readaddr plus tmp2 writeaddr
      tmp0 decrement goto
  drop
  tmp2 readaddr
end

define first_digit
  startofline readaddr cursor writeaddr 
  mark
    cursor readaddr readbyte dup isnumber 
    four bitand skip
      drop cursor increment goto
    char0 minus ten mult total readaddr plus total writeaddr
  drop
end

define last_digit
  endofline readaddr cursor writeaddr 
  mark
    cursor readaddr readbyte dup isnumber 
    four bitand skip
      drop cursor decrement goto
    char0 minus total readaddr plus total writeaddr
  drop
end

define read250
  twohundredfifty line writeaddr
  mark
    debugbreak
    end_to_newline
    first_digit last_digit
    endofline increment
    endofline readaddr startofline writeaddr
    line readaddr iszero three bitand skip
      line decrement goto
  drop
end

read250
read250

restart