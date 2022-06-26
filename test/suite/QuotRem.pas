program arithmetic;
  var x, y, q, r : integer;
  procedure QuotRem(x, y: integer; var q, r: integer);
    begin q := 0; r := x;
      { q * y + r = x and r >= y }
      while r >= y do
        begin r := r - y; q := q + 1
        end
      end;
    begin
      read(x); read(y);
      QuotRem(x, y, q, r);
      write(q); write(r); writeln
    end.
