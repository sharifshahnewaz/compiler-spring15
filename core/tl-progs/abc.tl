program 
  var N as int ; 
  var SQRT as int ; 
begin 
  N := readInt ;
  SQRT := N ; 
  SQRT := N ;
  SQRT := N + 10 * 9 * 12 + 13 ; 

  while SQRT * SQRT <= N * N do 
    SQRT := SQRT + 1 ; 
  end ; 

  if SQRT * SQRT <= N * N then
    writeInt SQRT + N * 8 div 9 ;
  else
    writeInt SQRT + N * 8 div 9 ;
  end ;
  writeInt SQRT + N * 8 div 9 ; 
end 
