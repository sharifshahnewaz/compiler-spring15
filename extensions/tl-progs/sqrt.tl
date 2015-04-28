program 
  var N as int ; 
  var SQRT as int ; 
begin 
  N := readInt ; 
  SQRT := 0 ; 

  while SQRT * SQRT <= N do 
    SQRT := SQRT + 1 ; 
  end ; 

  SQRT := SQRT - 1 ; 

  writeInt SQRT ; 

end 
