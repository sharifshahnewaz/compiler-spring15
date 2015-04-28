program
   var N1 as int ; 
   var N2 as int ; 
   var NEXT as int ; 
   var MAX as int ; 
begin 
   MAX := readInt ; 

   N1 := 0 ; 
   N2 := 1 ; 

   writeInt N1 ; 

   while N2 < MAX do 
      writeInt N2 ; 
      NEXT := N1 + N2 ; 
      N1 := N2 ; 
      N2 := NEXT ; 
   end ; 
end 
