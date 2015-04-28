program 
   var N as int ; 
   var I as int ; 
begin 
   N := readInt ; 

   I := 2 ; 
   while I <= N do 
      while N mod I = 0 do 
         N := N div I ; 
         writeInt I ; 
      end ; 
      I := I + 1 ; 
   end ; 
end 
