program
	var N as int ; 
 	var SQRT as int ; 
	
procedure FACTORIZE (  ) ;

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
end ;

procedure FIBONACCI ( ) ;

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
end ;


begin 
  N := readInt ; 
  SQRT := 0 ; 

  while SQRT * SQRT <= N do 
    SQRT := SQRT + 1 ; 
  end ; 

 %FACTORIZE ( ) ;

  SQRT := SQRT - 1 ; 

  writeInt SQRT ; 

end 
