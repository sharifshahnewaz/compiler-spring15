program
   var SMALLER as int ;
   var BIGGER as int ;
   var TEMP as int ;
begin
   BIGGER := readInt ;
   SMALLER := readInt ;

   if SMALLER > BIGGER then
      TEMP := SMALLER ;
      SMALLER := BIGGER ;
      BIGGER := TEMP ;
   end ;

   while SMALLER > 0 do
      BIGGER := BIGGER - SMALLER ;

      if SMALLER > BIGGER then
         TEMP := SMALLER ;
         SMALLER := BIGGER ;
         BIGGER := TEMP ;
      end ;
   end ;
   writeInt BIGGER ;
end
