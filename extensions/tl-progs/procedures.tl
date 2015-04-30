program
	var X as int ;
	var Y as int ;
	
procedure DUMMY ( A : int ; B : int ) ;

%procedure DUMMY ( ) ;
	var C as int ;
begin
	DUMMY ( 7 , B ) ;
	writeInt A - B ;
end ;

begin
    X := readInt ;
	Y := 5 - X ;
	DUMMY ( X , Y ) ;
	writeInt X - Y ;
end
