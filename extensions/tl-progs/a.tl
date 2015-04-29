program
	var X as int ;
	var Y as int ;
	
%procedure DUMMY ( A : int ; B : int )

procedure DUMMY ( A : int ; B : int ) ;
	var C as int ;
begin
	DUMMY ( A , B ) ;
	writeInt A - B ;
end ;

begin
    X := readInt ;
	Y := 5 - X ;
	DUMMY ( X , Y ) ;
	writeInt X - Y ;
end
