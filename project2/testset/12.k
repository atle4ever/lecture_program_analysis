a := read;
s := 41250;
p := &s;
num := 0;
while (a < 100)
(
	num := num + (*p);
	num := num * 16713;
	a := a + 1
);
num := num + 415
