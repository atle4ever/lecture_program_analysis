port:=196;
len:=43;
pos:=1390;
index:=101;
fd:=15;
xp:=2282;
count:=0;
i:=&xp;
while (port < pos)
    (len:=len+port;
     pos:=pos+*i;
     index:=index+port;
     xp:=*i+3734;
     fd:=fd+port;
     if (sock+1<pos)
         (index:=index+1)
         (index:=index+(-1))
    )
