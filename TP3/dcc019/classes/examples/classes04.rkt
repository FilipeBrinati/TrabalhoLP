#lang dcc019/classes

class c1 extends object
  field x
  field y
  method initialize() 1
  method setx1(v) set x = v
  method sety1(v) set y = v
  method getx1() x
  method gety1() y

class c2 extends c1
  field y
  method sety2(v) set y = v
  method getx2() x
  method gety2() y

let o2 = new c2()
in begin
  send o2 setx1(101);
  send o2 sety1(102);
  send o2 sety2(999);
  -(send o2 gety1(), send o2 gety2())
end

% Result: -897
