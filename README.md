# Vhdl Lisp 


<p> The idea behind this program is to make it easier for lisp programmers to work with VHDL by creating a similar and easier syntax .



#Dependencies

<p> SBCL 
<p> Femto-Emacs https://github.com/FemtoEmacs/Femto-Emacs in case you need a editor with syntax-highlight


#What it can do 

Take by example the following program writted in VHDL 

```vhdl

ibrary ieee;
use ieee.std_logic_1164.all;

entity Driver is
port(	x: in std_logic;
	F: out std_logic
);
end Driver;  
architecture behv1 of Driver is
begin

    process(x)
    begin
        -- compare to truth table
        if (x='1') then
            F <= '1';
        else
            F <= '0';
        end if;
    end process;

end behv1;
```
<h4> And here is the vhdlisp implementation </h4> 
```
(library ieee)

(use ieee.std_logic_1164.all)

(define-entity Driver
         (x is in std_logic)
         (f is out std_logic))

(def-arch behv1 of driver
     (process (x)
          (if (= x 1 )
             (set f 1)
           else
             (set f 0)) ))

```

Simple and beautifull

<h5> Here is the generated code </h5>  
<h5> Is kind of messy but works </h5>

```vhdl
library ieee ;

use ieee.std_logic_1164.all;

entity Driver is
  port( x:  in  std_logic ;
        f:  out  std_logic ); 
end Driver;

architecture behv1 of driver is
begin
  process(x)
  begin
    if (x = 1) then
      f <= 1 ;
    else 
      f <= 0 ;
    end if;
  end process;
end behv1;
```

#How to use

```lisp 
&  sbcl --load vhdlisp.lisp
   (main "samples/driver.vlisp" "driver.vhdl")
```
```
&  sbcl --load vhdlisp.lisp
   (compile-vhdlisp)
   ./vhdlisp "samples/driver.vlisp" "driver.vhdl"
```

#TODO 

I have a lot to do, but for now i'll keep my mind in some features missing and some bugs that may occur .

<p> * Fix bit numbers '1' and '0' (for now it is equal 1 and 0).
<p> * More example codes and a better documentation . 
<p> * Add many more missing  features .

#Bugs and suggestions 

<h5> Any bug report or suggestions can be send here or in my personal email lu.guerra7508@gmail.com </h5> 
