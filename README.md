# Vhdl Lisp 


<p> The idea behind this program is to make it easier for lisp programmers to work with VHDL by creating a similar and easier syntax .



#Dependencies

<p> SBCL 
<p> Any text editor, but, in case you need one https://github.com/FemtoEmacs/Femto-Emacs


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
library ,ieee ;
use ieee.std_logic_1164.all;
entity driver is
    port( x:  in std_logic;
f:  out std_logic); 
   end driver;
architecture behv1 of driver is
begin
process(X)
begin
if (x = 1) then
        (F <= 1);
else 
        (F <= 0);
end if
end process;
end behv1

```
#How to use
  For now you can only use it directly from sbcl 
```lisp 
&  sbcl --load vhdlisp.lisp
  (main "samples/driver.vlsp") 

```

#TODO 

I have a lot to do, but for now i'll keep my mind in some features missing and some bugs that may occur .

<p> * Fix bit numbers '1' and '0' (for now it is equal 1 and 0). 
<p> * Add the function to write directly to a vhdl file (sorry for this one) .
<p> * Add a executable version . 
<p> * Get arguments file-name and output-file from command line (for those who will compile).
<p> * More example codes and a better documentation . 
<p> * Add many more missing  features .

#Bugs and suggestions 

<h5> Any bug report or suggestions can be send here or in my personal email lu.guerra7508@gmail.com </h5> 
