library ieee ;

use ieee.std_logic_1164.all;

entity XOR_GATE is
port( a, b:  in  std_logic ;
F:  out  std_logic ); 
   end XOR_GATE;

architecture bhv of XOR_GATE is
begin
process(a b)
begin
F <= ((a or b) and ((not a) or b)) ;

end process;
end bhv;
