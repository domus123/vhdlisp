library ieee ;

use ieee.std_logic_1164.all;

use work.all;

architecture bhv1 of driver is
begin
component and-gate is
port ( X : out std_logic ;
Y : in std_logic );
component OR_GATE is
port ( x : in std_logic ;
y : in std_logic ;
F2 : out std_logic );
process(input1 input2)
begin
F2 <= (input1 and input2) ;

end process;
end bhv1;
