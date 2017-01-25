library ieee ;
use ieee.std_logic_1164.all;
use work.all;
architecture bhv1 of driver is
begin
component and-gate is
port ( X : out std_logic ;
Y : in std_logic ;
F1 : out std_logic );
end component;

component OR_GATE is
port ( x : in std_logic ;
y : in std_logic ;
F2 : out std_logic );
end component;

process(input1 input2)
begin
F2 <= (input1 and input2) ;

end process;
end bhv1;
begin 

Gate1: AND_GATE port map ( a => input1, b => input2, F1 => wire);
Gate2: OR_GATE port map ( x => wire, y => input3, F2 => output);
end struct;

