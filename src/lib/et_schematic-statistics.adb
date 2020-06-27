------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              SCHEMATIC                                   --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

package body et_schematic.statistics is

	procedure statistics_set (
		cat			: in type_statistics_category;
		increment	: in boolean := true;
		number 		: in count_type := 0) is 
	begin
		if increment then
			case cat is
				when COMPONENTS_TOTAL		=> statistics.components_total		:= statistics.components_total + 1;
				when COMPONENTS_VIRTUAL		=> statistics.components_virtual	:= statistics.components_virtual + 1;
				when COMPONENTS_REAL		=> statistics.components_real		:= statistics.components_real + 1;
				when COMPONENTS_MOUNTED		=> statistics.components_mounted	:= statistics.components_mounted + 1;
				
				when NETS_TOTAL				=> statistics.nets_total			:= statistics.nets_total + 1;
				when JUNCTIONS				=> statistics.junctions				:= statistics.junctions + 1;
				when PORTS_TOTAL			=> statistics.ports_total			:= statistics.ports_total + 1;
				
				when CAPACITORS				=> statistics.capacitors			:= statistics.capacitors + 1;
				when CONNECTORS				=> statistics.connectors			:= statistics.connectors + 1;
				when DIODES					=> statistics.diodes				:= statistics.diodes + 1;
				when INDUCTORS				=> statistics.inductors				:= statistics.inductors + 1;
				when INTEGRATED_CIRCUITS	=> statistics.integrated_circuits	:= statistics.integrated_circuits + 1;
				when JUMPERS				=> statistics.jumpers				:= statistics.jumpers + 1;
				when LEDS					=> statistics.leds					:= statistics.leds + 1;
-- 				when NETCHANGERS			=> statistics.netchangers			:= statistics.netchangers + 1;
				when RELAYS					=> statistics.relays				:= statistics.relays + 1;
				when RESISTORS				=> statistics.resistors				:= statistics.resistors + 1;
				when TESTPOINTS				=> statistics.testpoints			:= statistics.testpoints + 1;				
				when TRANSISTORS			=> statistics.transistors			:= statistics.transistors + 1;
			end case;
		else
			case cat is
				when COMPONENTS_TOTAL	=> statistics.components_total := number;
				when COMPONENTS_VIRTUAL	=> statistics.components_virtual := number;
				
				when others => null; -- CS
			end case;
		end if;
	end statistics_set;
		
	function statistics_query (cat : in type_statistics_category) return count_type is
	-- Returns the number objects as specified by given category.
	begin
		case cat is
			when COMPONENTS_TOTAL		=> return statistics.components_total;
			when COMPONENTS_VIRTUAL		=> return statistics.components_virtual;
			when COMPONENTS_REAL		=> return statistics.components_real;
			when COMPONENTS_MOUNTED		=> return statistics.components_mounted;
			
			when NETS_TOTAL				=> return statistics.nets_total;
			when JUNCTIONS				=> return statistics.junctions;
			when PORTS_TOTAL			=> return statistics.ports_total;
			
			when CAPACITORS				=> return statistics.capacitors;
			when CONNECTORS				=> return statistics.connectors;
			when DIODES					=> return statistics.diodes;
			when INDUCTORS				=> return statistics.inductors;
			when INTEGRATED_CIRCUITS	=> return statistics.integrated_circuits;
			when JUMPERS				=> return statistics.jumpers;
			when LEDS					=> return statistics.leds;
-- 			when NETCHANGERS			=> return statistics.netchangers;
			when RELAYS					=> return statistics.relays;
			when RESISTORS				=> return statistics.resistors;
			when TESTPOINTS				=> return statistics.testpoints;				
			when TRANSISTORS			=> return statistics.transistors;
		end case;

	end statistics_query;

	function statistics_query (cat : in type_statistics_category) return string is
	-- Returns the number objects as specified by given category.
	begin
		return count_type'image (statistics_query (cat));
	end statistics_query;

	
end et_schematic.statistics;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
