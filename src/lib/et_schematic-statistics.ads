------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              SCHEMATIC                                   --
--                                                                          --
--                               S p e c                                    --
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   ToDo: 

package et_schematic.statistics is

	-- Whenever we deal with statistic file this type should be used:
	statistic_file_name_length : constant positive := 100; -- CS: should suffice for now
	package type_statistic_file_name is new generic_bounded_length (statistic_file_name_length); 

	extension_statistics : constant string := "stat";

	type type_statistics is private;

	type type_statistics_category is (
		COMPONENTS_MOUNTED,
		COMPONENTS_REAL,
		COMPONENTS_TOTAL,
		COMPONENTS_VIRTUAL,
		
		NETS_TOTAL,
		-- CS: nets_global, nets_hierarchic
		JUNCTIONS,
		PORTS_TOTAL,

		CONNECTORS,
		CAPACITORS,
		DIODES,
		INDUCTORS,
		INTEGRATED_CIRCUITS,
		JUMPERS,
		LEDS,
		--NETCHANGERS,
		RELAYS,
		RESISTORS,
		TESTPOINTS,
		TRANSISTORS
		-- CS: no_connection_flags
		);
	
	procedure statistics_set (
		cat			: in type_statistics_category;
		increment	: in boolean := true;
		number 		: in count_type := 0);

	function statistics_query (cat : in type_statistics_category) return count_type;
	-- Returns the number objects as specified by given category.
	
	function statistics_query (cat : in type_statistics_category) return string;
	-- Returns the number objects as specified by given category.
	
	
	private
	
		type type_statistics is record
			components_mounted	: count_type := 0;
			components_real		: count_type := 0;
			components_total	: count_type := 0;
			components_virtual	: count_type := 0;
			
			nets_total			: count_type := 0;
			junctions			: count_type := 0;
			ports_total			: count_type := 0;

			capacitors			: count_type := 0;
			connectors			: count_type := 0;
			diodes				: count_type := 0;
			inductors			: count_type := 0;			
			integrated_circuits	: count_type := 0;
			jumpers				: count_type := 0;
			leds				: count_type := 0;
			netchangers			: count_type := 0;
			relays				: count_type := 0;
			resistors			: count_type := 0;
			testpoints			: count_type := 0;
			transistors			: count_type := 0;
-- CS		ports_virtual		: count_type := 0;
-- CS		ports_real			: count_type := 0;
		end record;	

		statistics : type_statistics;
		

		
end et_schematic.statistics;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
