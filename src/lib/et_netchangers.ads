------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        NETCHANGERS GENERAL                               --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                -- 
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--
-- DESCRIPTION:
--
-- This package is about general properties
-- and primitive operations of netchangers.
--
--
--   history of changes:
--
-- To Do: 
--
--

with ada.containers;					use ada.containers;
with ada.containers.ordered_maps;

with et_schematic_coordinates;
with et_pcb_signal_layers;				use et_pcb_signal_layers;
with et_board_geometry;

-- with et_logging;						use et_logging;



package et_netchangers is


-- PREFIX:

	netchanger_prefix : constant string := "N";
	-- CS: Instead of N a prefix NC could make things clearer.
	-- CS consider user defined prefixes. conventions ?
	
	
	
-- ID:

	netchanger_id_max : constant positive := 10000; 
	-- CS  increase if necessary
	
	type type_netchanger_id is range 1 .. netchanger_id_max;


	
	
	function to_netchanger_id (
		id : in string) 
		return type_netchanger_id;

		
	function to_string (
		id : in type_netchanger_id) 
		return string;		


	-- Returns a full name of a netchanger like N44:
	function get_netchanger_name (
		id : in type_netchanger_id) 
		return string;
		

		
	


		
	
	-- 1. In the schematic drawing, there is only one hardcoded
	--    symbol for a netchanger (see package spec et_netchanger_symbol_schematic).
	--    So there is no link to a netchanger symbol as we are familar
	--    with devices. Here we only need to know the position in the schematic.
	-- 2. Similar the board drawing, we only need the position and the
	--    signal layer where the netchanger is connecting tracks.
	--    In the board drawing a hardcoded symbol is used too
	--    (see package et_netchanger_symbol_board).
	
	type type_netchanger_position_board is record
		place	: et_board_geometry.pac_geometry_2.type_vector_model; -- x,y
		layer	: type_signal_layer := type_signal_layer'first;
	end record;

	
	type type_netchanger is record -- CS make private
		position_sch : et_schematic_coordinates.type_object_position; 
		-- x,y,sheet,rotation
		
		-- CS reverse, swap flag ?
		
		position_brd : type_netchanger_position_board;
	end record;




		

	
	package pac_netchangers is new ordered_maps (
		key_type		=> type_netchanger_id,
		element_type	=> type_netchanger);

	use pac_netchangers;
	

		
	-- Returns a cursor to the given netchanger.
	-- If the netchanger is not among the given netchangers,
	-- then no_element will be returned:
	function get_netchanger (
		netchangers : in pac_netchangers.map;
		index		: in type_netchanger_id)
		return pac_netchangers.cursor;
		
	
	
end et_netchangers;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
