------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         NETCHANGERS / SCHEMATIC                          --
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
-- This package is about properties and primitive operations
-- of netchangers in the schematic domain.
--
--
--   history of changes:
--
-- To Do: 
--
--


-- with et_logging;						use et_logging;
with et_schematic_geometry;				use et_schematic_geometry;
with et_sheets;							use et_sheets;

with et_netchanger_symbol_schematic;	use et_netchanger_symbol_schematic;


package et_netchangers.schematic is

	use et_schematic_coordinates;
	use pac_geometry_2;

	
		

		
-- PORT NAMES:

	type type_netchanger_port_name is (MASTER, SLAVE);
	-- CS: instead of master/slave notation use A/B ?
	-- CS: use prefix like PORT_A, PORT_B
	
	
	function to_port_name (
		name : in string) 
		return type_netchanger_port_name;
		

	function to_string (
		name : in type_netchanger_port_name)
		return string;	


	
	function opposide_port (
		port : in type_netchanger_port_name) 
		return type_netchanger_port_name;
		




	type type_swap_ports is (
		NO_SWAP,
		SWAP);						
	



	
	function get_position_schematic (
		netchanger : in type_netchanger)
		return type_object_position;
		
-- CS
-- 	function get_place_schematic (
-- 		netchanger	: in type_netchanger)
-- 		return type_vector_model;
-- 	
-- 	
-- 	function get_rotation_schematic (
-- 		netchanger	: in type_netchanger)
-- 		return type_rotation;

	procedure set_rotation_schematic (
		netchanger	: in out type_netchanger;
		rotation	: in et_schematic_geometry.type_rotation_model);




-- set place in schematic
-- set sheet in schematic

		
-- move netchanger in schematic
-- rotate netchanger in schematic
		

	

		
	function get_position_schematic (
		netchanger_cursor : in pac_netchangers.cursor)
		return type_object_position;


	function get_sheet (
		netchanger_cursor : in pac_netchangers.cursor)
		return type_sheet;

		
		
	
	type type_netchanger_ports is record
		master	: type_vector_model := position_master_port_default;
		slave	: type_vector_model := position_slave_port_default;
	end record;
	-- CS: instead of master/slave notation use A/B ?



	
	-- Returns the absolute port positions of 
	-- the given netchanger:
	function netchanger_ports (
		netchanger_cursor	: in pac_netchangers.cursor)
		return type_netchanger_ports;
	-- CS rename to get_netchanger_ports or just get_ports ?
	
	
end et_netchangers.schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
