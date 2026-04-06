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
	-- CS: use prefix ?
	
	
	function to_port_name (
		name : in string) 
		return type_netchanger_port_name;
		

	function to_string (
		name : in type_netchanger_port_name)
		return string;	



	-- Frequently the short form
	-- of a netchanger port is required:
	port_short_master : constant string := "M";
	port_short_slave  : constant string := "S";

	

	-- Returns the short name of the given port name:
	function to_short_name (
		direction : in type_netchanger_port_name) 
		return string;

	

	
	function get_opposide_port (
		port : in type_netchanger_port_name) 
		return type_netchanger_port_name;


	
		

	
	function get_direction (
		netchanger : in type_netchanger)
		return type_netchanger_direction;
		
		
	procedure set_direction (
		netchanger	: in out type_netchanger;
		direction	: in type_netchanger_direction);

	
	procedure toggle_direction (
		netchanger	: in out type_netchanger);

		

	

	-- Converts a netchanger position type to
	-- the position type (containing x/y/rotation):
	function to_position (
		position : in type_netchanger_position_schematic)
		return type_position;

		
	procedure set_place (
		position	: in out type_netchanger_position_schematic;
		place		: in type_vector_model);
		

	function get_place (
		position	: in type_netchanger_position_schematic)
		return type_vector_model;

		
	procedure set_sheet (
		position	: in out type_netchanger_position_schematic;
		sheet		: in type_sheet);

		
	function get_sheet (
		position : in type_netchanger_position_schematic)
		return type_sheet;


	-- Moves the netchanger by the given number of sheets:
	procedure move_netchanger (
		netchanger	: in out type_netchanger;
		offset		: in type_sheet_relative);
	

	function get_rotation (
		position : in type_netchanger_position_schematic)
		return type_rotation_0_90;


	
		
	function get_position (
		netchanger : in type_netchanger)
		return type_netchanger_position_schematic;


	procedure set_position (
		netchanger 	: in out type_netchanger;
		position	: in type_netchanger_position_schematic);
	
		
	procedure set_place (
		netchanger	: in out type_netchanger;
		place		: in type_vector_model);

		

	function get_rotation (
		netchanger	: in type_netchanger)
		return type_rotation_0_90;
		
		
	procedure set_rotation (
		netchanger	: in out type_netchanger;
		rotation	: in type_rotation_0_90);


	procedure toggle_rotation (
		netchanger	: in out type_netchanger);
		
		

	function get_sheet (
		netchanger	: in type_netchanger)
		return type_sheet;

		
	procedure set_sheet (
		netchanger	: in out type_netchanger;
		sheet		: in type_sheet);


	function to_netchanger_position (
		position : in type_object_position)
		return type_netchanger_position_schematic;

	
	function to_netchanger_position (
		sheet		: in et_sheets.type_sheet;
		place		: in et_schematic_geometry.pac_geometry_2.type_vector_model;
		rotation	: in et_schematic_geometry.pac_geometry_2.type_rotation_0_90)
		return type_netchanger_position_schematic;

	
		
	function to_object_position (
		position : in type_netchanger_position_schematic)
		return type_object_position;


	function to_string (
		position	: in type_netchanger_position_schematic)
		return string;

	

		

	

		
	function get_position_schematic (
		netchanger_cursor : in pac_netchangers.cursor)
		return type_netchanger_position_schematic;

		
	function get_rotation (
		netchanger_cursor : in pac_netchangers.cursor)
		return type_rotation_0_90;


	function get_direction (
		netchanger_cursor : in pac_netchangers.cursor)
		return type_netchanger_direction;
	

	function get_sheet (
		netchanger_cursor : in pac_netchangers.cursor)
		return type_sheet;

		


-- PORTS:
	
	
	type type_netchanger_ports is record
		master	: type_vector_model := position_master_port_default;
		slave	: type_vector_model := position_slave_port_default;
	end record;
	-- CS: instead of master/slave notation use A/B ?


	procedure swap_ports (
		ports : in out type_netchanger_ports);
	 

	
	-- Returns the absolute port positions of 
	-- the given netchanger.
	-- The position, direction and rotation
	-- the the netchanger in the schematic is taken into account:
	function get_netchanger_ports (
		netchanger : in type_netchanger)
		return type_netchanger_ports;

		
	function get_netchanger_ports (
		netchanger_cursor : in pac_netchangers.cursor)
		return type_netchanger_ports;

	
	
	
	
-- CATCH ZONE:
	
	-- Returns true if the given netchanger is in
	-- the given catch zone:
	function in_catch_zone (
		netchanger	: in type_netchanger;
		zone		: in type_catch_zone;
		sheet		: in type_sheet)
		return boolean;
	
	
	
	
-- STATUS:


	function is_selected (
		netchanger : in type_netchanger)
		return boolean;


	function is_proposed (
		netchanger : in type_netchanger)
		return boolean;


	function is_moving (
		netchanger : in type_netchanger)
		return boolean;

		
	procedure set_proposed (
		netchanger : in out type_netchanger);
		

	-- Sets the selected flag in both schematic
	-- and board symbol:
	procedure set_selected (
		netchanger : in out type_netchanger);

	
	procedure modify_status (
		netchanger	: in out type_netchanger;
		operation	: in type_status_operation);
		

	-- Resets the status flags in both schematic
	-- and board symbol:
	procedure reset_status (
		netchanger	: in out type_netchanger);

	
end et_netchangers.schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
