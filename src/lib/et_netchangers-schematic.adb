------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         NETCHANGERS / SCHEMATIC                          --
--                                                                          --
--                               B o d y                                    --
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
--   history of changes:
--
--   ToDo: 


with ada.text_io;					use ada.text_io;

with ada.characters;				use ada.characters;
with ada.characters.handling;		use ada.characters.handling;
with ada.strings;					use ada.strings;
with ada.strings.fixed;				use ada.strings.fixed;

with et_coordinates_formatting;		use et_coordinates_formatting;



package body et_netchangers.schematic is

	
	
	

	
-- PORT NAMES:

	function to_port_name (
		name : in string) 
		return type_netchanger_port_name 
	is begin
		return type_netchanger_port_name'value (name);
	end;

	
	
	function to_string (
		name : in type_netchanger_port_name) 
		return string 
	is begin
		return trim (to_lower (type_netchanger_port_name'image (name)), left);
	end;

	
		

	

	
	function get_opposide_port (
		port : in type_netchanger_port_name) 
		return type_netchanger_port_name 
	is begin
		case port is
			when MASTER => return SLAVE;
			when SLAVE  => return MASTER;
		end case;
	end;	
	
	
	
	
	
	
	procedure set_direction (
		netchanger	: in out type_netchanger;
		direction	: in type_netchanger_direction)
	is begin
		netchanger.direction := direction;
	end;
	
	
	
	
	function to_position (
		position : in type_netchanger_position_schematic)
		return type_position
	is 
		result : type_position;
	begin
		result.place := position.place;
		result.rotation := position.rotation;
	
		return result;
	end;

	

	
	procedure set_place (
		position	: in out type_netchanger_position_schematic;
		place		: in type_vector_model)
	is begin
		position.place := place;
	end;


	
	function get_place (
		position	: in type_netchanger_position_schematic)
		return type_vector_model
	is begin
		return position.place;
	end;
		

	
	
	procedure set_sheet (
		position	: in out type_netchanger_position_schematic;
		sheet		: in type_sheet)
	is begin
		position.sheet := sheet;
	end;

	
	
	
	function get_sheet (
		position : in type_netchanger_position_schematic)
		return type_sheet
	is begin
		return position.sheet;
	end;

	

	function get_rotation (
		position : in type_netchanger_position_schematic)
		return type_rotation_0_90
	is begin
		return position.rotation;
	end;
	
	
	
	function to_netchanger_position (
		position : in type_object_position)
		return type_netchanger_position_schematic
	is
		result : type_netchanger_position_schematic;
	begin
		result.place := get_place (position);
		result.rotation := get_rotation (position);
		result.sheet := get_sheet (position);
		return result;
	end;

	
	
	function to_object_position (
		position : in type_netchanger_position_schematic)
		return type_object_position
	is
		result : type_object_position;
	begin
		set_place (result, position.place);
		set_rotation (result, position.rotation);
		set_sheet (result, position.sheet);
		
		return result;
	end;
	
	
	

	function get_position (
		netchanger : in type_netchanger)
		return type_netchanger_position_schematic
	is begin
		return netchanger.position_sch;
	end;

	
	
	procedure set_position (
		netchanger 	: in out type_netchanger;
		position	: in type_netchanger_position_schematic)
	is begin
		netchanger.position_sch := position;
	end;


	
	procedure set_place (
		netchanger	: in out type_netchanger;
		place		: in type_vector_model)
	is begin
		netchanger.position_sch.place := place;
	end;

	
	

	function get_position_schematic (
		netchanger_cursor : in pac_netchangers.cursor)
		return type_netchanger_position_schematic
	is 
		n : type_netchanger renames element (netchanger_cursor);
	begin
		return get_position (n);
	end;
	




	procedure set_rotation (
		netchanger	: in out type_netchanger;
		rotation	: in type_rotation_0_90)
	is begin
		netchanger.position_sch.rotation := rotation;
	end;


	
	procedure set_sheet (
		netchanger	: in out type_netchanger;
		sheet		: in type_sheet)
	is begin
		netchanger.position_sch.sheet := sheet;
	end;

	
	
	function get_sheet (
		netchanger_cursor : in pac_netchangers.cursor)
		return type_sheet
	is
		n : type_netchanger renames element (netchanger_cursor);
	begin
		return n.position_sch.sheet;
	end;

	
	
	
-- PORTS:

	function get_netchanger_ports (
		netchanger_cursor : in pac_netchangers.cursor)		
		return type_netchanger_ports 
	is
		use pac_netchangers;
		n : type_netchanger renames element (netchanger_cursor);
		
		ports : type_netchanger_ports;
	begin
		-- rotate the ports according to rotation in schematic
		rotate_by (ports.master, n.position_sch.rotation);
		rotate_by (ports.slave,  n.position_sch.rotation);

		-- move the ports according to position in schematic
		move_by (ports.master, n.position_sch.place);
		move_by (ports.slave,  n.position_sch.place);
				
		return ports;
	end get_netchanger_ports;


	
end et_netchangers.schematic;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
