------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             NETCHANGERS                                  --
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
with ada.strings.bounded;      		use ada.strings.bounded;
with ada.strings.maps;				use ada.strings.maps;

with et_coordinates_formatting;		use et_coordinates_formatting;



package body et_netchangers is

	
-- ID:

	function to_netchanger_id (
		id : in string) 
		return type_netchanger_id 
	is begin
		return type_netchanger_id'value (id);
	end;


	
	function to_string (
		id : in type_netchanger_id) 
		return string 
	is begin
		return trim (type_netchanger_id'image (id), left);
	end;





	
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



	
	function opposide_port (
		port : in type_netchanger_port_name) 
		return type_netchanger_port_name 
	is begin
		case port is
			when MASTER => return SLAVE;
			when SLAVE  => return MASTER;
		end case;
	end;	
	
	
	
	
	
	
	
	
-- PORTS:

	function netchanger_ports (
		netchanger_cursor	: in pac_netchangers.cursor)
		return type_netchanger_ports 
	is
		use pac_netchangers;
		ports : type_netchanger_ports;
	begin
		-- rotate the ports according to rotation in schematic
		rotate_by (ports.master, get_rotation (element (netchanger_cursor).position_sch));
		rotate_by (ports.slave,  get_rotation (element (netchanger_cursor).position_sch));

		-- move the ports according to position in schematic
		move_by (ports.master, element (netchanger_cursor).position_sch.place);
		move_by (ports.slave,  element (netchanger_cursor).position_sch.place);
				
		return ports;
	end netchanger_ports;


	
end et_netchangers;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
