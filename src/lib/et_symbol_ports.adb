------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                            SYMBOL PORTS                                  --
--                                                                          --
--                              B o d y                                     --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
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
with ada.text_io;				use ada.text_io;
with ada.characters;			use ada.characters;
-- with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;


package body et_symbol_ports is


	function get_count (
		ports : in pac_symbol_ports.map)
		return natural
	is begin
		return natural (ports.length);
	end;



	
	function get_position (
		port	: in pac_symbol_ports.cursor)
		return type_vector_model
	is begin
		return element (port).position;
	end get_position;




	procedure delete_port (
		ports		: in out pac_symbol_ports.map;
		position	: in type_vector_model;
		deleted		: out boolean;
		port_name	: out pac_port_name.bounded_string)
	is
		c : pac_symbol_ports.cursor := ports.first;

		-- Tests whether the given port position is
		-- equal the given position. On match the
		-- flag "deleted" is set so that the iteration stops:
		procedure query_port (
			port_name	: in pac_port_name.bounded_string;
			port 		: in type_port) 
		is begin
			if port.position = position then
				deleted := true;
				delete_port.port_name := port_name;
			end if;
		end;

		
	begin
		-- Initially we assume that no port has been found:
		deleted := false;
		
		-- Iterate though the given ports until a port
		-- has been found that sits at the given position:
		while has_element (c) loop
			query_element (c, query_port'access);

			-- If a matching port has been found,
			-- then the iteration is aborted:
			if deleted then
				exit;
			end if;
			
			next (c);
		end loop;

		-- If a port has been found, then remove
		-- it from the given port list:
		if deleted then
			ports.delete (c);
		end if;
	end delete_port;

	

	

	procedure move_ports (
		ports	: in out pac_symbol_ports.map; -- the portlist
		offset	: in type_object_position) -- the offset (only x/y matters)
	is
		use pac_symbol_ports;

		procedure move (
			name	: in pac_port_name.bounded_string;
			port	: in out type_port) 
		is begin
			move_by (port.position, offset.place);
		end;

		procedure query_port (cursor : in pac_symbol_ports.cursor) is begin
			update_element (
				container	=> ports,
				position	=> cursor,
				process		=> move'access);
		end;
			
	begin -- move_ports
		iterate (ports, query_port'access);
	end move_ports;


	
	
	procedure rotate_ports (
		ports	: in out pac_symbol_ports.map; -- the portlist
		angle	: in type_rotation_model)  -- 90
	is

		procedure rotate (
			name	: in pac_port_name.bounded_string;
			port	: in out type_port) 
		is begin
			rotate_by (port.position, angle);
		end;

		
		procedure query_port (cursor : in pac_symbol_ports.cursor) is begin
			update_element (
				container	=> ports,
				position	=> cursor,
				process		=> rotate'access);
		end;
			
	begin
		-- Do nothing if the given angle is zero:
		if angle /= zero_rotation then
			iterate (ports, query_port'access);
		end if;
	end rotate_ports;


	
end et_symbol_ports;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
