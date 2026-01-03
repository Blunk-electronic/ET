------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              SUBMODULES                                  --
--                                                                          --
--                               B o d y                                    --
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

with et_coordinates_formatting;		use et_coordinates_formatting;



package body et_submodules is

	function to_submodule_size (size : in type_submodule_size) return string is begin
		return " size (x/y)" &
			to_string (size.x) &
			axis_separator &
			to_string (size.y);
	end;




	
	function at_edge (
		point	: in type_vector_model; -- P
		size	: in type_submodule_size) -- sx, sy
		return boolean is
		-- O--------O
		-- |        |
		-- |        s
		-- |        y
		-- |        |
		-- O---sx---O

		result : boolean := false;
	begin
		-- If P is at the left or right edge:
		if get_x (point) = zero or get_x (point) = size.x then

			-- If P is within y extension of box:
			if get_y (point) >= zero and get_y (point) <= size.y then
				result := true;
			end if;
		
		else
			-- If P is at the lower or upper edge:
			if get_y (point) = zero or get_y (point) = size.y  then

				-- If P is within x extension of box:
				if get_x (point) >= zero and get_x (point) <= size.x then
					result := true;

					if get_y (point) = zero then
						log (WARNING, "Net connected with port at lower edge of submodule at" & 
							to_string (point) & " may overlap with texts !");
					end if;
				end if;
			end if;
		end if;

		return result;
	end at_edge;


	
	
	function to_submodule_path (path : in string) return pac_submodule_path.bounded_string is begin
		return pac_submodule_path.to_bounded_string (path);
	end;



	
	function to_string (path : in pac_submodule_path.bounded_string) return string is begin
		return pac_submodule_path.to_string (path);
	end;




	
	function to_module_name (path : in pac_submodule_path.bounded_string) 
		return pac_module_name.bounded_string 
	is
		use pac_module_name;
		name : pac_module_name.bounded_string;
	begin
		name := to_module_name (remove_extension (to_string (path)));
		return name;
	end to_module_name;



	
	function to_direction_abbrevation (direction : in type_netchanger_port_name) return string is begin
		case direction is 
			when MASTER => return port_direction_abbrevation_master;
			when SLAVE => return port_direction_abbrevation_slave;
		end case;
	end to_direction_abbrevation;



	
	
	procedure move_ports (
		ports	: in out pac_submodule_ports.map; -- the portlist
		offset	: in type_object_position) -- the offset (only x/y matters)
	is

		procedure move (
			name	: in pac_net_name.bounded_string;
			port	: in out type_submodule_port) 
		is begin
			move_by (port.position, offset.place);
		end;

		procedure query_port (cursor : in pac_submodule_ports.cursor) is begin
			pac_submodule_ports.update_element (
				container	=> ports,
				position	=> cursor,
				process		=> move'access);
		end;
			
	begin -- move_ports
		pac_submodule_ports.iterate (ports, query_port'access);
	end move_ports;


	

	
	function to_string (view : in type_submodule_view_mode) return string is begin
		return to_lower (type_submodule_view_mode'image (view));
	end;



	
	function to_view_mode (mode : in string) return type_submodule_view_mode is begin
		return type_submodule_view_mode'value (mode);
	end;



	function get_count (
		submodules : in pac_submodules.map)
		return natural
	is begin
		return natural (submodules.length);
	end;
	

	
end et_submodules;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
