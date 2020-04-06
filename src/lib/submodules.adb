------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              SUBMODULES                                  --
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

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings;				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.bounded;       use ada.strings.bounded;

with et_general;				use et_general;
with et_coordinates;
with et_pcb;
with et_pcb_coordinates;
with et_string_processing;		use et_string_processing;

package body submodules is

	function to_submodule_size (size : in type_submodule_size) return string is 
		use et_coordinates.geometry;
	begin
		return " size (x/y)" &
			geometry.to_string (size.x) &
			axis_separator &
			geometry.to_string (size.y);
	end;

	function at_edge (
		point	: in et_coordinates.geometry.type_point; -- P
		size	: in submodules.type_submodule_size) -- sx, sy
		return boolean is
		-- O--------O
		-- |        |
		-- |        s
		-- |        y
		-- |        |
		-- O---sx---O

		use geometry;
		result : boolean := false;
	begin
		-- If P is at the left or right edge:
		if x (point) = zero or x (point) = size.x then

			-- If P is within y extension of box:
			if y (point) >= zero and y (point) <= size.y then
				result := true;
			end if;
		
		else
			-- If P is at the lower or upper edge:
			if y (point) = zero or y (point) = size.y  then

				-- If P is within x extension of box:
				if x (point) >= zero and x (point) <= size.x then
					result := true;

					if y (point) = zero then
						log (WARNING, "Net connected with port at lower edge of submodule at" & 
							to_string (point) & " may overlap with texts !");
					end if;
				end if;
			end if;
		end if;

		return result;
	end at_edge;
	
	function to_submodule_path (path : in string) return type_submodule_path.bounded_string is begin
		return type_submodule_path.to_bounded_string (path);
	end;

	function to_string (path : in submodules.type_submodule_path.bounded_string) return string is begin
		return type_submodule_path.to_string (path);
	end;

	function to_module_name (path : in type_submodule_path.bounded_string) 
		return et_general.type_module_name.bounded_string is
	-- Removes the file extension from given path and returns the module name.
		use et_general;
		use type_module_name;
		name : et_general.type_module_name.bounded_string;
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
	-- Moves the given ports by the given offset.
		ports	: in out type_submodule_ports.map; -- the portlist
		offset	: in et_coordinates.type_position) -- the offset (only x/y matters)
		is

		procedure move (
			name	: in et_general.type_net_name.bounded_string;
			port	: in out type_submodule_port) is
		begin
			move (port.position, offset);
		end;

		procedure query_port (cursor : in type_submodule_ports.cursor) is begin
			type_submodule_ports.update_element (
				container	=> ports,
				position	=> cursor,
				process		=> move'access);
		end;
			
	begin -- move_ports
		type_submodule_ports.iterate (ports, query_port'access);
	end move_ports;
	
	function to_string (view : in type_submodule_view_mode) return string is begin
		return to_lower (type_submodule_view_mode'image (view));
	end;

	function to_view_mode (mode : in string) return type_submodule_view_mode is begin
		return type_submodule_view_mode'value (mode);
	end;

	function to_netchanger_id (id : in string) return type_netchanger_id is begin
		return type_netchanger_id'value (id);
	end;

	function to_string (id : in type_netchanger_id) return string is begin
		return trim (type_netchanger_id'image (id), left);
	end;

	function opposide_port (port : in type_netchanger_port_name) return type_netchanger_port_name is begin
		case port is
			when MASTER => return SLAVE;
			when SLAVE  => return MASTER;
		end case;
	end;
	
	function to_port_name (name : in string) return type_netchanger_port_name is begin
		return type_netchanger_port_name'value (name);
	end;

	function to_string (name : in type_netchanger_port_name) return string is begin
		return trim (to_lower (type_netchanger_port_name'image (name)), left);
	end;

	function netchanger_ports (
	-- Returns the absolute x/y positions of the given netchanger.
		netchanger_cursor	: in type_netchangers.cursor)
		return type_netchanger_ports is
		use type_netchangers;
		use et_coordinates.geometry;
		ports : type_netchanger_ports;
	begin
		-- rotate the ports according to rotation in schematic
		rotate_by (ports.master, rot (element (netchanger_cursor).position_sch));
		rotate_by (ports.slave,  rot (element (netchanger_cursor).position_sch));

		-- move the ports according to position in schematic
		move (ports.master, element (netchanger_cursor).position_sch);
		move (ports.slave,  element (netchanger_cursor).position_sch);
				
		return ports;
	end netchanger_ports;
	
end submodules;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
