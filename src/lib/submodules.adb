------------------------------------------------------------------------------
--                                                                          --
--                          SYSTEM ET SUBMODULES                            --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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

--   The two letters "CS" indicate a "construction side" where things are not
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
-- with ada.text_io;				use ada.text_io;
-- with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded;       use ada.strings.bounded;
-- with ada.containers;            use ada.containers;
-- with ada.containers.vectors;
-- with ada.containers.doubly_linked_lists;
-- with ada.containers.indefinite_doubly_linked_lists;
-- with ada.containers.ordered_maps;
-- with ada.containers.indefinite_ordered_maps;
-- with ada.containers.ordered_sets;

with et_general;
with et_coordinates;
with et_libraries;
with et_pcb;
with et_pcb_coordinates;

package body submodules is

	function to_submodule_size (size : in type_submodule_size) return string is 
		use et_coordinates;
	begin
		return " size (x/y)" &
			et_coordinates.to_string (size.x) &
			axis_separator &
			et_coordinates.to_string (size.y);
	end;

	function at_edge (
	-- Returns true if the given point sits at the edge of a submodule box.
		point	: in et_coordinates.type_point; -- P
		size	: in submodules.type_submodule_size) -- sx, sy
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
		if distance (X, point) = zero_distance or distance (X, point) = size.x then

			-- If P is within y extension of box:
			if distance (Y, point) >= zero_distance and distance (Y, point) <= size.y then
				result := true;
			end if;
		
		else
			-- If P is at the lower or upper edge:
			if distance (Y, point) = zero_distance or distance (Y, point) = size.y  then

				-- If P is within x extension of box:
				if distance (X, point) >= zero_distance and distance (X, point) <= size.x then
					result := true;
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
		
	procedure move_ports (
	-- Moves the given ports by the given offset.
		ports	: in out type_submodule_ports.map; -- the portlist
		offset	: in et_coordinates.type_coordinates) -- the offset (only x/y matters)
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
		return latin_1.space & to_lower (type_submodule_view_mode'image (view));
	end;

	function to_view_mode (mode : in string) return type_submodule_view_mode is begin
		return type_submodule_view_mode'value (mode);
	end;

	function to_netchanger_id (id : in string) return type_netchanger_id is begin
		return type_netchanger_id'value (id);
	end;

	function to_string (id : in type_netchanger_id) return string is begin
		return type_netchanger_id'image (id);
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
		return latin_1.space & to_lower (type_netchanger_port_name'image (name));
	end;

	function netchanger_ports (
	-- Returns the absolute x/y positions of the given netchanger.
		netchanger_cursor	: in type_netchangers.cursor)
		return type_netchanger_ports is
		use type_netchangers;
		use et_coordinates;
		ports : type_netchanger_ports;
	begin
		-- rotate the ports according to rotation in schematic
		rotate (ports.master, element (netchanger_cursor).rotation);
		rotate (ports.slave,  element (netchanger_cursor).rotation);

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
