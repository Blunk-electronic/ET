------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          DEVICE UNITS IN SCHEMATIC                       --
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
-- <http://www.gnu.org/licenses/>.   
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

with ada.text_io;						use ada.text_io;
with ada.characters.handling;			use ada.characters.handling;
with ada.exceptions;
with et_exceptions;						use et_exceptions;


package body et_units is


	procedure rotate_placeholders (
		placeholders	: in out type_default_placeholders;
		rotation		: in type_rotation_model)
	is begin
		-- Rotate the POSITIONS	of the placeholders about
		-- the origin of the symbol:
		rotate_by (placeholders.name.position, rotation);
		rotate_by (placeholders.value.position, rotation);
		rotate_by (placeholders.purpose.position, rotation);

		-- Rotate the placeholders about THEIR OWN ORIGIN.
		-- The resulting angle is the sum of the initial 
		-- rotation (given by the symbol model) and the rotation
		-- of the unit.
		-- After summing up the rotation must be snapped to either
		-- HORIZONTAL or VERTICAL so that the text is readable
		-- from the right or from the front of the drawing.
		placeholders.name.rotation := 
			to_rotation_doc (to_rotation (placeholders.name.rotation) + rotation);

		placeholders.value.rotation := 
			to_rotation_doc (to_rotation (placeholders.value.rotation) + rotation);

		placeholders.purpose.rotation := 
			to_rotation_doc (to_rotation (placeholders.purpose.rotation) + rotation);
	end rotate_placeholders;

	


	function get_default_placeholders (
		symbol_cursor	: in pac_symbols.cursor;
		destination		: in type_object_position)
		return type_default_placeholders
	is
		use pac_symbols;

		sym : type_symbol renames element (symbol_cursor);
		
		r : type_default_placeholders; -- to be returned
	begin
		r.name		:= sym.placeholders.name;
		r.value		:= sym.placeholders.value;
		r.purpose	:= sym.placeholders.purpose;

		-- Rotate the positions of placeholders 
		-- according to rotation given by destination:
		rotate_placeholders (r, get_rotation (destination));
		
		return r;
	end get_default_placeholders;



	

	function get_default_placeholders (
		symbol_cursor	: in pac_units_internal.cursor;
		destination		: in type_object_position)
		return type_default_placeholders
	is
		use pac_units_internal;
		sym : type_unit_internal renames element (symbol_cursor);
		
		use pac_geometry_sch;

		r : type_default_placeholders; -- to be returned
	begin
		r.name		:= sym.symbol.placeholders.name;
		r.value		:= sym.symbol.placeholders.value;
		r.purpose	:= sym.symbol.placeholders.purpose;

		-- rotate the positions of placeholders according to rotation given by caller:
		rotate_placeholders (r, get_rotation (destination));
		
		return r;
	end get_default_placeholders;

	


	function is_real (
		unit : in type_unit)
		return boolean
	is begin
		if unit.appearance = APPEARANCE_PCB then
			return true;
		else
			return false;
		end if;
	end;


	
	
	
	function get_position (
		unit	: in type_unit)
		return type_object_position
	is begin
		return unit.position;
	end get_position;



	procedure set_position (
		unit		: in out type_unit;
		position	: in type_object_position)
	is begin
		unit.position := position;
	end set_position;
	



	function get_mirror_status (
		unit	: in type_unit)
		return type_mirror
	is begin
		return unit.mirror;
	end;

	

	procedure set_mirror_status (
		unit	: in out type_unit;
		mirror	: in type_mirror)
	is begin
		unit.mirror := mirror;
	end;

	
	

	function get_rotation (
		unit	: in type_unit)
		return type_rotation_model
	is begin
		return get_rotation (unit.position);
	end get_rotation;

	

	procedure set_rotation (
		unit		: in out type_unit;
		rotation	: in type_rotation_model)
	is begin
		set_rotation (unit.position, rotation);
	end set_rotation;


	

	procedure rotate_by (
		unit		: in out type_unit;
		rotation	: in type_rotation_model)
	is 
		-- Get the rotation before:
		r_actual : type_rotation_model := get_rotation (unit);
	begin
		-- Add the given rotation to the actual rotation:
		add (r_actual, rotation);		

		-- Assign the new rotation:
		set_rotation (unit, r_actual);
	end;

	
	
	
	function get_sheet (
		unit	: in type_unit)
		return type_sheet
	is begin
		return get_sheet (unit.position);
	end get_sheet;


	
	procedure set_sheet (
		unit	: in out type_unit;
		sheet	: in type_sheet)
	is begin
		set_sheet (unit.position, sheet);
	end set_sheet;
	

	
	
	function in_catch_zone (
		unit	: in type_unit;
		zone	: in type_catch_zone;
		sheet	: in type_sheet)
		return boolean
	is 
		result : boolean := false;

		unit_position : type_object_position := get_position (unit);
	begin
		-- The unit must be on the given sheet and
		-- in the given catch zone:
		if 	get_sheet (unit_position) = sheet 
		and	in_catch_zone (zone, get_place (unit_position)) then
			result := true;
		else
			result := false;
		end if;
		
		return result;
	end in_catch_zone;

	

	

	procedure unit_not_found (
		name : in pac_unit_name.bounded_string) 
	is begin
		raise semantic_error_1 with
			"ERROR: Unit " & to_string (name) & " not found !";
	end unit_not_found;

	
	

	procedure set_selected (
		unit : in out type_unit)
	is begin
		set_selected (unit.status);
	end;	
		
	

	procedure clear_selected (
		unit : in out type_unit)
	is begin
		clear_selected (unit.status);
	end;	


	function is_selected (
		unit : in type_unit)
		return boolean
	is begin
		if is_selected (unit.status) then
			return true;
		else
			return false;
		end if;
	end;
	

	
	procedure set_proposed (
		unit : in out type_unit)
	is begin
		set_proposed (unit.status);
	end;

	
	procedure clear_proposed (
		unit : in out type_unit)
	is begin
		clear_proposed (unit.status);
	end;

	
	function is_proposed (
		unit : in type_unit)
		return boolean
	is begin
		if is_proposed (unit.status) then
			return true;
		else
			return false;
		end if;
	end;



	
	procedure set_moving (
		unit : in out type_unit)
	is begin
		set_moving (unit.status);
	end;


	procedure clear_moving (
		unit : in out type_unit)
	is begin
		clear_moving (unit.status);
	end;

	
	function is_moving (
		unit : in type_unit)
		return boolean
	is begin
		if is_moving (unit.status) then
			return true;
		else
			return false;
		end if;
	end;

	

	
	procedure modify_status (
		unit		: in out type_unit;
		operation	: in type_status_operation)
	is begin
		modify_status (unit.status, operation);
	end modify_status;

	


	procedure reset_status (
		unit : in out type_unit)
	is begin
		reset_status (unit.status);
	end;



	

	procedure move_placeholder (
		unit		: in out type_unit;
		meaning		: in type_placeholder_meaning;					 
		coordinates	: in type_coordinates; -- relative/absolute
		point		: in type_vector_model) -- x/y
	is 
		-- In case absolute movement is required, calculate the
		-- new position of the placeholder relative to the unit origin:
		pos_abs : constant type_vector_model :=
			get_distance_relative (unit.position.place, point);

	begin
		case meaning is
			when NAME =>
				case coordinates is
					when ABSOLUTE =>
						--log (text => "pos " & to_string (point));
						unit.placeholders.name.position := pos_abs;

					when RELATIVE =>
						move_by (
							point	=> unit.placeholders.name.position,
							offset	=> point);
				end case;

				
			when VALUE =>
				case coordinates is
					when ABSOLUTE =>
						unit.placeholders.value.position := pos_abs;

					when RELATIVE =>
						move_by (
							point	=> unit.placeholders.value.position,
							offset	=> point);
				end case;

				
			when PURPOSE =>
				case coordinates is
					when ABSOLUTE =>
						unit.placeholders.purpose.position := pos_abs;

					when RELATIVE =>
						move_by (
							point	=> unit.placeholders.purpose.position,
							offset	=> point);
				end case;

		end case;
	end move_placeholder;


	

	
	
	procedure rotate_placeholder (
		unit		: in out type_unit;
		meaning		: in type_placeholder_meaning;					 
		toggle		: in boolean;
		rotation	: in type_rotation_documentation)
	is begin
		case meaning is
			when NAME =>
				if toggle then
					toggle_rotation (unit.placeholders.name);
				else
					unit.placeholders.name.rotation := rotation;
				end if;

				
			when VALUE =>
				if toggle then
					toggle_rotation (unit.placeholders.value);
				else
					unit.placeholders.value.rotation := rotation;
				end if;

				
			when PURPOSE =>
				if toggle then
					toggle_rotation (unit.placeholders.purpose);
				else
					unit.placeholders.purpose.rotation := rotation;
				end if;

		end case;
	end rotate_placeholder;

	



	

	function get_position (
		unit	: in pac_units.cursor)
		return type_object_position
	is begin
		return get_position (element (unit));
	end get_position;



	function get_rotation (
		unit	: in pac_units.cursor)
		return type_rotation_model
	is begin
		return get_rotation (element (unit));
	end;

	

	function get_mirror_status (
		unit	: in pac_units.cursor)
		return type_mirror
	is begin
		return element (unit).mirror;
	end;


	

	procedure iterate (
		units	: in pac_units.map;
		process	: not null access procedure (position : in pac_units.cursor);
		proceed	: not null access boolean)
	is
		c : pac_units.cursor := units.first;
	begin
		while c /= pac_units.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;


	

	function get_unit_name (
		unit : in pac_units.cursor)
		return pac_unit_name.bounded_string
	is begin
		return key (unit);
	end get_unit_name;



	

	function get_unit_name (
		unit : in pac_units.cursor)
		return string
	is begin
		return pac_unit_name.to_string (key (unit));
	end get_unit_name;


	
	
	
	function to_string (unit : in pac_units.cursor) return string is
		use pac_units;
	begin
		return get_unit_name (unit) 
			--& to_string (type_vector_model (element (unit).position));
			& to_string (element (unit).position.place);
			-- CS output sheet number and rotation ?
	end to_string;





	function is_proposed (
		unit : in pac_units.cursor)
		return boolean
	is begin
		return is_proposed (element (unit));
	end;
	

	function is_selected (
		unit : in pac_units.cursor)
		return boolean
	is begin
		return is_selected (element (unit));
	end;



	
	
	function is_moving (
		unit : in pac_units.cursor)
		return boolean
	is begin
		return is_moving (element (unit));
	end;



	

	function extract_sheets (
		positions	: in pac_unit_positions.map)
		return pac_sheet_numbers.list
	is
		use pac_sheet_numbers;
		use pac_sheet_sorting;
		
		result : pac_sheet_numbers.list;

		
		procedure query_position (c : in pac_unit_positions.cursor) is
			use pac_unit_positions;
			pos : type_object_position := element (c);
			sheet : type_sheet;
		begin
			sheet := get_sheet (pos);
			
			if not result.contains (sheet) then
				result.append (sheet);
			end if;
		end query_position;

		
	begin
		positions.iterate (query_position'access);

		-- Sort the sheets in ascending order:
		sort (result);
		
		return result;
	end extract_sheets;

	

	

	
	
	function get_unit_positions (
		units : in pac_units.map)
		return pac_unit_positions.map
	is
		list : pac_unit_positions.map; -- to be returned
		use pac_units;
		use pac_unit_positions;
		
		procedure query_unit (cursor : pac_units.cursor) is begin
			list.insert (key (cursor), element (cursor).position);
		end;
		
	begin
		iterate (units, query_unit'access);
		return list;
	end get_unit_positions;

	



	function get_port_name (
		port : in pac_dragged_ports.cursor)
		return pac_port_name.bounded_string
	is begin
		return key (port);
	end;


	
	

	function make_drag_list ( 
		ports_old : in pac_symbol_ports.map;
		ports_new : in pac_symbol_ports.map) 
		return pac_dragged_ports.map 
	is
		use et_symbol_ports;
		use pac_dragged_ports;
		drag_list : pac_dragged_ports.map;

		-- ports_old and ports_new are both equally long and contain 
		-- equal keys (the port names). So we use two cursors and advance them
		-- simultaneously in a loop (see below).
		use pac_symbol_ports;
		
		cursor_old : pac_symbol_ports.cursor := ports_old.first;
		cursor_new : pac_symbol_ports.cursor := ports_new.first;

		drag : type_drag;
	begin
		-- Iterate through list ports_old, copy the port name to 
		-- the drag list.
		-- Take the old position from ports_old and 
		-- the new position from ports_new:
		while has_element (cursor_old) loop
			drag := (before => element (cursor_old).position,
					after	=> element (cursor_new).position);
			
			insert (
				container	=> drag_list,
				key			=> key (cursor_old), -- the port name
				new_item	=> drag);
			
			next (cursor_old);
			next (cursor_new);
		end loop;
		
		return drag_list;
	end make_drag_list;



	
end et_units;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
