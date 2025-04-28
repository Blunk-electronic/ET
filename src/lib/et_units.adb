------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          DEVICE UNITS IN SCHEMATIC                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                -- 
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
-- with ada.strings.maps;					use ada.strings.maps;
-- with ada.strings.bounded;   		    use ada.strings.bounded;
with ada.characters.handling;			use ada.characters.handling;
with ada.exceptions;
with et_exceptions;						use et_exceptions;


package body et_units is


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



	
	
	function to_string (unit : in pac_units.cursor) return string is
		use pac_units;
	begin
		return to_string (key (unit)) 
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

	
	
	function unit_positions (
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
	end unit_positions;

	
	
end et_units;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
