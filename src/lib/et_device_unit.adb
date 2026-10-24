------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             DEVICE UNIT                                  --
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
-- <http://www.gnu.org/licenses/>.
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

-- with ada.text_io;			use ada.text_io;
with et_exceptions;						use et_exceptions;


package body et_device_unit is



	function is_real (
		unit : in type_unit)
		return boolean
	is (unit.appearance = APPEARANCE_PCB);



	function to_unit (
		appearance		: in type_appearance_schematic;
		object			: in type_schematic_object;
		mirror			: in type_mirror := MIRROR_NO;
		placeholders	: in type_text_placeholders := default_text_placeholders)
		return type_unit
	is
		unit : type_unit (appearance => appearance);
	begin
		case unit.appearance is
			when APPEARANCE_VIRTUAL =>
				unit := (object with APPEARANCE_VIRTUAL, mirror);

			when APPEARANCE_PCB =>
				unit := (object with APPEARANCE_PCB, mirror, placeholders);

		end case;

		return unit;
	end to_unit;


	-- function get_place (
	-- 	unit	: in type_unit)
	-- 	return type_vector_model
	-- is (get_place (unit.position));



	function get_position (
		unit	: in type_unit)
		return type_schematic_object
	is (type_schematic_object (unit));



	-- procedure set_position (
	-- 	unit		: in out type_unit;
	-- 	position	: in type_object_position)
	-- is begin
	-- 	unit.position := position;
	-- end set_position;




	function get_mirror_status (
		unit	: in type_unit)
		return type_mirror
	is (unit.mirror_status);


	function get_mirror_status (
		unit	: in type_unit)
		return string
	is (type_mirror'image (unit.mirror_status));


	procedure set_mirror_status (
		unit	: in out type_unit;
		mirror	: in type_mirror)
	is begin
		unit.mirror_status := mirror;
	end set_mirror_status;



	procedure toggle_mirror_status (
		unit	: in out type_unit)
	is begin
		toggle_along_y (unit.mirror_status);
	end toggle_mirror_status;




	-- function get_rotation (
	-- 	unit	: in type_unit)
	-- 	return type_rotation_model
	-- is (get_rotation (unit.position));

	-- procedure set_rotation (
	-- 	unit		: in out type_unit;
	-- 	rotation	: in type_rotation_model)
	-- is begin
	-- 	set_rotation (unit.position, rotation);
	-- end set_rotation;




	-- procedure rotate_by (
	-- 	unit		: in out type_unit;
	-- 	rotation	: in type_rotation_model)
	-- is
	-- 	-- Get the rotation before:
	-- 	r_actual : type_rotation_model := get_rotation (unit);
	-- begin
	-- 	-- Add the given rotation to the actual rotation:
	-- 	add (r_actual, rotation);

	-- 	-- Assign the new rotation:
	-- 	set_rotation (unit, r_actual);
	-- end rotate_by;

	-- function get_sheet (
	-- 	unit	: in type_unit)
	-- 	return type_sheet
	-- is (get_sheet (unit.position));

	-- procedure set_sheet (
	-- 	unit	: in out type_unit;
	-- 	sheet	: in type_sheet)
	-- is begin
	-- 	set_sheet (unit.position, sheet);
	-- end set_sheet;




	-- procedure move_unit (
	-- 	unit	: in out type_unit;
	-- 	offset	: in type_vector_model)
	-- is begin
	-- 	move_by (unit.position, offset);
	-- end move_unit;



	-- procedure move_unit (
	-- 	unit	: in out type_unit;
	-- 	offset	: in type_sheet_relative)
	-- is begin
	-- 	move_by_sheets (unit.position, offset);
	-- end move_unit;





	-- function in_catch_zone (
	-- 	unit	: in type_unit;
	-- 	zone	: in type_catch_zone;
	-- 	sheet	: in type_sheet)
	-- 	return boolean
	-- is
	-- 	result : boolean := false;

	-- 	unit_position : constant type_object_position := get_position (unit);
	-- begin
	-- 	-- The unit must be on the given sheet and
	-- 	-- in the given catch zone:
	-- 	if	get_sheet (unit_position) = sheet
	-- 	and	in_catch_zone (zone, get_place (unit_position)) then
	-- 		result := true;
	-- 	else
	-- 		result := false;
	-- 	end if;

	-- 	return result;
	-- end in_catch_zone;







	-- function on_sheet_and_in_area (
	-- 	unit	: in type_unit;
	-- 	sheet	: in type_sheet;
	-- 	area	: in type_area)
	-- 	return boolean
	-- is
	-- 	result : boolean := false;
	-- 	place : type_vector_model;
	-- begin
	-- 	-- The given unit must be on the given sheet:
	-- 	if get_sheet (unit) = sheet then

	-- 		-- Get the x/y position of the unit:
	-- 		place := get_place (get_position (unit));

	-- 		-- The unit must be in the given area:
	-- 		if in_area (place, area) then
	-- 			result := true;
	-- 		else
	-- 			result := false;
	-- 		end if;

	-- 	else
	-- 		result := false;
	-- 	end if;

	-- 	return result;
	-- end on_sheet_and_in_area;









	-- function on_sheet_and_selected (
	-- 	unit	: in type_unit;
	-- 	sheet	: in type_sheet)
	-- 	return boolean
	-- is
	-- 	result : boolean := false;
	-- begin
	-- 	if get_sheet (unit) = sheet then
	-- 		if is_selected (unit) then
	-- 			result := true;
	-- 		end if;
	-- 	end if;

	-- 	return result;
	-- end on_sheet_and_selected;







	procedure unit_not_found (
		name : in type_unit_name)
	is begin
		raise semantic_error_1 with
			"ERROR: Unit " & to_string (name) & " not found !";
	end unit_not_found;




	-- procedure set_selected (
	-- 	unit : in out type_unit)
	-- is begin
	-- 	set_selected (unit.status);
	-- end set_selected;

	-- procedure clear_selected (
	-- 	unit : in out type_unit)
	-- is begin
	-- 	clear_selected (unit.status);
	-- end clear_selected;

	-- function is_selected (
	-- 	unit : in type_unit)
	-- 	return boolean
	-- is (is_selected (unit.status));

	-- procedure set_proposed (
	-- 	unit : in out type_unit)
	-- is begin
	-- 	set_proposed (unit.status);
	-- end set_proposed;

	-- procedure clear_proposed (
	-- 	unit : in out type_unit)
	-- is begin
	-- 	clear_proposed (unit.status);
	-- end clear_proposed;

	-- function is_proposed (
	-- 	unit : in type_unit)
	-- 	return boolean
	-- is (is_proposed (unit.status));

	-- procedure set_moving (
	-- 	unit : in out type_unit)
	-- is begin
	-- 	set_moving (unit.status);
	-- end set_moving;

	-- procedure clear_moving (
	-- 	unit : in out type_unit)
	-- is begin
	-- 	clear_moving (unit.status);
	-- end clear_moving;

	-- function is_moving (
	-- 	unit : in type_unit)
	-- 	return boolean
	-- is (is_moving (unit.status));

	-- procedure modify_status (
	-- 	unit		: in out type_unit;
	-- 	operation	: in type_status_operation)
	-- is begin
	-- 	modify_status (unit.status, operation);
	-- end modify_status;

	-- procedure reset_status (
	-- 	unit : in out type_unit)
	-- is begin
	-- 	reset_status (unit.status);
	-- end reset_status;








-- PLACEHOLDERS:


	function get_placeholders (
		unit : in type_unit)
		return type_text_placeholders
	is begin
		if is_real (unit) then
			return unit.placeholders;
		else
			return (others => <>);
		end if;
	end get_placeholders;


	procedure set_placeholders (
		unit			: in out type_unit;
		placeholders	: in type_text_placeholders)
	is begin
		unit.placeholders := placeholders;
	end set_placeholders;



	procedure move_placeholder (
		unit		: in out type_unit;
		meaning		: in type_placeholder_meaning;
		coordinates	: in type_coordinates; -- relative/absolute
		point		: in type_vector_model) -- x/y
	is
		-- In case absolute movement is required, calculate the
		-- new position of the placeholder relative to the unit origin:
		pos_abs : constant type_vector_model :=
			get_distance_relative (get_place (unit), point);

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




end et_device_unit;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
