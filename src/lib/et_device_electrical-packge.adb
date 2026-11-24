------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      DEVICE ELECTRICAL / PACKAGE                         --
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
with et_string_processing;			use et_string_processing;

with et_device_library.packages;	use et_device_library.packages;



package body et_device_electrical.packge is



-- POSITION:

	procedure set_place (
		device	: in out type_device_electrical;
		place	: in type_vector_model)
	is begin
		set_place (device.position, place);
	end set_place;



	procedure move_by (
		device	: in out type_device_electrical;
		offset	: in type_vector_model)
	is begin
		move_by (device.position, offset);
	end move_by;



	function get_place (
		device	: in type_device_electrical)
		return type_vector_model
	is (get_place (device.position));



	function get_place (
		device	: in type_device_electrical;
		format	: in type_output_format := FORMAT_1)
		return string
	is (to_string (get_place (device.position), format));




	procedure set_position (
		device		: in out type_device_electrical;
		position	: in type_board_object)
	is begin
		device.position := position;
	end set_position;


	function get_position (
		device : in type_device_electrical)
		return type_board_object
	is (device.position);



	function get_position (
		device	: in type_device_electrical;
		format	: in type_output_format := FORMAT_1)
		return string
	is (get_position (device.position, format));



	function get_rotation (
		device	: in type_device_electrical)
		return type_rotation_model
	is (get_rotation (device.position));


	procedure set_rotation (
		device		: in out type_device_electrical;
		rotation	: in type_rotation_model)
	is begin
		set_rotation (device.position, rotation);
	end set_rotation;


	procedure set_rotation_relative (
		device		: in out type_device_electrical;
		rotation	: in type_rotation_model)
	is begin
		set_rotation_relative (device.position, rotation);
	end set_rotation_relative;






-- FACE:

	procedure set_face (
		device	: in out type_device_electrical;
		face	: in type_face)
	is begin
		set_face (device.position, face);
	end set_face;


	procedure toggle_face (
		device	: in out type_device_electrical)
	is begin
		toggle_face (device.position);
	end toggle_face;


	function get_face (
		device	: in type_device_electrical)
		return type_face
	is (get_face (device.position));


	function get_face (
		device	: in type_device_electrical)
		return string
	is (to_string (get_face (device.position)));







-- AREA:

	function in_area (
		device	: in type_device_electrical;
		area	: in type_area)
		return boolean
	is (in_area (get_place (device), area));




	function on_face_and_in_area (
		device	: in type_device_electrical;
		face	: in type_face;
		area	: in type_area)
		return boolean
	is
		result : boolean := false;
	begin
		if is_real (device) and then
			get_face (device) = face and then
				in_area (device, area) then
					result := true;
		end if;

		return result;
	end on_face_and_in_area;






-- PLACEHOLDERS:

	procedure set_placeholders (
		device			: in out type_device_electrical;
		placeholders	: in type_text_placeholders)
	is begin
		device.placeholders := placeholders;
	end set_placeholders;



	function get_placeholders_silkscreen (
		device	: in type_device_electrical;
		face	: in type_face)
		return pac_text_placeholders.vector
	is begin
		case face is
			when TOP =>
				return device.placeholders.silkscreen.top;

			when BOTTOM =>
				return device.placeholders.silkscreen.bottom;
		end case;
	end get_placeholders_silkscreen;



	function get_placeholders_assy_doc (
		device	: in type_device_electrical;
		face	: in type_face)
		return pac_text_placeholders.vector
	is begin
		case face is
			when TOP =>
				return device.placeholders.assy_doc.top;

			when BOTTOM =>
				return device.placeholders.assy_doc.bottom;
		end case;
	end get_placeholders_assy_doc;



	function get_placeholders (
		device			: in type_device_electrical)
		return type_text_placeholders
	is (device.placeholders);



	procedure reset_placeholder_positions (
		device		: in out type_device_electrical)
	is
		-- cursor_lib : pac_device_models.cursor;
		default_placeholders : type_text_placeholders;
	begin
		-- Locate the device in the device library:
		-- cursor_lib := get_device_model (device.model_name);

		-- Get the default placeholders as they are specified
		-- in the package model:
		default_placeholders := get_default_placeholders (
			device.model_cursor, get_package_variant (device));

		-- Assign the default placeholders to the given device:
		device.placeholders := default_placeholders;
	end reset_placeholder_positions;




	procedure iterate_placeholders_silkscreen (
		device	: in type_device_electrical;
		face	: in type_face;
		process	: not null access procedure (position : in pac_text_placeholders.cursor))
	is
		use pac_text_placeholders;
		c : pac_text_placeholders.cursor;
	begin
		case face is
			when TOP =>
				c := device.placeholders.silkscreen.top.first;

			when BOTTOM =>
				c := device.placeholders.silkscreen.bottom.first;
		end case;

		while has_element (c) loop
			process (c);
			next (c);
		end loop;
	end iterate_placeholders_silkscreen;




	procedure iterate_placeholders_assy_doc (
		device	: in type_device_electrical;
		face	: in type_face;
		process	: not null access procedure (position : in pac_text_placeholders.cursor))
	is
		use pac_text_placeholders;
		c : pac_text_placeholders.cursor;
	begin
		case face is
			when TOP =>
				c := device.placeholders.assy_doc.top.first;

			when BOTTOM =>
				c := device.placeholders.assy_doc.bottom.first;
		end case;

		while has_element (c) loop
			process (c);
			next (c);
		end loop;
	end iterate_placeholders_assy_doc;




	procedure move_placeholder (
		device		: in out type_device_electrical;
		meaning		: in type_placeholder_meaning;
		layer		: in type_placeholder_layer;
		face		: in type_face;
		index		: in type_placeholder_index;
		coordinates	: in type_coordinates;
		point		: in type_vector_model)
	is begin
		move_placeholder (device.placeholders, meaning, layer,
			face, index, get_position (device), coordinates, point);

	end move_placeholder;






	procedure rotate_placeholder (
		device		: in out type_device_electrical;
		meaning		: in type_placeholder_meaning;
		layer		: in type_placeholder_layer;
		face		: in type_face;
		index		: in type_placeholder_index;
		coordinates	: in type_coordinates;
		rotation	: in type_rotation_model)
	is begin
		rotate_placeholder (device.placeholders, meaning, layer,
			face, index, coordinates, rotation);

	end rotate_placeholder;




	procedure propose_placeholders (
		device				: in out type_device_electrical;
		package_position	: in type_board_object; -- CS: no need. already provided in device
		catch_zone			: in type_catch_zone;
		count				: in out natural;
		log_threshold		: in type_log_level)
	is begin
		propose_placeholders (device.placeholders,
			package_position, catch_zone, count, log_threshold);

	end propose_placeholders;




	procedure modify_status (
		device				: in out type_device_electrical;
		layer				: in type_placeholder_layer;
		face				: in type_face;
		placeholder_cursor	: in pac_text_placeholders.cursor;
		operation			: in type_status_operation)
	is begin
		modify_status (
			placeholders		=> device.placeholders,
			layer				=> layer,
			face				=> face,
			placeholder_cursor	=> placeholder_cursor,
			operation			=> operation);

	end modify_status;



	procedure reset_placeholder_status (
		device : in out type_device_electrical)
	is begin
		reset_status (device.placeholders);
	end reset_placeholder_status;



end et_device_electrical.packge;


-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
