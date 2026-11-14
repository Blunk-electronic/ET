------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         DEVICE NON-ELECTRICAL                            --
--                                                                          --
--                              B o d y                                     --
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

-- with ada.text_io;			use ada.text_io;
with et_board_text;
with ada.characters.latin_1;
with ada.strings.unbounded;

with et_logging;
with et_string_processing;			use et_string_processing;


package body et_device_non_electrical is


	procedure device_name_in_use (
		name : in type_device_name)
	is
		use et_logging;
	begin
		log (SEVERITY_ERROR, "Name " & enclose_in_quotes (to_string (name)) &
			 " already used by another non-electrical device !",
			 console => true);

		raise constraint_error;
	end device_name_in_use;




	function to_mirror_along_y_axis (
		flipped : in type_flipped)
		return type_mirror
	is begin
		case flipped is
			when YES => return MIRROR_ALONG_Y_AXIS;
			when NO =>  return MIRROR_NO;
		end case;
	end to_mirror_along_y_axis;



	procedure reset_device (
		device	: in out type_device_non_electrical)
	is
		d : type_device_non_electrical;
	begin
		device := d;
	end reset_device;





	function get_position (
		device : in type_device_non_electrical)
		return type_board_object
	is (type_board_object (device));



	function get_package_model_name (
		device	: in type_device_non_electrical)
		return type_package_model_name
	is
		use pac_package_models;
	begin
		return key (device.model_cursor);
	end get_package_model_name;



	function get_package_model_name (
		device : in type_device_non_electrical)
		return string
	is (to_string (get_package_model_name (device)));








	-- procedure set_rotation_relative (
	-- 	device		: in out type_device_non_electrical;
	-- 	rotation	: in type_rotation_model)
	-- is begin
	-- 	set_rotation_relative (device.position, rotation);
	-- end set_rotation_relative;




-- VALUE:

	procedure set_value (
		device	: in out type_device_non_electrical;
		value	: in type_device_value)
	is begin
		device.value := value;
	end set_value;


	function get_value (
		device	: in type_device_non_electrical)
		return type_device_value
	is (device.value);


	function get_value (
		device	: in type_device_non_electrical)
		return string
	is (to_string (get_value (device)));


	function has_value (
		device	: in type_device_non_electrical)
		return boolean
	is begin
		if is_empty (get_value (device)) then
			return false;
		else
			return true;
		end if;
	end has_value;



-- PARTCODE:

	procedure set_partcode (
		device		: in out type_device_non_electrical;
		partcode	: in type_device_partcode)
	is begin
		device.partcode := partcode;
	end set_partcode;


	function get_partcode (
		device	: in type_device_non_electrical)
		return type_device_partcode
	is (device.partcode);


	function get_partcode (
		device	: in type_device_non_electrical)
		return string
	is (to_string (get_partcode (device)));


	function has_partcode (
		device	: in type_device_non_electrical)
		return boolean
	is begin
		if is_empty (get_partcode (device)) then
			return false;
		else
			return true;
		end if;
	end has_partcode;




-- PURPOSE:

	procedure set_purpose (
		device	: in out type_device_non_electrical;
		purpose	: in type_device_purpose)
	is begin
		device.purpose := purpose;
	end set_purpose;


	function get_purpose (
		device	: in type_device_non_electrical)
		return type_device_purpose
	is (device.purpose);


	function get_purpose (
		device	: in type_device_non_electrical)
		return string
	is (to_string (get_purpose (device)));


	function has_purpose (
		device	: in type_device_non_electrical)
		return boolean
	is begin
		if is_empty (get_purpose (device)) then
			return false;
		else
			return true;
		end if;
	end has_purpose;




-- BOM:


	function is_bom_relevant (
		device : in type_device_non_electrical)
		return boolean
	is
		use pac_package_models;
	begin
		return is_bom_relevant (key (device.model_cursor));
	end is_bom_relevant;





-- PLACEHOLDERS:



	procedure reset_placeholder_positions (
		device		: in out type_device_non_electrical)
	is
		cursor_lib : pac_package_models.cursor;
		default_placeholders : type_text_placeholders;
	begin
		-- Locate the device in the package library:
		cursor_lib := device.model_cursor;

		-- Get the default placeholders as they are specified
		-- in the package model:
		default_placeholders := get_default_placeholders (cursor_lib);

		-- Assign the default placeholders to the device:
		device.placeholders := default_placeholders;
	end reset_placeholder_positions;





	procedure move_placeholder (
		device		: in out type_device_non_electrical;
		meaning		: in type_placeholder_meaning;
		layer		: in type_placeholder_layer;
		face		: in type_face;
		index		: in type_placeholder_index;
		coordinates	: in type_coordinates; -- relative/absolute
		point		: in type_vector_model) -- x/y
	is begin
		move_placeholder (device.placeholders, meaning, layer,
			face, index, device.get_position,
			coordinates, point);

	end move_placeholder;






	procedure rotate_placeholder (
		device		: in out type_device_non_electrical;
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










	function get_device_properties (
		device		: in type_device_non_electrical;
		level		: in type_properties_level;
		linebreaks	: in boolean := false)
		return string
	is
		use ada.strings.unbounded;
		result : unbounded_string;


		-- If linebreaks are requested by the caller, then
		-- this function returns a linefeed character on each call.
		-- If no linefeeds are requested, then the return is an empty string:
		function ins_LF return string is
			use ada.characters.latin_1;
		begin
			if linebreaks then
				return "" & LF;
			else
				return "";
			end if;
		end ins_LF;


		-- CS: Refinement required about what
		-- should be output on which level.

		procedure get_info_1 is begin
			if has_value (device) then
				result := to_unbounded_string (" value: "
					& get_value (device) & ins_LF);
			end if;

			if has_purpose (device) then
				result := to_unbounded_string (" purpose: "
					& get_purpose (device) & ins_LF);
			end if;
		end get_info_1;


		procedure get_info_2 is begin
			if has_partcode (device) then
				result := result & " partcode: "
					& get_partcode (device) & ins_LF;
			end if;
		end get_info_2;


		procedure get_info_3 is begin
			result := result & " package model: "
				& to_string (get_package_model_name (device)) & ins_LF;
		end get_info_3;



	begin
		case level is
			when DEVICE_PROPERTIES_LEVEL_1 =>
				get_info_1;

			when DEVICE_PROPERTIES_LEVEL_2 =>
				get_info_1;
				get_info_2;

			when DEVICE_PROPERTIES_LEVEL_3 =>
				get_info_1;
				get_info_2;
				get_info_3;
		end case;

		return to_string (result);
	end get_device_properties;



end et_device_non_electrical;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
