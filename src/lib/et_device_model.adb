------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                            DEVICE MODEL                                  --
--                                                                          --
--                              B o d y                                     --
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
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;

with et_string_processing;		use et_string_processing;
with et_exceptions;				use et_exceptions;


package body et_device_model is

	
	function to_full_name (
		device_name	: in type_device_name; -- IC34
		symbol_name	: in pac_unit_name.bounded_string; -- PWR
		unit_count	: in type_unit_count) -- the total number of units
		return string is -- IC34.PWR
	begin
		if unit_count > 1 then
			return to_string (device_name) 
				& device_unit_separator 
				& to_string (symbol_name);
		else
			return to_string (device_name);
		end if;
	end to_full_name;

	
	


	
	function rotate_placeholders (
		symbol_cursor	: in pac_units_internal.cursor;
		destination		: in et_coordinates_2.type_position)
		return type_rotated_placeholders
	is
		use pac_units_internal;
		use et_coordinates_2.pac_geometry_sch;

		r : type_rotated_placeholders; -- to be returned
	begin
		r.name		:= element (symbol_cursor).symbol.name;
		r.value		:= element (symbol_cursor).symbol.value;
		r.purpose	:= element (symbol_cursor).symbol.purpose;

		-- rotate the positions of placeholders according to rotation given by caller:
		rotate (r, get_rotation (destination));
		
		return r;
	end rotate_placeholders;



	
end et_device_model;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
