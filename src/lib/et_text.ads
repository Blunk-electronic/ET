------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                TEXT                                      --
--                                                                          --
--                               S p e c                                    --
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

with ada.strings.maps;			use ada.strings.maps;

with et_geometry_2a;
with et_logging;				use et_logging;
with et_alignment;				use et_alignment;
with et_object_status;			use et_object_status;
with et_text_content;			use et_text_content;
with et_rotation_docu;			use et_rotation_docu;


package et_text is
	
	generic
		with package pac_geometry is new et_geometry_2a (<>);
		size_min		: pac_geometry.type_distance_positive;
		size_max		: pac_geometry.type_distance_positive;
		size_default	: pac_geometry.type_distance_positive;		

		
	package generic_pac_text is
		use pac_geometry;

		use pac_geometry_1;
		-- NOTE: This use clause does not work properly. 
		-- For some reason the package name must be explicitely provided
		-- for stuff that stems from pac_geometry_1.
		-- Otherwise the linker reports lots of "undefined references" ...

		
		subtype type_text_size is pac_geometry.type_distance_positive 
			range size_min .. size_max; -- in millimeters
	
		

		
		-- Converts given distance to type_text_size. Raises error on excessive text size.
		function to_text_size (size : in pac_geometry.type_distance) return type_text_size;

		

		
		
		procedure validate_text_size (size : in pac_geometry.type_distance);
		-- Checks whether given text size is in range of type_text_size.

		


		-- This is the root type of all text types:
		--type type_text is abstract tagged record
		type type_text is tagged record
			size		: type_text_size := size_default;
			alignment	: type_text_alignment;
			status		: et_object_status.type_object_status;
		end record;


		-- Resets size, alignment and status to default:
		procedure reset_text (
			text : in out type_text);
		
		-- CS procedures to set size and alignment
		-- CS procedures to get size and alignment
		
		

		function is_proposed (
			text : in type_text)
			return boolean;

		procedure set_proposed (
			text : in out type_text);

		procedure clear_proposed (
			text : in out type_text);
		

		
		function is_moving (
			text : in type_text)
			return boolean;

		procedure set_moving (
			text : in out type_text);

		procedure clear_moving (
			text : in out type_text);

		

		function is_selected (
			text : in type_text)
			return boolean;

		procedure set_selected (
			text : in out type_text);

		procedure clear_selected (
			text : in out type_text);

		

		procedure modify_status (
			text 		: in out type_text;
			operation	: in et_object_status.type_status_operation);


		-- Clears all status flags of the given text:
		procedure reset_status (
		   text 		: in out type_text);
		
		
		-- Returns the properties of the given text in a long single string.	
		function text_properties (
			text : in type_text)
			return string;


		
		origin_half_size : constant pac_geometry.type_distance_positive := 0.5; -- CS type_float_positive ?
		origin_line_width : constant pac_geometry.type_distance_positive := 0.01; -- CS type_float_positive ?

		
		-- Converts HORIZONTAL/VERTICAL to 0.0/90.0 degrees:
		function to_rotation (rotation : in type_rotation_documentation) 
			return pac_geometry.type_rotation;

		
		-- Converts HORIZONTAL/VERTICAL to 0.0/90.0 degrees as string:
		function to_string (rotation : in type_rotation_documentation) return string;

		
		-- Adds HORIZONTAL/VERTICAL (which is 0/90 degrees) to rotation_add:
		function "+" (
			rotation_doc	: in type_rotation_documentation;
			rotation_add	: in pac_geometry.type_rotation)
			return pac_geometry.type_rotation;

		
		-- Issues a warning that the given angle is neither 0 or 90 degrees.
		procedure warning_rotation_outside_range; -- CS argument for angle ?

		
		-- Converts an angle to either HORIZONTAL or VERTICAL. 
		-- This is required for documentational text which
		-- must be readable from the front or from the right.
		-- Examples: 
		-- - If rotation is 0 degree, then the return is HORIZONTAL.
		-- - If rotation is 40 degree, then the return is HORIZONTAL.
		-- - If rotation is 45 degree, then the return is HORIZONTAL.
		-- - If rotation is 46 degree, then the return is VERTICAL.
		-- - If rotation is 50 degree, then the return is VERTICAL.
		-- - If rotation is 135 degree, then the return is VERTICAL.		
		-- - If rotation is 170 degree, then the return is HORIZONTAL.		
		-- - If rotation is 270 degree, then the return is VERTICAL.		
		function to_rotation_doc (
			rotation : in pac_geometry.type_rotation) 
			return type_rotation_documentation;

		
		-- Converts a string like "0.0" or "90.0" to HORIZONTAL or VERTICAL.
		function to_rotation_doc (
			rotation : in string) 
			return type_rotation_documentation;		

		
		
	end generic_pac_text;

end et_text;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
