------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                TEXT                                      --
--                                                                          --
--                               S p e c                                    --
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

with ada.strings.bounded; 		use ada.strings.bounded;
with et_geometry;
with et_string_processing;

package et_text is

-- TEXT ALIGNMENT
	keyword_alignment	: constant string := "alignment";
	keyword_horizontal	: constant string := "horizontal";
	keyword_vertical	: constant string := "vertical";		

	
	type type_text_alignment_horizontal is (LEFT, CENTER, RIGHT);
	function to_string (alignment : in type_text_alignment_horizontal) return string;
	function to_alignment_horizontal (alignment : in string) return type_text_alignment_horizontal;
	
	type type_text_alignment_vertical is (TOP, CENTER, BOTTOM);
	function to_string (alignment : in type_text_alignment_vertical) return string;
	function to_alignment_vertical (alignment : in string) return type_text_alignment_vertical;
	
	type type_text_alignment is record
		horizontal	: type_text_alignment_horizontal := CENTER;
		vertical	: type_text_alignment_vertical := CENTER;
	end record;

	function to_alignment (
		line : in et_string_processing.type_fields_of_line; -- "alignment horizontal center vertical center"
		from : in positive)
		return type_text_alignment;
	
	function to_string (alignment : in type_text_alignment) return string;
	
-- TEXT CONTENT
	-- A text may have up to 200 characters which seems sufficient for now.
	keyword_content : constant string := "content";
	
	text_length_max : constant natural := 200;
	package type_text_content is new generic_bounded_length (text_length_max); -- CS rename to pac_text_content

	function to_string (text_content : in type_text_content.bounded_string) return string;
	function to_content (content : in string) return type_text_content.bounded_string;
	
	procedure check_text_content_length (content : in string);
	-- Tests if the content is not longer than allowed.


	keyword_line_width	: constant string := "line_width";	
	keyword_size		: constant string := "size";

	keyword_meaning		: constant string := "meaning";	-- for placeholders


	
	generic
		with package geometry is new et_geometry.geometry_operations_2d (<>);
		
		size_min, size_max, size_default : geometry.type_distance;
		line_width_min, line_width_max, line_width_default : geometry.type_distance;
		
	package text is
		use geometry;

		subtype type_text_size is type_distance range size_min .. size_max; -- in millimeters
		subtype type_text_line_width is type_distance range line_width_min .. line_width_max;
		
		function to_text_size (size : in type_distance) return type_text_size;
		-- Converts given distance to type_text_size. Raises error on excessive text size.
		
		procedure validate_text_size (size : in type_distance);
		-- Checks whether given text size is in range of type_text_size.

		procedure validate_text_line_width (width : in type_distance);
		-- Checks whether given line width is in range of type_text_line_width
		
		type type_text is abstract tagged record
			size		: type_text_size := size_default;
			line_width	: type_text_line_width := line_width_default;
			alignment	: type_text_alignment;
		end record;

		function text_properties (text : in type_text) return string;
		-- Returns the properties of the given text in a long single string.	

		subtype type_rotation_documentation is type_rotation range 0.0 .. 90.0;

		procedure warning_rotation_outside_range;
		
	-- 	private

	end text;

end et_text;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
