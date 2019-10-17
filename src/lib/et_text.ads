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

with et_string_processing;

package et_text is
	
	generic
		type type_distance is delta <>;
-- 		type type_rotation is delta <>;
		
	package text is

		text_size_min : constant type_distance := 1.0;
		text_size_max : constant type_distance := 100.0;
		subtype type_text_size is type_distance range text_size_min .. text_size_max;
		
		type type_text_dimensions is record
			width	: type_text_size := text_size_min;
			height	: type_text_size := text_size_min;
		end record;

		function to_string (size : in type_distance) return string;
-- 		function to_string (rotation : in type_rotation) return string;
		
		procedure validate_text_size (size : in type_distance);
		-- Checks whether given text size is in range of type_text_size.


		keyword_line_width : constant string := "line_width";	
		line_width_min : constant type_distance := 0.15;
		line_width_max : constant type_distance := 10.0;
		subtype type_text_line_width is type_distance range line_width_min .. line_width_max;

		procedure validate_text_line_width (width : in type_distance);
		-- Checks whether given line width is in range of type_text_line_width

		
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

		
		
		type type_text is abstract tagged record
			dimensions	: type_text_dimensions;
			line_width	: type_text_line_width := type_text_line_width'first;
-- 			alignment	: et_libraries.type_text_alignment;

		end record;

		function text_properties (text : in type_text) return string;
		-- Returns the properties of the given text in a long single string.	

			
	-- 	private

	end text;

end et_text;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
