------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                TEXT                                      --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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
with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_ordered_maps;

with cairo;
with et_geometry;
with et_string_processing;

package et_text is

-- TEXT ALIGNMENT
	keyword_alignment	: constant string := "alignment";
	keyword_horizontal	: constant string := "horizontal";
	keyword_vertical	: constant string := "vertical";		

	-- The alignment refers to the anchor point of the text.
	-- The anchor point is usually where the origin of the text is.
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

-- FONT
	font_family_length_max : constant positive := 50;
	package pac_font_family is new generic_bounded_length (font_family_length_max);

	function to_string (family : in pac_font_family.bounded_string) return string;
	function to_family (family : in string) return pac_font_family.bounded_string;
	
	type type_font is record
		family	: pac_font_family.bounded_string; -- string := "monospace";
		slant	: cairo.cairo_font_slant := cairo.CAIRO_FONT_SLANT_NORMAL;
		weight	: cairo.cairo_font_weight := cairo.CAIRO_FONT_WEIGHT_NORMAL;
	end record;

	
-- CONTENT
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


	type type_rotation_documentation is (HORIZONTAL, VERTICAL);

	
-- VECTORIZED TEXT

	-- In vector text a single character consists of straight lines.
	-- CS: It seems sufficient to have at most 20 lines per character.
	type type_segment_id is range 1 .. 20; 

	vector_text_alignment_default : constant type_text_alignment := (LEFT, BOTTOM);

	type type_vector_text_mirrored is (NO, YES);
	vector_text_mirror_default : constant type_vector_text_mirrored := NO;

	
	
	generic
		with package pac_shapes is new et_geometry.generic_pac_shapes (<>);
		
		size_min, size_max, size_default : pac_shapes.pac_geometry.type_distance;
		line_width_min, line_width_max, line_width_default : pac_shapes.pac_geometry.type_distance;
		
	package generic_pac_text is
		--use pac_shapes.pac_geometry;
		use pac_shapes;
		use pac_geometry;

		subtype type_text_size is type_distance range size_min .. size_max; -- in millimeters
		subtype type_text_line_width is type_distance range line_width_min .. line_width_max;

		-- With this line uncommented the linker does not output any errors:
		function to_text_size (size : in pac_geometry.type_distance) return type_text_size;

		-- With this line uncommented the linker outputs errors like "undefined reference ..."
		-- function to_text_size (size : in type_distance) return type_text_size;
		
		-- Converts given distance to type_text_size. Raises error on excessive text size.
		
		procedure validate_text_size (size : in type_distance);
		-- Checks whether given text size is in range of type_text_size.

		procedure validate_text_line_width (width : in type_distance);
		-- Checks whether given line width is in range of type_text_line_width
		
		type type_text is abstract tagged record
			size		: type_text_size := size_default;
			alignment	: type_text_alignment;
		end record;

		function text_properties (text : in type_text) return string;
		-- Returns the properties of the given text in a long single string.	

		origin_half_size : constant type_distance_positive := 0.5;
		origin_line_width : constant type_distance_positive := 0.01;
		
		-- Converts HORIZONTAL/VERTICAL to 0.0/90.0 degrees:
		function to_rotation (rotation : in type_rotation_documentation) 
			return type_rotation;

		-- Converts HORIZONTAL/VERTICAL to 0.0/90.0 degrees as string:
		function to_string (rotation : in type_rotation_documentation) return string;

		-- Adds HORIZONTAL/VERTICAL (which is 0/90 degrees) to rotation_add:
		function "+" (
			rotation_doc	: in type_rotation_documentation;
			rotation_add	: in type_rotation)
			return type_rotation;

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
		function snap (rotation : in pac_geometry.type_rotation) return type_rotation_documentation;
		
		-- Converts a string like "0.0" or "90.0" to HORIZONTAL or VERTICAL.
		function to_rotation_doc (rotation : in string) return type_rotation_documentation;		


		
	-- VECTORIZED TEXT
		
-- 		type type_line_with_to_size_ratio is range 1 .. 50; -- in percent
-- 		line_width_to_size_ratio_default : constant type_line_with_to_size_ratio := 15;

		type type_vector_text_line is new pac_shapes.type_line with null record;

		subtype type_character_height is type_distance range -0.4 .. 1.0;
		subtype type_character_width  is type_distance_positive range zero .. 0.7;		
		character_width : constant type_character_height := 0.7;

		-- A single segment of a character is defined as:
		type type_segment is record 
			start_x	: type_character_width;
			start_y : type_character_height;
			end_x	: type_character_width;
			end_y	: type_character_height;
		end record;

		type type_character is array (type_segment_id range <>) of type_segment;

		x0 : constant type_character_width := 0.0;
		x1 : constant type_character_width := 0.2;
		x2 : constant type_character_width := 0.35;
		x3 : constant type_character_width := 0.5;
		x4 : constant type_character_width := 0.7;

		y6 : constant type_character_height := 1.0;
		y5 : constant type_character_height := 0.8 + 0.05;
		y4 : constant type_character_height := 0.7 - 0.05;
		y3 : constant type_character_height := 0.5;
		y2 : constant type_character_height := 0.3 + 0.05;
		y1 : constant type_character_height := 0.2 - 0.05;
		y0 : constant type_character_height := 0.0;
		
		y7 : constant type_character_height := -0.2; -- used for lower case letters only
		y8 : constant type_character_height := -0.4; -- used for lower case letters only
		
		-- UPPER CASE LETTERS:
		capital_a : constant type_character (1 .. 5) := (
			1	=> (x0, y0, x0, y4), -- |
			2	=> (x0, y4, x2, y6), -- /
			3	=> (x2, y6, x4, y4), -- \
			4	=> (x4, y4, x4, y0), -- |
			5	=> (x0, y3, x4, y3)  -- -
			);

		capital_b : constant type_character (1 ..10) := (
			 1	=> (x0, y0, x0, y6), -- |
			 2	=> (x0, y6, x3, y6), -- -
			 3	=> (x3, y6, x4, y5), -- \
			 4	=> (x4, y5, x4, y4), -- |
			 5	=> (x4, y4, x3, y3), -- /
			 6	=> (x0, y3, x3, y3), -- -
			 7	=> (x3, y3, x4, y2), -- \
			 8	=> (x4, y2, x4, y1), -- |
			 9	=> (x4, y1, x3, y0), -- /
			10	=> (x3, y0, x0, y0)  -- _
			);
		
		capital_c : constant type_character (1 .. 7) := (
			 1	=> (x4, y5, x3, y6),
			 2	=> (x3, y6, x1, y6),
			 3	=> (x1, y6, x0, y5),
			 4	=> (x0, y5, x0, y1),
			 5	=> (x0, y1, x1, y0),
			 6	=> (x1, y0, x3, y0),
			 7	=> (x3, y0, x4, y1)
			);

		capital_d : constant type_character (1 .. 6) := (
			 1	=> (x0, y6, x0, y0),
			 2	=> (x0, y6, x3, y6),
			 3	=> (x3, y6, x4, y5),
			 4	=> (x4, y5, x4, y1),
			 5	=> (x4, y1, x3, y0),
			 6	=> (x3, y0, x0, y0)
			);

		capital_e : constant type_character (1 .. 4) := (
			 1	=> (x0, y6, x0, y0),
			 2	=> (x0, y6, x4, y6),
			 3	=> (x0, y3, x3, y3),
			 4	=> (x0, y0, x4, y0)
			);

		capital_f : constant type_character (1 .. 3) := (
			 1	=> (x0, y6, x0, y0),
			 2	=> (x0, y6, x4, y6),
			 3	=> (x0, y3, x3, y3)
			);
		
		capital_g : constant type_character (1 .. 9) := (
			 1	=> (x4, y5, x3, y6),
			 2	=> (x3, y6, x1, y6),
			 3	=> (x1, y6, x0, y5),
			 4	=> (x0, y5, x0, y1),
			 5	=> (x0, y1, x1, y0),
			 6	=> (x1, y0, x3, y0),
			 7	=> (x3, y0, x4, y1),
			 8	=> (x4, y1, x4, y3),
			 9	=> (x4, y3, x2, y3)
			);

		capital_h : constant type_character (1 .. 3) := (
			 1	=> (x0, y6, x0, y0),
			 2	=> (x4, y6, x4, y0),
			 3	=> (x0, y3, x4, y3)
			);

		capital_i : constant type_character (1 .. 3) := (
			 1	=> (x2, y6, x2, y0),
			 2	=> (x1, y6, x3, y6),
			 3	=> (x1, y0, x3, y0)
			);

		capital_j : constant type_character (1 .. 5) := (
			 1	=> (x2, y6, x4, y6),
			 2	=> (x3, y6, x3, y1),
			 3	=> (x3, y1, x2, y0),
			 4	=> (x2, y0, x1, y0),
			 5	=> (x1, y0, x0, y1)
			 );
		
		capital_k : constant type_character (1 .. 3) := (
			 1	=> (x0, y6, x0, y0),
			 2	=> (x0, y3, x4, y6),
			 3	=> (x0, y3, x4, y0)
			);

		capital_l : constant type_character (1 .. 2) := (
			 1	=> (x0, y6, x0, y0),
			 2	=> (x0, y0, x4, y0)
			);

		capital_m : constant type_character (1 .. 4) := (
			 1	=> (x0, y6, x0, y0),
			 2	=> (x0, y6, x2, y3),
			 3	=> (x2, y3, x4, y6),
			 4	=> (x4, y6, x4, y0)
			);

		capital_n : constant type_character (1 .. 3) := (
			 1	=> (x0, y6, x0, y0),
			 2	=> (x0, y6, x4, y0),
			 3	=> (x4, y6, x4, y0)
			);

		capital_o : constant type_character (1 .. 8) := (
			 1	=> (x0, y5, x0, y1),
			 2	=> (x0, y1, x1, y0),
			 3	=> (x1, y0, x3, y0),
			 4	=> (x3, y0, x4, y1),
			 5	=> (x4, y1, x4, y5),
			 6	=> (x4, y5, x3, y6),
			 7	=> (x3, y6, x1, y6),
			 8	=> (x1, y6, x0, y5)
			);

		capital_p : constant type_character (1 .. 6) := (
			 1	=> (x0, y6, x0, y0),
			 2	=> (x0, y6, x3, y6),
			 3	=> (x3, y6, x4, y5),
			 4	=> (x4, y5, x4, y4),
			 5	=> (x4, y4, x3, y3),
			 6	=> (x3, y3, x0, y3)
			);

		capital_q : constant type_character (1 .. 9) := (
			 1	=> (x0, y5, x0, y1),
			 2	=> (x0, y1, x1, y0),
			 3	=> (x1, y0, x3, y0),
			 4	=> (x3, y0, x4, y1),
			 5	=> (x4, y1, x4, y5),
			 6	=> (x4, y5, x3, y6),
			 7	=> (x3, y6, x1, y6),
			 8	=> (x1, y6, x0, y5),
			 9	=> (x3, y1, x4, y0)
			);

		capital_r : constant type_character (1 .. 7) := (
			 1	=> (x0, y6, x0, y0),
			 2	=> (x0, y6, x3, y6),
			 3	=> (x3, y6, x4, y5),
			 4	=> (x4, y5, x4, y4),
			 5	=> (x4, y4, x3, y3),
			 6	=> (x3, y3, x0, y3),
			 7	=> (x3, y3, x4, y0)
			 );
		
		capital_s : constant type_character (1 .. 11) := (
			 1	=> (x4, y5, x3, y6),
			 2	=> (x3, y6, x1, y6),
			 3	=> (x1, y6, x0, y5),
			 4	=> (x0, y5, x0, y4),
			 5	=> (x0, y4, x1, y3),
			 6	=> (x1, y3, x3, y3),
			 7	=> (x3, y3, x4, y2),
			 8	=> (x4, y2, x4, y1),
			 9  => (x4, y1, x3, y0),
			 10	=> (x3, y0, x1, y0),
			 11	=> (x1, y0, x0, y1)
			);

		capital_t : constant type_character (1 .. 2) := (
			 1	=> (x2, y6, x2, y0),
			 2	=> (x0, y6, x4, y6)
			);

		capital_u : constant type_character (1 .. 5) := (
			 1	=> (x0, y6, x0, y1),
			 2	=> (x0, y1, x1, y0),
			 3	=> (x1, y0, x3, y0),
			 4	=> (x3, y0, x4, y1),
			 5	=> (x4, y1, x4, y6)
			);

		capital_v : constant type_character (1 .. 2) := (
			 1	=> (x0, y6, x2, y0),
			 2	=> (x2, y0, x4, y6)
			);

		capital_w : constant type_character (1 .. 4) := (
			 1	=> (x0, y6, x0, y0),
			 2	=> (x0, y0, x2, y3),
			 3	=> (x2, y3, x4, y0),
			 4	=> (x4, y0, x4, y6)
			);

		capital_x : constant type_character (1 .. 2) := (
			 1	=> (x0, y6, x4, y0),
			 2	=> (x4, y6, x0, y0)
			);

		capital_y : constant type_character (1 .. 3) := (
			 1	=> (x0, y6, x2, y3),
			 2	=> (x4, y6, x2, y3),
			 3	=> (x2, x3, x2, y0)
			);

		capital_z : constant type_character (1 .. 3) := (
			 1	=> (x0, y6, x4, y6),
			 2	=> (x4, y6, x0, y0),
			 3	=> (x0, y0, x4, y0)
			);

		
		-- LOWER CASE LETTERS:
		small_a : constant type_character (1 .. 7) := (
			1	=> (x4, y2, x1, y2),
			2	=> (x1, y2, x0, y1),
			3	=> (x0, y1, x1, y0),
			4	=> (x1, y0, x4, y0),
			5	=> (x4, y0, x4, y3),
			6	=> (x4, y3, x3, y4),
			7	=> (x3, y4, x1, y4)
			);

		small_b : constant type_character (1 .. 6) := (
			 1	=> (x0, y6, x0, y0),
			 2	=> (x0, y4, x3, y4),
			 3	=> (x3, y4, x4, y3),
			 4	=> (x4, y3, x4, y1),
			 5	=> (x4, y1, x3, y0),
			 6	=> (x3, y0, x0, y0)
			);

		small_c : constant type_character (1 .. 5) := (
			 1	=> (x4, y4, x1, y4),
			 2	=> (x1, y4, x0, y3),
			 3	=> (x0, y3, x0, y1),
			 4	=> (x0, y1, x1, y0),
			 5	=> (x1, y0, x4, y0)
			 );

		small_d : constant type_character (1 .. 6) := (
			 1	=> (x4, y4, x1, y4),
			 2	=> (x1, y4, x0, y3),
			 3	=> (x0, y3, x0, y1),
			 4	=> (x0, y1, x1, y0),
			 5	=> (x1, y0, x4, y0),
			 6	=> (x4, y6, x4, y0)
			 );

		small_e : constant type_character (1 .. 8) := (
			 1	=> (x0, y2, x4, y2),
			 2	=> (x4, y2, x4, y3),
			 3	=> (x4, y3, x3, y4),
			 4	=> (x3, y4, x1, y4),
			 5	=> (x1, y4, x0, y3),
			 6	=> (x0, y3, x0, y1),
			 7	=> (x0, y1, x1, y0),
			 8	=> (x1, y0, x3, y0)
			 );

		small_f : constant type_character (1 .. 3) := (
			 1	=> (x3, y6, x2, y5),
			 2	=> (x2, y5, x2, y0),
			 3	=> (x1, y3, x3, y3)
			);
		
		small_g : constant type_character (1 .. 8) := (
			 1	=> (x4, y4, x1, y4),
			 2	=> (x1, y4, x0, y3),
			 3	=> (x0, y3, x0, y1),
			 4	=> (x0, y1, x1, y0),
			 5	=> (x1, y0, x4, y0),
			 6	=> (x4, y4, x4, y7),
			 7	=> (x4, y7, x3, y8),
			 8	=> (x3, y8, x2, y8)
			);

		small_h : constant type_character (1 .. 5) := (
			 1	=> (x0, y6, x0, y0),
			 2	=> (x0, y3, x1, y4),
			 3	=> (x1, y4, x3, y4),
			 4	=> (x3, y4, x4, y3),
			 5	=> (x4, y3, x4, y0)
			);

		small_i : constant type_character (1 .. 4) := (
			 1	=> (x2, y6, x2, y5),
			 2	=> (x1, y3, x2, y3),
			 3	=> (x2, y3, x2, y0),
			 4	=> (x1, y0, x3, y0) 
			);

		small_j : constant type_character (1 .. 4) := (
			 1	=> (x3, y6, x3, y5),
			 2	=> (x3, y3, x3, y7),
			 3	=> (x3, y7, x2, y8),
			 4	=> (x2, y8, x1, y8) 
			);
		
		small_k : constant type_character (1 .. 3) := (
			 1	=> (x0, y6, x0, y0),
			 2	=> (x0, y2, x3, y4),
			 3	=> (x0, y2, x3, y0)
			);

		small_l : constant type_character (1 .. 3) := (
			 1	=> (x1, y6, x2, y6),
			 2	=> (x2, y6, x2, y0),
			 3	=> (x1, y0, x3, y0) 
			);

		small_m : constant type_character (1 .. 7) := (
			 1	=> (x0, y0, x0, y4),
			 2	=> (x0, y4, x1, y4),
			 3	=> (x1, y4, x2, y3),
			 4	=> (x2, y3, x2, y0),
			 5	=> (x2, y3, x3, y4),
			 6	=> (x3, y4, x4, y3),
			 7	=> (x4, y3, x4, y0) 
			);

		small_n : constant type_character (1 .. 4) := (
			 1	=> (x0, y0, x0, y4),
			 2	=> (x0, y4, x3, y4),
			 3	=> (x3, y4, x4, y3),
			 4	=> (x4, y3, x4, y0)
			);

		small_o : constant type_character (1 .. 8) := (
			 1	=> (x1, y0, x3, y0),
			 2	=> (x3, y0, x4, y1),
			 3	=> (x4, y1, x4, y3),
			 4	=> (x4, y3, x3, y4),
			 5	=> (x3, y4, x1, y4),
			 6	=> (x1, y4, x0, y3),
			 7	=> (x0, y3, x0, y1),
			 8	=> (x0, y1, x1, y0)
			);

		small_p : constant type_character (1 .. 6) := (
			 1	=> (x0, y4, x0, y8),
			 2	=> (x0, y4, x3, y4),
			 3	=> (x3, y4, x4, y3),
			 4	=> (x4, y3, x4, y1),
			 5	=> (x4, y1, x3, y0),
			 6	=> (x3, y0, x0, y0)
			);

		small_q : constant type_character (1 .. 6) := (
			 1	=> (x4, y4, x1, y4),
			 2	=> (x1, y4, x0, y3),
			 3	=> (x0, y3, x0, y1),
			 4	=> (x0, y1, x1, y0),
			 5	=> (x1, y0, x4, y0),
			 6	=> (x4, y4, x4, y8)
			);

		small_r : constant type_character (1 .. 3) := (
			 1	=> (x0, y4, x0, y0),
			 2	=> (x0, y2, x2, y4),
			 3	=> (x2, y4, x3, y4)
			 );
		
		small_s : constant type_character (1 .. 7) := (
			 1	=> (x4, y4, x1, y4),
			 2	=> (x1, y4, x0, y3),
			 3	=> (x0, y3, x1, y2),
			 4	=> (x1, y2, x3, y2),
			 5	=> (x3, y2, x4, y1),
			 6	=> (x4, y1, x3, y0),
			 7	=> (x3, y0, x0, y0)
			);

		small_t : constant type_character (1 .. 3) := (
			 1	=> (x2, y5, x2, y1),
			 2	=> (x2, y1, x3, y0),
			 3	=> (x1, y4, x3, y4)
			);

		small_u : constant type_character (1 .. 4) := (
			 1	=> (x0, y4, x0, y1),
			 2	=> (x0, y1, x1, y0),
			 3	=> (x1, y0, x4, y0),
			 4	=> (x4, y0, x4, y4)
			);

		small_v : constant type_character (1 .. 2) := (
			 1	=> (x0, y4, x2, y0),
			 2	=> (x2, y0, x4, y4)
			);

		small_w : constant type_character (1 .. 6) := (
			 1	=> (x0, y4, x0, y1),
			 2	=> (x0, y1, x1, y0),
			 3	=> (x1, y0, x2, y1),
			 4	=> (x2, y1, x3, y0),
			 5	=> (x3, y0, x4, y1),
			 6	=> (x4, y1, x4, y4)
			);

		small_x : constant type_character (1 .. 2) := (
			 1	=> (x0, y4, x4, y0),
			 2	=> (x4, y4, x0, y0)
			);

		small_y : constant type_character (1 .. 6) := (
			 1	=> (x0, y4, x0, y1),
			 2	=> (x0, y1, x1, y0),
			 3	=> (x1, y0, x4, y0),
			 4	=> (x4, y4, x4, y7),
			 5	=> (x4, y7, x3, y8),
			 6	=> (x3, y8, x2, y8)
			 );
			 
		small_z : constant type_character (1 .. 3) := (
			 1	=> (x0, y4, x4, y4),
			 2	=> (x4, y4, x0, y0),
			 3	=> (x0, y0, x4, y0)
			);

		-- DIGITS
		digit_0 : constant type_character (1 .. 9) := (
			 1	=> (x0, y5, x0, y1),
			 2	=> (x0, y1, x1, y0),
			 3	=> (x1, y0, x3, y0),
			 4	=> (x3, y0, x4, y1),
			 5	=> (x4, y1, x4, y5),
			 6	=> (x4, y5, x3, y6),
			 7	=> (x3, y6, x1, y6),
			 8	=> (x1, y6, x0, y5),
			 9	=> (x0, y1, x4, y5)
			);
		
		digit_1 : constant type_character (1 .. 3) := (
			 1	=> (x0, y4, x2, y6),
			 2	=> (x2, y6, x2, y0),
			 3	=> (x0, y0, x4, y0)
			);

		digit_2 : constant type_character (1 .. 6) := (
			 1	=> (x0, y5, x1, y6),
			 2	=> (x1, y6, x3, y6),
			 3	=> (x3, y6, x4, y5),
			 4	=> (x4, y5, x4, y4),
			 5	=> (x4, y4, x0, y0),
			 6	=> (x0, y0, x4, y0)
			);

		digit_3 : constant type_character (1 ..11) := (
			 1	=> (x0, y5, x1, y6),
			 2	=> (x1, y6, x3, y6),
			 3	=> (x3, y6, x4, y5),
			 4	=> (x4, y5, x4, y4),
			 5	=> (x4, y4, x3, y3),
			 6	=> (x3, y3, x2, y3),
			 7	=> (x3, y3, x4, y2),
			 8	=> (x4, y2, x4, y1),
			 9	=> (x4, y1, x3, y0),
			 10	=> (x3, y0, x1, y0),
			 11	=> (x1, y0, x0, y1)
			);

		digit_4 : constant type_character (1 .. 3) := (
			 1	=> (x3, y6, x3, y0),
			 2	=> (x3, y6, x0, y3),
			 3	=> (x0, y3, x4, y3)
			 );
		
		digit_5 : constant type_character (1 .. 9) := (
			 1	=> (x0, y6, x0, y3),
			 2	=> (x0, y3, x2, y4),
			 3	=> (x2, y4, x3, y4),
			 4	=> (x3, y4, x4, y3),
			 5	=> (x4, y3, x4, y1),
			 6	=> (x4, y1, x3, y0),
			 7	=> (x3, y0, x1, y0),
			 8	=> (x1, y0, x0, y1),
			 9	=> (x0, y6, x4, y6)
			);

		digit_6 : constant type_character (1 ..10) := (
			 1	=> (x4, y5, x3, y6),
			 2	=> (x3, y6, x1, y6),
			 3	=> (x1, y6, x0, y5),
			 4	=> (x0, y5, x0, y1),
			 5	=> (x0, y1, x1, y0),
			 6	=> (x1, y0, x3, y0),
			 7	=> (x3, y0, x4, y1),
			 8	=> (x4, y1, x4, y2),
			 9	=> (x4, y2, x3, y3),
			 10	=> (x3, y3, x0, y3)
			);

		digit_7 : constant type_character (1 .. 3) := (
			 1	=> (x0, y6, x4, y6),
			 2	=> (x4, y6, x1, y0),
			 3	=> (x1, y3, x4, y3)
			);

		digit_8 : constant type_character (1 .. 15) := (
			 1	=> (x1, y0, x3, y0),
			 2	=> (x3, y0, x4, y1),
			 3	=> (x4, y1, x4, y2),
			 4	=> (x4, y2, x3, y3),
			 5	=> (x3, y3, x1, y3),
			 6	=> (x1, y3, x0, y2),
			 7	=> (x0, y2, x0, y1),
			 8	=> (x0, y1, x1, y0),
			 9	=> (x1, y3, x0, y4),
			 10 => (x0, y4, x0, y5),
			 11	=> (x0, y5, x1, y6),
			 12	=> (x1, y6, x3, y6),
			 13	=> (x3, y6, x4, y5),
			 14	=> (x4, y5, x4, y4),
			 15	=> (x4, y4, x3, y3)
			);

		digit_9 : constant type_character (1 ..10) := (
			 1	=> (x4, y5, x3, y6),
			 2	=> (x3, y6, x1, y6),
			 3	=> (x1, y6, x0, y5),
			 4	=> (x0, y5, x0, y4),
			 5	=> (x0, y4, x1, y3),
			 6	=> (x1, y3, x4, y3),
			 7	=> (x4, y5, x4, y1),
			 8	=> (x4, y1, x3, y0),
			 9	=> (x3, y0, x1, y0),
			 10	=> (x1, y0, x0, y1)
			 );

		-- SPECIAL CHARACTERS
		special_dash : constant type_character (1 .. 1) := (
			 1	=> (x1, y3, x3, y3)
			 );
		
		special_underline : constant type_character (1 .. 1) := (
			 1	=> (x0, y0, x4, y0)
			 );

		
		package pac_vector_text_lines is new doubly_linked_lists (type_vector_text_line);

		-- Converts a character to a list of lines:
		function to_lines (char : in type_character) return pac_vector_text_lines.list;

	

		
		function vectorize (
			content		: in type_text_content.bounded_string;
			size		: in type_text_size;
			rotation	: in type_rotation;
			position	: in type_point;
			mirror		: in type_vector_text_mirrored := vector_text_mirror_default;
			line_width	: in type_distance_positive;
			alignment	: in type_text_alignment := vector_text_alignment_default)
			return pac_vector_text_lines.list;

		


	end generic_pac_text;

end et_text;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
