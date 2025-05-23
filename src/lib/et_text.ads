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

with ada.strings;				use ada.strings;
with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_ordered_maps;

with et_geometry_1;
with et_geometry_1.et_polygons;
with et_geometry_1.et_polygons.offsetting;
with et_geometry_1.et_polygons.union;

with et_geometry_2a;
with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;
with et_logical_pixels;			use et_logical_pixels;
with et_mirroring;				use et_mirroring;
with et_alignment;				use et_alignment;
with et_object_status;			use et_object_status;


package et_text is

	
-- CONTENT
	
	-- A text may have up to 200 characters which seems sufficient for now.
	text_length_max : constant natural := 200;
	package pac_text_content is new generic_bounded_length (text_length_max);

	function to_string (text_content : in pac_text_content.bounded_string) return string;
	function to_content (content : in string) return pac_text_content.bounded_string;

	empty_text_content : constant pac_text_content.bounded_string :=
		pac_text_content.to_bounded_string ("");
	
	function is_empty (content : in pac_text_content.bounded_string) return boolean;
	
	valid_characters : character_set := to_set 
		(ranges => (('a','z'),('A','Z'),('0','9'))) or to_set ("_-+/: "); 


	-- Tests if the given text contains only valid characters as specified
	-- by given character set. Returns false if invalid character found.
	function characters_valid (
		content		: in pac_text_content.bounded_string;
		characters	: in character_set := valid_characters) 
		return boolean;

	replace_by_default : constant character := '_';

	-- Replaces invalid characters in content by character given in replace_by:
	procedure replace_invalid_characters (
		content		: in out pac_text_content.bounded_string;
		replace_by	: in character := replace_by_default;
		characters	: in character_set := valid_characters);

	
	procedure check_text_content_length (content : in string);
	-- Tests if the content is not longer than allowed.


	


	type type_rotation_documentation is (HORIZONTAL, VERTICAL);

	-- Toggles between HORIZONTAL and VERTICAL:
	procedure toggle_rotation (rotation : in out type_rotation_documentation);



	
-- VECTORIZED TEXT

	-- In vector text a single character consists of straight lines.
	-- CS: It seems sufficient to have at most 20 lines per character.
	type type_segment_id is range 1 .. 20; 

	vector_text_alignment_default : constant type_text_alignment := (ALIGN_LEFT, ALIGN_BOTTOM);


	-- The border around a character is a polygon with a limited
	-- number of vertices:
	subtype type_border_vertex_id is natural range 1 .. 10; 
	

	
	
-- GENERIC PART
	
	generic
		with package pac_geometry is new et_geometry_2a (<>);
		with package pac_polygons   is new pac_geometry.pac_geometry_1.et_polygons;
		with package pac_offsetting is new pac_polygons.offsetting;
		
		size_min		: pac_geometry.type_distance_positive;
		size_max		: pac_geometry.type_distance_positive;
		size_default	: pac_geometry.type_distance_positive;		

		-- These parameters are relevant for vector text:
		line_width_min		: pac_geometry.type_distance_positive;
		line_width_max		: pac_geometry.type_distance_positive;
		line_width_default	: pac_geometry.type_distance_positive;
		
	package generic_pac_text is
		use pac_geometry;

		use pac_geometry_1;
		-- NOTE: This use clause does not work properly. 
		-- For some reason the package name must be explicitely provided
		-- for stuff that stems from pac_geometry_1.
		-- Otherwise the linker reports lots of "undefined references" ...

		
		subtype type_text_size is pac_geometry.type_distance_positive 
			range size_min .. size_max; -- in millimeters



		
		
		subtype type_text_line_width is pac_geometry.type_distance_positive
			range line_width_min .. line_width_max;

		
		-- Converts given distance to type_text_size. Raises error on excessive text size.
		function to_text_size (size : in pac_geometry.type_distance) return type_text_size;

		

		
		
		procedure validate_text_size (size : in pac_geometry.type_distance);
		-- Checks whether given text size is in range of type_text_size.

		
		-- Checks whether given line width is in range of type_text_line_width
		procedure validate_text_line_width (width : in pac_geometry.type_distance);


		-- This is the root type of all text types:
		type type_text is abstract tagged record
			size		: type_text_size := size_default;
			alignment	: type_text_alignment;
			status		: et_object_status.type_object_status;
		end record;


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


		-- This basic text type is intended for all texts which are
		-- somehow frabrication relevant. Such texts are so called
		-- vector-texts. They consist of lines with a certain width.
		type type_text_fab is new type_text with record
			position	: pac_geometry.type_position; -- x/y/rotation
			line_width	: type_text_line_width := type_text_line_width'first; -- CS rename to linewidth
		end record;


		
		
		-- Returns the x/y coordinates and the rotation of a text:
		function get_position (text : in type_text_fab)
			return pac_geometry.type_position;
		
		-- Returns the x/y coordinates of a text:
		function get_place (text : in type_text_fab)
			return type_vector_model;
		
		-- Returns the rotation of a text:
		function get_rotation (text : in type_text_fab)
			return type_rotation;


		-- Returns the properties of the given text:
		function to_string (
			text : in type_text_fab)
			return string;


		
		-- Mirrors a text along the given axis:
		procedure mirror_text (
			text	: in out type_text_fab;
			axis	: in type_mirror := MIRROR_ALONG_Y_AXIS);

		-- Rotates a text by the given angle about the origin:
		procedure rotate_text_by (
			text	: in out type_text_fab;
			angle	: in type_rotation);

		-- Moves a text by the given offset:
		procedure move_text_by (
			text	: in out type_text_fab;
			offset	: in type_vector_model);

		-- Moves a text to the given point:
		procedure move_text_to (
			text	: in out type_text_fab;
			point	: in type_vector_model);

		
		-- Returns the properties of the given text in a long single string.	
		function text_properties (
			text : in type_text_fab)
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
		function snap (rotation : in pac_geometry.type_rotation) return type_rotation_documentation;

		
		-- Converts a string like "0.0" or "90.0" to HORIZONTAL or VERTICAL.
		function to_rotation_doc (rotation : in string) return type_rotation_documentation;		


		
	-- VECTORIZED TEXT
		
-- 		type type_line_with_to_size_ratio is range 1 .. 50; -- in percent
-- 		line_width_to_size_ratio_default : constant type_line_with_to_size_ratio := 15;

		subtype type_character_height is type_float range -0.4 .. 1.0;
		subtype type_character_width  is type_float range  0.0 .. 0.7;

		character_width : constant type_character_height := 0.7;
	


		

		-- To model a vectorized default character this stuff is required.
		-- A single line segment of a character is defined as:
		type type_character_segment is record
			start_x	: type_character_width;
			start_y : type_character_height;
			end_x	: type_character_width;
			end_y	: type_character_height;
		end record;

		type type_character_segments is array (type_segment_id range <>) 
			of type_character_segment;

			
			

		-- This is a vectorized default character type:
		type type_character (
			segment_ct			: type_segment_id;
			border_vertex_ct	: type_border_vertex_id) 
		is record
			-- The line segments that model the actual character:
			segments	: type_character_segments (1 .. segment_ct);

			-- The border around the character is described by location vectors.
			-- It is a polygon in CCW winding that models the basic outline
			-- of the character:
			border		: type_vector_array (1 .. border_vertex_ct);
		end record;


		

		-- These are common columns and rows where the segments
		-- of a default character start, end or meet each other.
		-- The define the way such a character looks like.

		-- columns:
		x0 : constant type_character_width := 0.0;
		x1 : constant type_character_width := 0.2;
		x2 : constant type_character_width := 0.35;
		x3 : constant type_character_width := 0.5;
		x4 : constant type_character_width := 0.7;

		-- rows:
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
		
		capital_a : constant type_character := (
			segment_ct => 5,												   
			segments => (
				1	=> (x0, y0, x0, y4), -- |
				2	=> (x0, y4, x2, y6), -- /
				3	=> (x2, y6, x4, y4), -- \
				4	=> (x4, y4, x4, y0), -- |
				5	=> (x0, y3, x4, y3)), -- -

			border_vertex_ct => 4,
			border => ( -- describes a polygon in CCW winding !
				1	=> set (x0, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y6),
				4	=> set (x0, y6))
				-- CS needs refinement
			);

		
		capital_b : constant type_character := (
			segment_ct => 10,
			segments => (
				1	=> (x0, y0, x0, y6), -- |
				2	=> (x0, y6, x3, y6), -- -
				3	=> (x3, y6, x4, y5), -- \
				4	=> (x4, y5, x4, y4), -- |
				5	=> (x4, y4, x3, y3), -- /
				6	=> (x0, y3, x3, y3), -- -
				7	=> (x3, y3, x4, y2), -- \
				8	=> (x4, y2, x4, y1), -- |
				9	=> (x4, y1, x3, y0), -- /
				10	=> (x3, y0, x0, y0)), -- _

			border_vertex_ct => 9,
			border => ( -- describes a polygon in CCW winding !
				1	=> set (x0, y0),
				2	=> set (x3, y0),
				3	=> set (x4, y1),
				4	=> set (x4, y2),
				5	=> set (x3, y3),
				6	=> set (x4, y4),
				7	=> set (x4, y5),
				8	=> set (x3, y6),
				9	=> set (x0, y6))
			);

		
		capital_c : constant type_character := (
			segment_ct => 7,
			segments => (									
			 1	=> (x4, y5, x3, y6),
			 2	=> (x3, y6, x1, y6),
			 3	=> (x1, y6, x0, y5),
			 4	=> (x0, y5, x0, y1),
			 5	=> (x0, y1, x1, y0),
			 6	=> (x1, y0, x3, y0),
			 7	=> (x3, y0, x4, y1)),

			border_vertex_ct => 6,
			border => (
				1	=> set (x0, y0),
				2	=> set (x3, y0),
				3	=> set (x4, y1),
				4	=> set (x4, y5),
				5	=> set (x3, y6),
				6	=> set (x0, y6))
		  );

		
		capital_d : constant type_character := (
			segment_ct => 6,
			segments => (
				1	=> (x0, y6, x0, y0),
				2	=> (x0, y6, x3, y6),
				3	=> (x3, y6, x4, y5),
				4	=> (x4, y5, x4, y1),
				5	=> (x4, y1, x3, y0),
				6	=> (x3, y0, x0, y0)),

			border_vertex_ct => 6,
			border => (
				1	=> set (x0, y0),
				2	=> set (x3, y0),
				3	=> set (x4, y1),
				4	=> set (x4, y5),
				5	=> set (x3, y6),
				6	=> set (x0, y6))
		  );
			

		capital_e : constant type_character := (
			segment_ct => 4,
			segments => (
				1	=> (x0, y6, x0, y0),
				2	=> (x0, y6, x4, y6),
				3	=> (x0, y3, x3, y3),
				4	=> (x0, y0, x4, y0)),
			
			border_vertex_ct => 4,
			border => (
				1	=> set (x0, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y6),
				4	=> set (x0, y6))
		  );

		
		capital_f : constant type_character := (
			segment_ct => 3,
			segments => (
				1	=> (x0, y6, x0, y0),
				2	=> (x0, y6, x4, y6),
				3	=> (x0, y3, x3, y3)),

			border_vertex_ct => 4,
			border => (
				1	=> set (x0, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y6),
				4	=> set (x0, y6))
		  );
											   
		
		capital_g : constant type_character := (
			segment_ct => 9,
			segments => (
				1	=> (x4, y5, x3, y6),
				2	=> (x3, y6, x1, y6),
				3	=> (x1, y6, x0, y5),
				4	=> (x0, y5, x0, y1),
				5	=> (x0, y1, x1, y0),
				6	=> (x1, y0, x3, y0),
				7	=> (x3, y0, x4, y1),
				8	=> (x4, y1, x4, y3),
				9	=> (x4, y3, x2, y3)),

			border_vertex_ct => 6,
			border => (
				1	=> set (x0, y0),
				2	=> set (x3, y0),
				3	=> set (x4, y1),
				4	=> set (x4, y5),
				5	=> set (x3, y6),
				6	=> set (x0, y6))
			);

		
		capital_h : constant type_character := (
			segment_ct => 3,
			segments => (
				1	=> (x0, y6, x0, y0),
				2	=> (x4, y6, x4, y0),
				3	=> (x0, y3, x4, y3)),

			border_vertex_ct => 4,
			border => (
				1	=> set (x0, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y6),
				4	=> set (x0, y6))
			);

		
		capital_i : constant type_character := (
			segment_ct => 3,
			segments => (
				1	=> (x2, y6, x2, y0),
				2	=> (x1, y6, x3, y6),
				3	=> (x1, y0, x3, y0)),

			border_vertex_ct => 4,
			border => (
				1	=> set (x1, y0),
				2	=> set (x3, y0),
				3	=> set (x3, y6),
				4	=> set (x1, y6))
			);


		capital_j : constant type_character := (
			segment_ct => 5,
			segments => (
				1	=> (x2, y6, x4, y6),
				2	=> (x3, y6, x3, y1),
				3	=> (x3, y1, x2, y0),
				4	=> (x2, y0, x1, y0),
				5	=> (x1, y0, x0, y1)),

			border_vertex_ct => 4,
			border => (
				1	=> set (x0, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y6),
				4	=> set (x0, y6))
			);

		
		capital_k : constant type_character := (
			segment_ct => 3,
			segments => (									   
				1	=> (x0, y6, x0, y0),
				2	=> (x0, y3, x4, y6),
				3	=> (x0, y3, x4, y0)),

			border_vertex_ct => 4,
			border => (
				1	=> set (x0, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y6),
				4	=> set (x0, y6))
		  );
			

		capital_l : constant type_character := (
			segment_ct => 2,
			segments => (									   
				1	=> (x0, y6, x0, y0),
				2	=> (x0, y0, x4, y0)),

			border_vertex_ct => 4,
			border => (
				1	=> set (x0, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y6),
				4	=> set (x0, y6))
		  );


		capital_m : constant type_character := (
			segment_ct => 4,
			segments => (									   
				1	=> (x0, y6, x0, y0),
				2	=> (x0, y6, x2, y3),
				3	=> (x2, y3, x4, y6),
				4	=> (x4, y6, x4, y0)),
			
			border_vertex_ct => 4,
			border => (
				1	=> set (x0, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y6),
				4	=> set (x0, y6))
		  );


		capital_n : constant type_character := (
			segment_ct => 3,
			segments => (									   
				1	=> (x0, y6, x0, y0),
				2	=> (x0, y6, x4, y0),
				3	=> (x4, y6, x4, y0)),
			
			border_vertex_ct => 4,
			border => (
				1	=> set (x0, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y6),
				4	=> set (x0, y6))
		  );


		capital_o : constant type_character := (
			segment_ct => 8,
			segments => (									   
				1	=> (x0, y5, x0, y1),
				2	=> (x0, y1, x1, y0),
				3	=> (x1, y0, x3, y0),
				4	=> (x3, y0, x4, y1),
				5	=> (x4, y1, x4, y5),
				6	=> (x4, y5, x3, y6),
				7	=> (x3, y6, x1, y6),
				8	=> (x1, y6, x0, y5)),
			
			border_vertex_ct => 8,
			border => (
				1	=> set (x1, y0),
				2	=> set (x3, y0),
				3	=> set (x4, y1),
				4	=> set (x4, y5),
				5	=> set (x3, y6),
				6	=> set (x1, y6),
				7	=> set (x0, y5),
				8	=> set (x0, y1))
		  );

		
		capital_p : constant type_character := (
			segment_ct => 6,
			segments => (									   
				1	=> (x0, y6, x0, y0),
				2	=> (x0, y6, x3, y6),
				3	=> (x3, y6, x4, y5),
				4	=> (x4, y5, x4, y4),
				5	=> (x4, y4, x3, y3),
				6	=> (x3, y3, x0, y3)),

			border_vertex_ct => 5,
			border => (
				1	=> set (x0, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y5),
				4	=> set (x3, y6),
				5	=> set (x0, y6))
		  );
			

		capital_q : constant type_character := (
			segment_ct => 9,
			segments => (									   
				1	=> (x0, y5, x0, y1),
				2	=> (x0, y1, x1, y0),
				3	=> (x1, y0, x3, y0),
				4	=> (x3, y0, x4, y1),
				5	=> (x4, y1, x4, y5),
				6	=> (x4, y5, x3, y6),
				7	=> (x3, y6, x1, y6),
				8	=> (x1, y6, x0, y5),
				9	=> (x3, y1, x4, y0)),

			border_vertex_ct => 7,
			border => (
				1	=> set (x1, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y5),
				4	=> set (x3, y6),
				5	=> set (x1, y6),
				6	=> set (x0, y5),
				7	=> set (x0, y1))
			);
			

		capital_r : constant type_character := (
			segment_ct => 7,
			segments => (									   
				1	=> (x0, y6, x0, y0),
				2	=> (x0, y6, x3, y6),
				3	=> (x3, y6, x4, y5),
				4	=> (x4, y5, x4, y4),
				5	=> (x4, y4, x3, y3),
				6	=> (x3, y3, x0, y3),
				7	=> (x3, y3, x4, y0)),

			border_vertex_ct => 5,
			border => (
				1	=> set (x0, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y5),
				4	=> set (x3, y6),
				5	=> set (x0, y6))
			);
			
		
		capital_s : constant type_character := (
			segment_ct => 11,
			segments => (									   
				1	=> (x4, y5, x3, y6),
				2	=> (x3, y6, x1, y6),
				3	=> (x1, y6, x0, y5),
				4	=> (x0, y5, x0, y4),
				5	=> (x0, y4, x1, y3),
				6	=> (x1, y3, x3, y3),
				7	=> (x3, y3, x4, y2),
				8	=> (x4, y2, x4, y1),
				9 	=> (x4, y1, x3, y0),
				10	=> (x3, y0, x1, y0),
				11	=> (x1, y0, x0, y1)),

			border_vertex_ct => 8,
			border => (
				1	=> set (x1, y0),
				2	=> set (x3, y0),
				3	=> set (x4, y1),
				4	=> set (x4, y5),
				5	=> set (x3, y6),
				6	=> set (x1, y6),
				7	=> set (x0, y5),
				8	=> set (x0, y1))
		  );

		
		capital_t : constant type_character := (
			segment_ct => 2,
			segments => (									   
				1	=> (x2, y6, x2, y0),
				2	=> (x0, y6, x4, y6)),

			border_vertex_ct => 4,
			border => (
				1	=> set (x0, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y6),
				4	=> set (x0, y6))
		  );

		
		capital_u : constant type_character := (
			segment_ct => 5,
			segments => (									   
				1	=> (x0, y6, x0, y1),
				2	=> (x0, y1, x1, y0),
				3	=> (x1, y0, x3, y0),
				4	=> (x3, y0, x4, y1),
				5	=> (x4, y1, x4, y6)),

			border_vertex_ct => 7,
			border => (
				1	=> set (x1, y0),
				2	=> set (x3, y0),
				3	=> set (x4, y1),
				4	=> set (x4, y6),
				5	=> set (x0, y6),
				6	=> set (x1, y5),
				7	=> set (x0, y1))
		  );


		capital_v : constant type_character := (
			segment_ct => 2,
			segments => (									   
				1	=> (x0, y6, x2, y0),
				2	=> (x2, y0, x4, y6)),

			border_vertex_ct => 4,
			border => (
				1	=> set (x0, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y6),
				4	=> set (x0, y6))
		  );


		capital_w : constant type_character := (
			segment_ct => 4,
			segments => (									   
				1	=> (x0, y6, x0, y0),
				2	=> (x0, y0, x2, y3),
				3	=> (x2, y3, x4, y0),
				4	=> (x4, y0, x4, y6)),

			border_vertex_ct => 4,
			border => (
				1	=> set (x0, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y6),
				4	=> set (x0, y6))
		  );

		
		capital_x : constant type_character := (
			segment_ct => 2,
			segments => (									   
				1	=> (x0, y6, x4, y0),
				2	=> (x4, y6, x0, y0)),

			border_vertex_ct => 4,
			border => (
				1	=> set (x0, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y6),
				4	=> set (x0, y6))
		  );


		capital_y : constant type_character := (
			segment_ct => 3,
			segments => (									   
				1	=> (x0, y6, x2, y3),
				2	=> (x4, y6, x2, y3),
				3	=> (x2, x3, x2, y0)),
			
			border_vertex_ct => 4,
			border => (
				1	=> set (x0, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y6),
				4	=> set (x0, y6))
		  );


		capital_z : constant type_character := (
			segment_ct => 3,
			segments => (									   
				1	=> (x0, y6, x4, y6),
				2	=> (x4, y6, x0, y0),
				3	=> (x0, y0, x4, y0)),
			
			border_vertex_ct => 4,
			border => (
				1	=> set (x0, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y6),
				4	=> set (x0, y6))
		  );


		
	-- LOWER CASE LETTERS:
		
		small_a : constant type_character := (
			segment_ct	=> 7,
			segments => (									 
				1	=> (x4, y2, x1, y2),
				2	=> (x1, y2, x0, y1),
				3	=> (x0, y1, x1, y0),
				4	=> (x1, y0, x4, y0),
				5	=> (x4, y0, x4, y3),
				6	=> (x4, y3, x3, y4),
				7	=> (x3, y4, x1, y4)),

			border_vertex_ct => 7,
			border => (
				1	=> set (x1, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y3),
				4	=> set (x3, y4),
				5	=> set (x1, y4),
				6	=> set (x0, y3),
				7	=> set (x0, y1))
		  );

			

		small_b : constant type_character := (
			segment_ct => 6,
			segments => (									 
				1	=> (x0, y6, x0, y0),
				2	=> (x0, y4, x3, y4),
				3	=> (x3, y4, x4, y3),
				4	=> (x4, y3, x4, y1),
				5	=> (x4, y1, x3, y0),
				6	=> (x3, y0, x0, y0)),

			border_vertex_ct => 6,
			border => (
				1	=> set (x0, y0),
				2	=> set (x3, y0),
				3	=> set (x4, y1),
				4	=> set (x4, y5),
				5	=> set (x3, y6),
				6	=> set (x0, y6))
		  );


			
		small_c : constant type_character := (
			segment_ct => 5,
			segments => (									 
				1	=> (x4, y4, x1, y4),
				2	=> (x1, y4, x0, y3),
				3	=> (x0, y3, x0, y1),
				4	=> (x0, y1, x1, y0),
				5	=> (x1, y0, x4, y0)),

			border_vertex_ct => 8,
			border => (
				1	=> set (x1, y0),
				2	=> set (x3, y0),
				3	=> set (x4, y1),
				4	=> set (x4, y3),
				5	=> set (x3, y4),
				6	=> set (x1, y4),
				7	=> set (x0, y3),
				8	=> set (x0, y1))
		  );
			

		small_d : constant type_character := (
			segment_ct => 6,
			segments => (									 
				1	=> (x4, y4, x1, y4),
				2	=> (x1, y4, x0, y3),
				3	=> (x0, y3, x0, y1),
				4	=> (x0, y1, x1, y0),
				5	=> (x1, y0, x4, y0),
				6	=> (x4, y6, x4, y0)),

			border_vertex_ct => 6,
			border => (
				1	=> set (x0, y0),
				2	=> set (x3, y0),
				3	=> set (x4, y1),
				4	=> set (x4, y5),
				5	=> set (x3, y6),
				6	=> set (x0, y6))
		  );


		small_e : constant type_character := (
			segment_ct => 8,
			segments => (									 
				1	=> (x0, y2, x4, y2),
				2	=> (x4, y2, x4, y3),
				3	=> (x4, y3, x3, y4),
				4	=> (x3, y4, x1, y4),
				5	=> (x1, y4, x0, y3),
				6	=> (x0, y3, x0, y1),
				7	=> (x0, y1, x1, y0),
				8	=> (x1, y0, x3, y0)),
			
			border_vertex_ct => 8,
			border => (
				1	=> set (x1, y0),
				2	=> set (x3, y0),
				3	=> set (x4, y1),
				4	=> set (x4, y3),
				5	=> set (x3, y4),
				6	=> set (x1, y4),
				7	=> set (x0, y3),
				8	=> set (x0, y1))
		  );


		small_f : constant type_character := (
			segment_ct => 3,
			segments => (								 
				1	=> (x3, y6, x2, y5),
				2	=> (x2, y5, x2, y0),
				3	=> (x1, y3, x3, y3)),
			
			border_vertex_ct => 4,
			border => (
				1	=> set (x1, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y6),
				4	=> set (x1, y6))
		  );

		
		small_g : constant type_character := (
			segment_ct => 8,
			segments => (								 
				1	=> (x4, y4, x1, y4),
				2	=> (x1, y4, x0, y3),
				3	=> (x0, y3, x0, y1),
				4	=> (x0, y1, x1, y0),
				5	=> (x1, y0, x4, y0),
				6	=> (x4, y4, x4, y7),
				7	=> (x4, y7, x3, y8),
				8	=> (x3, y8, x2, y8)),
			
			border_vertex_ct => 8,
			border => (
				1	=> set (x0, y7),
				2	=> set (x1, y8),
				3	=> set (x3, y8),
				4	=> set (x4, y7),
				5	=> set (x4, y4),
				6	=> set (x1, y4),
				7	=> set (x0, y3),
				8	=> set (x0, y0))
		  );

		
		small_h : constant type_character := (
			segment_ct => 5,
			segments => (								 
				1	=> (x0, y6, x0, y0),
				2	=> (x0, y3, x1, y4),
				3	=> (x1, y4, x3, y4),
				4	=> (x3, y4, x4, y3),
				5	=> (x4, y3, x4, y0)),

			border_vertex_ct => 5,
			border => (
				1	=> set (x0, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y5),
				4	=> set (x3, y6),
				5	=> set (x0, y6))
		  );

		
		small_i : constant type_character := (
			segment_ct => 4,
			segments => (								 
				1	=> (x2, y6, x2, y5),
				2	=> (x1, y3, x2, y3),
				3	=> (x2, y3, x2, y0),
				4	=> (x1, y0, x3, y0)),

			border_vertex_ct => 4,
			border => (
				1	=> set (x1, y0),
				2	=> set (x3, y0),
				3	=> set (x3, y6),
				4	=> set (x1, y6))
			);

		
		small_j : constant type_character := (
			segment_ct => 4,
			segments => (								 
				1	=> (x3, y6, x3, y5),
				2	=> (x3, y3, x3, y7),
				3	=> (x3, y7, x2, y8),
				4	=> (x2, y8, x1, y8)),
			
			border_vertex_ct => 8,
			border => (
				1	=> set (x0, y7),
				2	=> set (x1, y8),
				3	=> set (x3, y8),
				4	=> set (x4, y7),
				5	=> set (x4, y5),
				6	=> set (x3, y6),
				7	=> set (x1, y6),
				8	=> set (x0, y5))
			);

			
		small_k : constant type_character := (
			segment_ct => 3,
			segments => (
				1	=> (x0, y6, x0, y0),
				2	=> (x0, y2, x3, y4),
				3	=> (x0, y2, x3, y0)),

			border_vertex_ct => 5,
			border => (
				1	=> set (x0, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y5),
				4	=> set (x3, y6),
				5	=> set (x0, y6))
			);
			

		small_l : constant type_character := (
			segment_ct => 3,
			segments => (
				1	=> (x1, y6, x2, y6),
				2	=> (x2, y6, x2, y0),
				3	=> (x1, y0, x3, y0)),

			border_vertex_ct => 4,
			border => (
				1	=> set (x1, y0),
				2	=> set (x3, y0),
				3	=> set (x3, y6),
				4	=> set (x1, y6))
			);

		
		small_m : constant type_character := (
			segment_ct => 7,
			segments => (
				1	=> (x0, y0, x0, y4),
				2	=> (x0, y4, x1, y4),
				3	=> (x1, y4, x2, y3),
				4	=> (x2, y3, x2, y0),
				5	=> (x2, y3, x3, y4),
				6	=> (x3, y4, x4, y3),
				7	=> (x4, y3, x4, y0)),
			
			border_vertex_ct => 5,
			border => (
				1	=> set (x0, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y3),
				4	=> set (x3, y4),
				5	=> set (x0, y4))				
			);

			
		small_n : constant type_character := (
			segment_ct => 4,
			segments => (
				1	=> (x0, y0, x0, y4),
				2	=> (x0, y4, x3, y4),
				3	=> (x3, y4, x4, y3),
				4	=> (x4, y3, x4, y0)),
			
			border_vertex_ct => 5,
			border => (
				1	=> set (x0, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y3),
				4	=> set (x3, y4),
				5	=> set (x0, y4))				
			);

		
		small_o : constant type_character := (
			segment_ct => 8,
			segments => (
				1	=> (x1, y0, x3, y0),
				2	=> (x3, y0, x4, y1),
				3	=> (x4, y1, x4, y3),
				4	=> (x4, y3, x3, y4),
				5	=> (x3, y4, x1, y4),
				6	=> (x1, y4, x0, y3),
				7	=> (x0, y3, x0, y1),
				8	=> (x0, y1, x1, y0)),
			
			border_vertex_ct => 8,
			border => (
				1	=> set (x1, y0),
				2	=> set (x3, y0),
				3	=> set (x4, y1),
				4	=> set (x4, y3),
				5	=> set (x3, y4),
				6	=> set (x1, y4),
				7	=> set (x0, y3),
				8	=> set (x0, y1))
		  );

		
		small_p : constant type_character := (
			segment_ct => 6,
			segments => (
				1	=> (x0, y4, x0, y8),
				2	=> (x0, y4, x3, y4),
				3	=> (x3, y4, x4, y3),
				4	=> (x4, y3, x4, y1),
				5	=> (x4, y1, x3, y0),
				6	=> (x3, y0, x0, y0)),

			border_vertex_ct => 6,
			border => (
				1	=> set (x0, y8),
				2	=> set (x3, y8),
				3	=> set (x4, y7),
				4	=> set (x4, y3),
				5	=> set (x3, y4),
				6	=> set (x0, y4))
		  );
			

		small_q : constant type_character := (
			segment_ct => 6,
			segments => (
				1	=> (x4, y4, x1, y4),
				2	=> (x1, y4, x0, y3),
				3	=> (x0, y3, x0, y1),
				4	=> (x0, y1, x1, y0),
				5	=> (x1, y0, x4, y0),
				6	=> (x4, y4, x4, y8)),
			
			border_vertex_ct => 7,
			border => (
				1	=> set (x0, y7),
				2	=> set (x1, y8),
				3	=> set (x4, y8),
				4	=> set (x4, y3),
				5	=> set (x3, y4),
				6	=> set (x1, y4),
				7	=> set (x0, y3))
		  );

			
		small_r : constant type_character := (
			segment_ct => 3,
			segments => (
				1	=> (x0, y4, x0, y0),
				2	=> (x0, y2, x2, y4),
				3	=> (x2, y4, x3, y4)),

			border_vertex_ct => 5,
			border => (
				1	=> set (x0, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y3),
				4	=> set (x3, y4),
				5	=> set (x0, y4))				
			);

		
		small_s : constant type_character := (
			segment_ct => 7,
			segments => (
				1	=> (x4, y4, x1, y4),
				2	=> (x1, y4, x0, y3),
				3	=> (x0, y3, x1, y2),
				4	=> (x1, y2, x3, y2),
				5	=> (x3, y2, x4, y1),
				6	=> (x4, y1, x3, y0),
				7	=> (x3, y0, x0, y0)),

			border_vertex_ct => 7,
			border => (
				1	=> set (x0, y0),
				2	=> set (x3, y0),
				3	=> set (x4, y1),
				4	=> set (x4, y4),
				5	=> set (x1, y4),
				6	=> set (x0, y3),
				7	=> set (x0, y1))
		  );


		small_t : constant type_character := (
			segment_ct => 3,
			segments => (
				1	=> (x2, y5, x2, y1),
				2	=> (x2, y1, x3, y0),
				3	=> (x1, y4, x3, y4)),

			border_vertex_ct => 5,
			border => (
				1	=> set (x1, y0),
				2	=> set (x3, y0),
				3	=> set (x4, y1),
				4	=> set (x4, y5),
				5	=> set (x1, y5))
			);
			

		small_u : constant type_character := (
			segment_ct => 4,
			segments => (
				1	=> (x0, y4, x0, y1),
				2	=> (x0, y1, x1, y0),
				3	=> (x1, y0, x4, y0),
				4	=> (x4, y0, x4, y4)),
			
			border_vertex_ct => 5,
			border => (
				1	=> set (x0, y1),
				2	=> set (x1, y0),
				3	=> set (x4, y0),
				4	=> set (x4, y4),
				5	=> set (x0, y4))
			);

			
		small_v : constant type_character := (
			segment_ct => 2,
			segments => (
				1	=> (x0, y4, x2, y0),
				2	=> (x2, y0, x4, y4)),

			border_vertex_ct => 4,
			border => (
				1	=> set (x0, y4),
				2	=> set (x1, y0),
				3	=> set (x3, y0),
				4	=> set (x4, y4))
			);
			

		small_w : constant type_character := (
			segment_ct => 6,
			segments => (
				1	=> (x0, y4, x0, y1),
				2	=> (x0, y1, x1, y0),
				3	=> (x1, y0, x2, y1),
				4	=> (x2, y1, x3, y0),
				5	=> (x3, y0, x4, y1),
				6	=> (x4, y1, x4, y4)),
			
			border_vertex_ct => 6,
			border => (
				1	=> set (x0, y1),
				2	=> set (x1, y0),
				3	=> set (x3, y0),
				4	=> set (x4, y1),
				5	=> set (x4, y4),
				6	=> set (x0, y4))
			);

			
		small_x : constant type_character := (
			segment_ct => 2,
			segments => (
				1	=> (x0, y4, x4, y0),
				2	=> (x4, y4, x0, y0)),

			border_vertex_ct => 4,
			border => (
				1	=> set (x0, y4),
				2	=> set (x0, y0),
				3	=> set (x4, y0),
				4	=> set (x4, y4))
			);

			
		small_y : constant type_character := (
			segment_ct => 6,
			segments => (
				1	=> (x0, y4, x0, y1),
				2	=> (x0, y1, x1, y0),
				3	=> (x1, y0, x4, y0),
				4	=> (x4, y4, x4, y7),
				5	=> (x4, y7, x3, y8),
				6	=> (x3, y8, x2, y8)),

			border_vertex_ct => 6,
			border => (
				1	=> set (x0, y7),
				2	=> set (x1, y8),
				3	=> set (x3, y8),
				4	=> set (x4, y7),
				5	=> set (x4, y4),
				6	=> set (x0, y4))
			);

			
		small_z : constant type_character := (
			segment_ct => 3,
			segments => (
				1	=> (x0, y4, x4, y4),
				2	=> (x4, y4, x0, y0),
				3	=> (x0, y0, x4, y0)),

			border_vertex_ct => 4,
			border => (
				1	=> set (x0, y4),
				2	=> set (x0, y0),
				3	=> set (x4, y0),
				4	=> set (x4, y4))
			);


	-- DIGITS
		
		digit_0 : constant type_character := (
			segment_ct => 9,
			segments => (									   
				1	=> (x0, y5, x0, y1),
				2	=> (x0, y1, x1, y0),
				3	=> (x1, y0, x3, y0),
				4	=> (x3, y0, x4, y1),
				5	=> (x4, y1, x4, y5),
				6	=> (x4, y5, x3, y6),
				7	=> (x3, y6, x1, y6),
				8	=> (x1, y6, x0, y5),
				9	=> (x0, y1, x4, y5)),

			border_vertex_ct => 8,
			border => (
				1	=> set (x1, y0),
				2	=> set (x3, y0),
				3	=> set (x4, y1),
				4	=> set (x4, y5),
				5	=> set (x3, y6),
				6	=> set (x1, y6),
				7	=> set (x0, y5),
				8	=> set (x0, y1))
		  );

		
		digit_1 : constant type_character := (
			segment_ct => 3,
			segments => (									   
				1	=> (x0, y4, x2, y6),
				2	=> (x2, y6, x2, y0),
				3	=> (x0, y0, x4, y0)),
			
			border_vertex_ct => 4,
			border => (
				1	=> set (x0, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y6),
				4	=> set (x0, y6))
		  );

		
		digit_2 : constant type_character := (
			segment_ct => 6,
			segments => (									   
				1	=> (x0, y5, x1, y6),
				2	=> (x1, y6, x3, y6),
				3	=> (x3, y6, x4, y5),
				4	=> (x4, y5, x4, y4),
				5	=> (x4, y4, x0, y0),
				6	=> (x0, y0, x4, y0)),
			
			border_vertex_ct => 6,
			border => (
				1	=> set (x0, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y5),
				4	=> set (x3, y6),
				5	=> set (x1, y6),
				6	=> set (x0, y5))
			);

		
		digit_3 : constant type_character := (
			segment_ct => 11,
			segments => (									   
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
				11	=> (x1, y0, x0, y1)),

			border_vertex_ct => 8,
			border => (
				1	=> set (x1, y0),
				2	=> set (x3, y0),
				3	=> set (x4, y1),
				4	=> set (x4, y5),
				5	=> set (x3, y6),
				6	=> set (x1, y6),
				7	=> set (x0, y5),
				8	=> set (x0, y1))
			);
		

		digit_4 : constant type_character := (
			segment_ct => 3,
			segments => (									   
				1	=> (x3, y6, x3, y0),
				2	=> (x3, y6, x0, y3),
				3	=> (x0, y3, x4, y3)),
			
			border_vertex_ct => 4,
			border => (
				1	=> set (x0, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y6),
				4	=> set (x0, y6))
			);

		
		digit_5 : constant type_character := (
			segment_ct => 9,
			segments => (									   
				1	=> (x0, y6, x0, y3),
				2	=> (x0, y3, x2, y4),
				3	=> (x2, y4, x3, y4),
				4	=> (x3, y4, x4, y3),
				5	=> (x4, y3, x4, y1),
				6	=> (x4, y1, x3, y0),
				7	=> (x3, y0, x1, y0),
				8	=> (x1, y0, x0, y1),
				9	=> (x0, y6, x4, y6)),

			border_vertex_ct => 6,
			border => (
				1	=> set (x1, y0),
				2	=> set (x3, y0),
				3	=> set (x4, y1),
				4	=> set (x4, y6),
				5	=> set (x0, y6),
				6	=> set (x0, y1))
		  );


		digit_6 : constant type_character := (
			segment_ct => 10,
			segments => (									   
				1	=> (x4, y5, x3, y6),
				2	=> (x3, y6, x1, y6),
				3	=> (x1, y6, x0, y5),
				4	=> (x0, y5, x0, y1),
				5	=> (x0, y1, x1, y0),
				6	=> (x1, y0, x3, y0),
				7	=> (x3, y0, x4, y1),
				8	=> (x4, y1, x4, y2),
				9	=> (x4, y2, x3, y3),
				10	=> (x3, y3, x0, y3)),

			border_vertex_ct => 8,
			border => (
				1	=> set (x1, y0),
				2	=> set (x3, y0),
				3	=> set (x4, y1),
				4	=> set (x4, y5),
				5	=> set (x3, y6),
				6	=> set (x1, y6),
				7	=> set (x0, y5),
				8	=> set (x0, y1))
		  );

			
		digit_7 : constant type_character := (
			segment_ct => 3,
			segments => (									   
				1	=> (x0, y6, x4, y6),
				2	=> (x4, y6, x1, y0),
				3	=> (x1, y3, x4, y3)),

			border_vertex_ct => 4,
			border => (
				1	=> set (x0, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y6),
				4	=> set (x0, y6))
			);

			
		digit_8 : constant type_character := (
			segment_ct => 15,
			segments => (									   
				1	=> (x1, y0, x3, y0),
				2	=> (x3, y0, x4, y1),
				3	=> (x4, y1, x4, y2),
				4	=> (x4, y2, x3, y3),
				5	=> (x3, y3, x1, y3),
				6	=> (x1, y3, x0, y2),
				7	=> (x0, y2, x0, y1),
				8	=> (x0, y1, x1, y0),
				9	=> (x1, y3, x0, y4),
				10	=> (x0, y4, x0, y5),
				11	=> (x0, y5, x1, y6),
				12	=> (x1, y6, x3, y6),
				13	=> (x3, y6, x4, y5),
				14	=> (x4, y5, x4, y4),
				15	=> (x4, y4, x3, y3)),

			border_vertex_ct => 8,
			border => (
				1	=> set (x1, y0),
				2	=> set (x3, y0),
				3	=> set (x4, y1),
				4	=> set (x4, y5),
				5	=> set (x3, y6),
				6	=> set (x1, y6),
				7	=> set (x0, y5),
				8	=> set (x0, y1))
		  );

			
		digit_9 : constant type_character := (
			segment_ct => 10,
			segments => (									   
				1	=> (x4, y5, x3, y6),
				2	=> (x3, y6, x1, y6),
				3	=> (x1, y6, x0, y5),
				4	=> (x0, y5, x0, y4),
				5	=> (x0, y4, x1, y3),
				6	=> (x1, y3, x4, y3),
				7	=> (x4, y5, x4, y1),
				8	=> (x4, y1, x3, y0),
				9	=> (x3, y0, x1, y0),
				10	=> (x1, y0, x0, y1)),
			
			border_vertex_ct => 8,
			border => (
				1	=> set (x1, y0),
				2	=> set (x3, y0),
				3	=> set (x4, y1),
				4	=> set (x4, y5),
				5	=> set (x3, y6),
				6	=> set (x1, y6),
				7	=> set (x0, y5),
				8	=> set (x0, y1))
			);

		
	-- SPECIAL CHARACTERS
		
		special_plus : constant type_character := (
			segment_ct => 2,
			segments => (									   
				1	=> (x1, y3, x3, y3),
				2	=> (x2, y4, x2, y2)),
						
			border_vertex_ct => 8,
			border => (
				1	=> set (x1, y1),
				2	=> set (x3, y1),
				3	=> set (x4, y2),
				4	=> set (x4, y3),
				5	=> set (x3, y4),
				6	=> set (x1, y4),
				7	=> set (x0, y3),
				8	=> set (x0, y2))
			);

		
		special_dash : constant type_character := (
			segment_ct => 1,
			segments => (									   
				1	=> (x1, y3, x3, y3)),

			border_vertex_ct => 8,
			border => (
				1	=> set (x1, y1),
				2	=> set (x3, y1),
				3	=> set (x4, y2),
				4	=> set (x4, y3),
				5	=> set (x3, y4),
				6	=> set (x1, y4),
				7	=> set (x0, y3),
				8	=> set (x0, y2))
			);
			
		
		special_underline : constant type_character := (
			segment_ct => 1,
			segments => (									   
				1	=> (x0, y0, x4, y0)),
			
			border_vertex_ct => 4,
			border => (
				1	=> set (x0, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y1),
				4	=> set (x0, y1))
			);
			

			
		special_forward_slash : constant type_character := (
			segment_ct => 1,
			segments => (									   
				1	=> (x1, y0, x3, y6)),

			border_vertex_ct => 4,
			border => (
				1	=> set (x0, y0),
				2	=> set (x4, y0),
				3	=> set (x4, y6),
				4	=> set (x0, y6))
		  );

			
		special_colon : constant type_character := (
			segment_ct => 2,
			segments => (									   
				1	=> (x2, y3, x2, y3),
				2	=> (x2, y1, x2, y1)),

			border_vertex_ct => 4,
			border => (
				1	=> set (x1, y0),
				2	=> set (x3, y0),
				3	=> set (x3, y4),
				4	=> set (x1, y4))
		  );

			
		-- A character is a list of lines. These lines are machine made. They are
		-- a result of rotation, scaling, mirroring, ...
		-- The start and end points are expressed by float numbers.
		type type_character_line is new pac_geometry_1.type_line_fine;
		
		package pac_character_lines is new doubly_linked_lists (type_character_line);
		use pac_character_lines;
		
		
		-- Converts a character to a list of lines:
		function to_lines (
			char : in type_character) 
			return pac_character_lines.list;


		type type_text_fab_with_content is new type_text_fab with record
			content	: pac_text_content.bounded_string;
		end record;


		function get_content (
			text : in type_text_fab_with_content)
			return string;
		
			
		-- Returns the position, linewidth and content
		-- of the given text:
		function to_string (
			text : in type_text_fab_with_content)
			return string;
		
		
		package pac_texts_fab_with_content is new
			doubly_linked_lists (type_text_fab_with_content);
		

			
		type type_vector_text is private;
			
		-- Renders the given text content to a vector text.
		-- CS: IMPORTANT: Argument "content" MUST contain something ! If empty
		-- constraint error will arise !
		-- If make_border is true, then for each character a border is
		-- computed. The border is a polygon that wraps around the character.
		-- This function is required for texts in fill zones. Therefore it is
		-- turned off by default.
		function vectorize_text (
			content		: in pac_text_content.bounded_string; -- MUST CONTAIN SOMETHING !
			size		: in type_text_size;
			rotation	: in pac_geometry.type_rotation;
			position	: in pac_geometry.type_vector_model;
			mirror		: in type_mirror := MIRROR_NO;
			line_width	: in pac_geometry.type_distance_positive;
			alignment	: in type_text_alignment := vector_text_alignment_default;
			make_border	: in boolean := false)
			return type_vector_text;


		-- Returns the cursor to the first line of the
		-- given vector text:
		function first (
			text	: in type_vector_text)
			return pac_character_lines.cursor;

		
		-- Iterates the lines of the given vector text:
		procedure iterate (
			text	: in type_vector_text;
			process	: not null access procedure (
				position: in pac_character_lines.cursor));


		-- Returns the lines of the given vector text:
		function get_lines (
			text	: in type_vector_text)
			return pac_character_lines.list;


		-- Returns the borders of the characters of the given vector text:
		function get_borders (
			text	: in type_vector_text)
			return pac_polygons.pac_polygon_list.list;
		
								
		-- Returns the linewidth of the given vector text:
		function get_linewidth (
			text	: in type_vector_text)
			return type_distance_positive;
		

		
		-- Mirrors a vector text along the given axis:
		procedure mirror_vector_text (
			text	: in out type_vector_text;
			axis	: in type_mirror := MIRROR_ALONG_Y_AXIS);
		

		-- Rotates a vector text by the given angle about the origin:
		procedure rotate_vector_text (
			text	: in out type_vector_text;
			angle	: in type_rotation);


		-- Move a vector text by the given offset:
		procedure move_vector_text (
			text	: in out type_vector_text;
			offset	: in type_vector_model);

		
	private
		type type_vector_text is record
			-- The line segments the text is composed of:
			lines		: pac_character_lines.list;

			-- The border around the characters (optional):
			borders		: pac_polygons.pac_polygon_list.list;

			-- The linewidth of the line segments:
			width		: pac_geometry.type_distance_positive := 0.0; -- CS use lower limit ?
		end record;
		
	end generic_pac_text;

end et_text;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
