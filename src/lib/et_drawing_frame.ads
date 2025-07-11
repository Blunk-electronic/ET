------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           DRAWING FRAME                                  --
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

with ada.strings.bounded; 		use ada.strings.bounded;
with ada.strings.maps;			use ada.strings.maps;
with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;

with et_axes;					use et_axes;
with et_text;					use et_text;
with et_fonts;					use et_fonts;


package et_drawing_frame is



-- PAPER SIZES
	
    type type_paper_size is (A3, A4); -- CS: others ?
    paper_size_default : constant type_paper_size := A4;

	function to_paper_size (paper_size : in string) return type_paper_size;
	function to_string (paper_size : in type_paper_size) return string;

	type type_orientation is (PORTRAIT, LANDSCAPE);
	orientation_default : constant type_orientation := LANDSCAPE;

	function to_string (orientation : in type_orientation) return string;
	function to_orientation (orientation : in string) return type_orientation;

	
	template_file_name_length_max : constant positive := 300;
	template_file_name_dummy : constant string := "dummy_frame";

	-- There are only two linewidths, expressed in mm:
	linewidth_1 : constant := 0.3;
	linewidth_2 : constant := 1.0; 



	

-- COLUMNS AND ROWS:
	
	-- A drawing frame is divided in columns and rows. The columns run from 1 to maximal 26.
	-- The rows run from A to Z.
	type type_rows is new positive range 1..26;
	rows_default : constant type_rows := 7;

	function to_string (rows : in type_rows) return string;
	function to_rows (rows : in string) return type_rows;
	
	type type_columns is new positive range 1..26;
	columns_default : constant type_columns := 10;

	function to_string (columns : in type_columns) return string;
	function to_columns (columns : in string) return type_columns;
	
	row_characters : constant character_set := to_set (span => ('A','Z')); -- CS currently not used
	-- CS row numbers must be mapped to row characters 
	
	type type_sectors is record
		rows	: type_rows := rows_default; 
		columns	: type_columns := columns_default;
	end record;


	

-- DISTANCE AND DIMENSION:
	
	-- The unit for all kinds of distances in 
	-- a drawing frame is millimeters.
	-- We use whole numbers as this accurary is sufficient
	-- for everything related to a drawing frame:
	type type_distance is new integer range -10_000 .. 10_000;
	-- CS rename to type_distance_frame ?

	subtype type_distance_positive is -- CS rename to type_distance_frame_positive ?
		type_distance range 0 .. type_distance'last;
	


	
	-- Converts a distance to a string:
	function to_string (
		distance : in type_distance) 
		return string;

	
	-- Converts a string to a distance:
	function to_distance (
		distance : in string)
		return type_distance;

	

	
	-- The dimensions of a frame:
	type type_frame_size is record
		x	: type_distance_positive := 280;
		y	: type_distance_positive := 200;
	end record;


	
	-- The space between inner and outer border:
	subtype type_border_width is type_distance_positive range 4 .. 10;
	border_width_default : constant type_border_width := 5;


	

	

-- PAPER SIZES:
	
	paper_size_A3_x : constant type_distance_positive := 420;
	paper_size_A3_y : constant type_distance_positive := 297;
	
	paper_size_A4_x : constant type_distance_positive := 297;
	paper_size_A4_y : constant type_distance_positive := 210;

	
	-- Returns for the given paper size, 
	-- orientation and axis the corresponding size in mm:
	function paper_dimension (
		paper_size	: in type_paper_size;
		orientation	: in type_orientation := LANDSCAPE;
		axis		: in type_axis_2d)
		return type_distance_positive;



	

-- POSITION:


	
	-- A position in the drawing frame domain:
	type type_position is record
		x, y : type_distance := 0;
	end record;


	-- Adds two position vectors:
	function add (
		right, left : in type_position)
		return type_position;
	


	
	type type_output_format is (
		FORMAT_1,
		FORMAT_2,
		FORMAT_3);

	
	-- Converts a position to a string
	-- and formatted as follows:
	-- FORMAT_1 : "x/y 4.5 / 5.6" 
	-- FORMAT_2 : x 4.5 y 5.6
	-- FORMAT_3 : 4.5 5.6	
	function to_string (
		p 		: in type_position;
		format	: in type_output_format := FORMAT_1)
		return string;


	
	
-- LINES:
	
	type type_line is record
		A	: type_position;
		B	: type_position;
		-- CS line width ?
	end record;

	package pac_lines is new doubly_linked_lists (type_line);




	
-- TEXT:
	
	subtype type_text_size is type_distance_positive range 1 .. 50;
	
	text_size_default : constant type_text_size := 3;


	
	type type_placeholder is tagged record
		size		: type_text_size := text_size_default;
		position	: type_position;
	end record;

	
	-- These placeholders are common in both schematic and pcb title blocks:
	type type_placeholders_common is record
		project_name			: type_placeholder; -- name of project directory
		module_file_name		: type_placeholder; -- name of module file
		active_assembly_variant	: type_placeholder; -- the active assembly variant
	end record;

	
	-- Basic placeholders are separately available for schematic and pcb.
	-- For example the revision in schematic is not necessarily the same as in the layout.
	-- Another example: The person who has drawn the schematic is not necessarily the
	-- same as the one who did the layout.
	type type_placeholders_basic is tagged record
		company			: type_placeholder;
		customer		: type_placeholder;
		partcode		: type_placeholder;
		drawing_number	: type_placeholder;
		revision		: type_placeholder;
		
		drawn_by		: type_placeholder;
		checked_by		: type_placeholder;
		approved_by		: type_placeholder;

		drawn_date		: type_placeholder;
		checked_date	: type_placeholder;
		approved_date	: type_placeholder;

-- 		created_date	: type_placeholder;
-- 		edited_date		: type_placeholder;
	end record;


	
	-- Static texts are strings like "drawn" or "sheet".
	-- They are usually placed left of a placeholder:
	type type_static_text is new type_placeholder with record
		content : pac_text_content.bounded_string;
	end record;

	package pac_static_texts is new doubly_linked_lists (type_static_text);



	

	-- The basic title block:
	type type_title_block is tagged record
		position		: type_position;

		-- The positions of title block objects are relative
		-- to the position of the title block:
		lines				: pac_lines.list;
		placeholders_common	: type_placeholders_common;

		static_texts		: pac_static_texts.list;
	end record;


	
	-- GUI relevant only: The font of placeholders:
	font_placeholders : constant type_font :=
		to_font (FAMILY_MONOSPACE, SLANT_NORMAL, WEIGHT_NORMAL);	

	-- GUI relevant only: The font of other texts:
	font_texts : constant type_font :=
		to_font (FAMILY_MONOSPACE, SLANT_NORMAL, WEIGHT_NORMAL);	

	-- GUI relevant only: The font of column and row indexes:
	font_indexes : constant type_font :=
		to_font (FAMILY_MONOSPACE, SLANT_NORMAL, WEIGHT_NORMAL);

	-- GUI relevant only: The font size of column and row indexes:
	font_indexes_size : constant type_distance := 3;
	


	

-- FILE NAMES



	package pac_template_name is new generic_bounded_length (template_file_name_length_max);


	
	function to_string (name : in pac_template_name.bounded_string) return string;
	function to_template_name (name : in string) return pac_template_name.bounded_string;


	
	



	
-- GENERAL FRAME:

	-- Prefixes before enumeration types prevent clashes with gnat keywords
	-- and package names:
	domain_prefix : constant string := ("DOMAIN_");


	
	-- The frame may be used in a schematic drawing or a layout drawing:
	type type_domain is (DOMAIN_SCHEMATIC, DOMAIN_PCB);

	function to_string (domain : in type_domain) return string;
	function to_domain (domain : in string) return type_domain;


	type type_frame_general is tagged record
		paper			: type_paper_size := paper_size_default;
		orientation		: type_orientation := orientation_default;
		position		: type_position; -- of the lower-left corner
		border_width	: type_border_width := border_width_default;
		size			: type_frame_size;
		sectors			: type_sectors;
	end record;


	-- Sets the position of a frame:
	procedure set_position (
		frame 		: in out type_frame_general;
		position	: in type_position);
	

	-- Gets the position of a frame:
	function get_position (
		frame 		: in type_frame_general)
		return type_position;


	
end et_drawing_frame;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
