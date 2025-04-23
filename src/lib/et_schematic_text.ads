------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            SCHEMATIC TEXT                                --
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
--   ToDo: 
--

with et_sheets;							use et_sheets;
with et_coordinates_2;					use et_coordinates_2;

with et_schematic_shapes_and_text;		use et_schematic_shapes_and_text;

with ada.containers; 					use ada.containers;
with ada.containers.doubly_linked_lists;

with et_text;							use et_text;
with et_fonts;							use et_fonts;


package et_schematic_text is


	use pac_geometry_2;


	text_size_min : constant type_distance_positive := 1.0;
	text_size_max : constant type_distance_positive := 50.0;
	text_size_default : constant type_distance_positive := 1.3;
	
	subtype type_text_line_width is type_distance_positive range 0.0 .. 5.0; -- unit is mm -- CS: minimum of 0.0 reasonable ?
	text_line_width_min : constant type_distance_positive := 0.1;
	text_line_width_max : constant type_distance_positive := 5.0;
	text_line_width_default : constant type_distance_positive := 0.3; 

	
	-- Instantiation of the text package:
	package pac_text_schematic is new et_text.generic_pac_text (
		pac_geometry		=> pac_geometry_2,
		pac_polygons		=> pac_polygons, -- never used, but mandatory for instantiation
		pac_offsetting		=> pac_polygon_offsetting,
		size_min			=> text_size_min,
		size_max			=> text_size_max,
		size_default		=> text_size_default,
		line_width_min		=> text_line_width_min,
		line_width_max		=> text_line_width_max,
		line_width_default	=> text_line_width_default
		);

	use pac_text_schematic; 
	

	-- These are basic properties a text has got:
	type type_text_basic is new type_text with record
		-- CS font : type_font; ?
        rotation	: et_text.type_rotation_documentation := et_text.type_rotation_documentation'first;
	end record;

	
	

	

	-- GUI relevant only: The font of a text/note in the schematic:
	text_font : constant type_font :=
		to_font (FAMILY_MONOSPACE, SLANT_NORMAL, WEIGHT_NORMAL);

	
	-- A text/note in the schematic:
	type type_text is new pac_text_schematic.type_text with record
		position	: type_vector_model;
		rotation	: type_rotation_documentation := et_text.HORIZONTAL;
		sheet		: type_sheet := type_sheet'first;
		content		: pac_text_content.bounded_string;
		--font		: et_text.type_font;
	end record;
		
	package pac_texts is new doubly_linked_lists (type_text);

	

	procedure dummy;


end et_schematic_text;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
