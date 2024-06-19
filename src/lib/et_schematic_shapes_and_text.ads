------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       BOARD SHAPES AND TEXT                              --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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
with ada.text_io;				use ada.text_io;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with cairo;


with et_coordinates_2;			use et_coordinates_2;
with et_string_processing;
with et_general;
with et_text;
with et_logging;				use et_logging;


package et_schematic_shapes_and_text is

	--use pac_geometry_sch;
	use pac_geometry_2;


	
-- TEXT

	text_size_min : constant type_distance_model_positive := 1.0;
	text_size_max : constant type_distance_model_positive := 50.0;
	text_size_default : constant type_distance_model_positive := 1.3;
	
	subtype type_text_line_width is type_distance_model_positive range 0.0 .. 5.0; -- unit is mm -- CS: minimum of 0.0 reasonable ?
	text_line_width_min : constant type_distance_model_positive := 0.1;
	text_line_width_max : constant type_distance_model_positive := 5.0;
	text_line_width_default : constant type_distance_model_positive := 0.3; 

	
	-- Instantiation of the text package:
	package pac_text_schematic is new et_text.generic_pac_text (
		pac_geometry_2		=> pac_geometry_2,
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

	
		
end et_schematic_shapes_and_text;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
