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
--   to do:


with et_text;
with et_pcb_coordinates;		use et_pcb_coordinates;
with et_geometry;				use et_geometry;
with et_geometry_2;
with et_geometry_2.contours;
with et_geometry_1.et_polygons;
with et_geometry_1.et_polygons.clipping;
with et_geometry_1.et_polygons.cropping;
with et_geometry_1.et_polygons.union;
with et_geometry_1.et_polygons.offsetting;

with et_design_rules;			use et_design_rules;
with et_logging;				use et_logging;


package et_board_shapes_and_text is

	use pac_geometry_brd;
	use pac_geometry_2;
		

	package pac_contours is new pac_geometry_2.contours;
	use pac_contours;

	
	package pac_polygons is new pac_geometry_1.et_polygons;
	use pac_polygons;
	
	package pac_polygon_clipping is new pac_polygons.clipping;
	package pac_polygon_cropping is new pac_polygons.cropping;
	package pac_polygon_union is new pac_polygons.union;
	package pac_polygon_offsetting is new pac_polygons.offsetting;



-- FAB RELEVANT

	--fab_tolerance : constant type_distance_positive := 0.001;
	--fab_tolerance : constant type_distance_positive := 0.01;
	fill_tolerance : constant type_distance_positive := 0.05;


	linewidth_fab_min : constant type_distance_positive := 0.005;
	linewidth_fab_max : constant type_distance_positive := 10.0;
	
	package pac_text_board is new et_text.generic_pac_text (
		pac_geometry_2		=> pac_geometry_2,
		pac_polygons		=> pac_polygons,											 
		pac_offsetting		=> pac_polygon_offsetting,
		size_min			=> 0.01,
		size_max			=> 100.0,
		size_default		=> 1.0,
		line_width_min		=> linewidth_fab_min,
		line_width_max		=> linewidth_fab_max,
		line_width_default	=> 0.005);

	
	subtype type_general_line_width is type_distance_positive
		range linewidth_fab_min .. linewidth_fab_max;
	
	-- Checks whether given line width is in range 
	-- of type_general_line_width:
	procedure validate_general_line_width (
		width : in et_pcb_coordinates.type_distance);


	keyword_width 		: constant string := "width";
	keyword_linewidth	: constant string := "linewidth";



-- LAYER CATEGORY

	-- Prefixes before enumeration types prevent clashes with gnat keywords
	-- and package names:	
	layer_category_prefix : constant string := "LAYER_CAT_";

	type type_layer_category is (
		LAYER_CAT_OUTLINE,
		-- CS LAYER_CAT_OUTLINE_TEMPLATE -- CS
									
		-- NON CONDUCTOR LAYERS.
		-- These layers are paired. Means there is a TOP and a BOTTOM:
		LAYER_CAT_SILKSCREEN,
		LAYER_CAT_ASSY,
		LAYER_CAT_KEEPOUT,
		LAYER_CAT_STENCIL,
		LAYER_CAT_STOP,

		-- CONDUCTOR LAYERS.
		-- These layers are numbered:
		LAYER_CAT_CONDUCTOR,
		
		-- NOTE: Restrict layers do not contain any conducting
		-- objects. They are irrelevant for manufacturing.
		-- Since they are of mere supportive nature for routing
		-- we regarded them as conductor layers.
		-- These layers are numbered:
		LAYER_CAT_ROUTE_RESTRICT,
		LAYER_CAT_VIA_RESTRICT);

	
	subtype type_layer_category_non_conductor is type_layer_category
		range LAYER_CAT_SILKSCREEN .. LAYER_CAT_STOP;

	subtype type_layer_category_conductor is type_layer_category
		range LAYER_CAT_CONDUCTOR .. LAYER_CAT_VIA_RESTRICT;

	subtype type_layer_category_restrict is type_layer_category
		range LAYER_CAT_ROUTE_RESTRICT .. LAYER_CAT_VIA_RESTRICT;
	
	
	function to_layer_category (cat : in string) return type_layer_category;
	function to_string (cat : in type_layer_category) return string;



	-- Maps from face to mirror status of a vectorized text.
	-- Use it for non-device related texts and placeholders.
	function face_to_mirror (f : in type_face) 
		return et_text.type_vector_text_mirrored;

	
	
end et_board_shapes_and_text;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
