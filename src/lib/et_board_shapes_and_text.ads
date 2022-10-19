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


-- HATCHING OF OBJECTS WITH CLOSED CIRCUMFENCE
	keyword_hatching_line_width		: constant string := "hatching_line_width";
	keyword_hatching_border_width	: constant string := "hatching_border_width";	
	keyword_hatching_line_spacing	: constant string := "hatching_line_spacing";		

	hatching_line_width_default : constant type_distance_positive := 0.2;
	hatching_spacing_default	: constant type_distance_positive := 1.0;
	
	
	type type_hatching is record
		-- the width of the border line
		border_width : type_distance_positive := 1.0;
		
		-- the with of the lines inside the area:
		line_width : type_distance_positive := hatching_line_width_default;

		-- the space between the lines inside the area:
		spacing	: type_distance_positive := hatching_spacing_default;
	end record;


	
-- EASING
	keyword_easing_style : constant string := "easing_style";
	keyword_easing_radius : constant string := "easing_radius";	

	type type_easing_style is (NONE, CHAMFER, FILLET);

	function to_easing_style (easing : in string) return type_easing_style;
	function to_string (easing : in type_easing_style) return string;
	
	easing_radius_max : constant type_distance_positive := 100.0;
	subtype type_easing_radius is type_distance_positive range type_distance_positive'first .. easing_radius_max;

	type type_easing is record
		style	: type_easing_style := NONE;
		radius	: type_easing_radius := zero; -- center of circle at corner point
	end record;


	

	-- Contours in non-conducor layers such as silkscreen, stencil, ...
	type type_contour_non_conductor (fill_style : type_fill_style) 
		is new type_contour with 
	record
		easing : type_easing;
		
		case fill_style is
			when SOLID		=> null;
			when HATCHED	=> hatching : type_hatching;
		end case;
	end record;


	

	

	
	-- This circle type is used by silk screen, assembly doc, stop mask, stencil
	type type_fillable_circle (
		filled		: type_filled;
		fill_style	: type_fill_style) -- don't care if filled is NO
		is new type_circle 
	with record
		case filled is
			when NO => 
				-- the line width of the circumfence:
				border_width : type_general_line_width := type_general_line_width'first;

			when YES =>
				case fill_style is
					when SOLID => null;
					when HATCHED =>
						hatching : type_hatching;
				end case;
				
		end case;
	end record;

	-- CS type_circle_cutout ?
	
	function to_string (circle : in type_fillable_circle) return string;

	-- This circle type is used by keepout, route restrict, via restrict.
	-- The fill style is always solid, hence no discrimintant for fiil style.
	-- When drawing, for the width of the border a fixed value will be applied.
	type type_fillable_circle_solid is new type_circle with record
		filled : type_filled;
	end record;





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

	subtype type_layer_category_outline is type_layer_category
		range LAYER_CAT_OUTLINE .. LAYER_CAT_OUTLINE;
	
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
