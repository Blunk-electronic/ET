------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       BOARD SHAPES AND TEXT                              --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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
with et_design_rules;			use et_design_rules;
with et_string_processing;		use et_string_processing;

package et_board_shapes_and_text is
	use pac_geometry_brd;

	-- Instantiation of the shapes package:
	package pac_shapes is new 
		et_geometry.generic_pac_shapes (et_pcb_coordinates.pac_geometry_brd);

	use pac_shapes;
		

		

	type type_text_parameters is record
		size_min 		: type_distance_positive;
		size_max 		: type_distance_positive;
		size_default 	: type_distance_positive;		

		-- These parameters are relevant for vector text:
		width_min 		: type_distance_positive;
		width_max 		: type_distance_positive;
		width_default 	: type_distance_positive;
	end record;


-- FAB RELEVANT
	text_parameters_fab : constant type_text_parameters := (
		size_min 		=> 0.5,
		size_max 		=> 100.0,
		size_default 	=> 1.5,
		width_min 		=> 0.15,
		width_max 		=> 10.0,
		width_default 	=> 0.15);

	package pac_text_fab is new et_text.generic_pac_text (
		pac_shapes			=> pac_shapes,
		size_min			=> text_parameters_fab.size_min,
		size_max			=> text_parameters_fab.size_max,
		size_default		=> text_parameters_fab.size_default,
		line_width_min		=> text_parameters_fab.width_min,
		line_width_max		=> text_parameters_fab.width_max,
		line_width_default	=> text_parameters_fab.width_default);

	
	subtype type_general_line_width is type_distance_positive
		range text_parameters_fab.width_min .. text_parameters_fab.width_max;
	
	-- Checks whether given line width is in range 
	-- of type_general_line_width:
	procedure validate_general_line_width (width : in type_distance);


	
	

-- DOCUMENTATION RELEVANT (NON-FAB)
	text_parameters_doc : constant type_text_parameters := (
		size_min 		=> 0.01,
		size_max 		=> 100.0,
		size_default 	=> 1.0,
		width_min 		=> 0.005,
		width_max 		=> 10.0,
		width_default 	=> 0.005);
	
	package pac_text_doc is new et_text.generic_pac_text (
		pac_shapes			=> pac_shapes,
		size_min			=> text_parameters_doc.size_min,
		size_max			=> text_parameters_doc.size_max,
		size_default		=> text_parameters_doc.size_default,
		line_width_min		=> text_parameters_doc.width_min,
		line_width_max		=> text_parameters_doc.width_max,
		line_width_default	=> text_parameters_doc.width_default);




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



	-- GUI relevant only
	type type_conductor_hatching is record
		-- the width of the border line
		border_width : type_track_width := type_track_width'first;
		
		-- the with of the lines inside the area:
		line_width : type_track_width := type_track_width'first;

		-- the space between the lines inside the area:
		spacing	: type_track_clearance := type_track_clearance'first;
	end record;

	

	-- Polygons in non-conducor layers such as silkscreen, stencil, ...
	type type_polygon_non_conductor (fill_style : type_fill_style) 
	is new type_polygon_base with record
		easing : type_easing;
		
		case fill_style is
			when SOLID		=> null;
			when HATCHED	=> hatching : type_hatching;
		end case;
	end record;


	
	-- the space between foreign pads and the polygon outline
	keyword_isolation : constant string := "isolation";

	-- the minimal width of a polygon
	keyword_min_width : constant string := "min_width";

	
	---- Polygons in conductor layers have a dedicated type for the hatching:
	--type type_polygon_conductor (fill_style : type_fill_style) 
	--is new type_polygon_base with record

		---- the minimum width:
		--width_min : type_track_width := type_track_width'first;

		---- the space between the polygon and foreign conductor objects:
		--isolation : type_track_clearance := type_track_clearance'first; 
	
		--easing : type_easing;
		
		--case fill_style is
			--when SOLID		=> null;
			--when HATCHED	=> hatching : type_conductor_hatching;
		--end case;
	--end record;
	


	
	-- This circle type is used by silk screen, assembly doc, stop mask, stencil
	type type_fillable_circle (
		filled		: type_filled;
		fill_style	: type_fill_style -- don't care if filled is NO
		)
		is new type_circle with record
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




	
end et_board_shapes_and_text;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
