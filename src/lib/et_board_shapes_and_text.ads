------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       BOARD SHAPES AND TEXT                              --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                --
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
--   to do:


with et_text;
with et_pcb_sides;				use et_pcb_sides;
with et_pcb_coordinates_2;		use et_pcb_coordinates_2;
with et_geometry_2a;
with et_geometry_2a.contours;
with et_geometry_1.et_polygons;
with et_geometry_1.et_polygons.clipping;
with et_geometry_1.et_polygons.cropping;
with et_geometry_1.et_polygons.union;
with et_geometry_1.et_polygons.offsetting;

with et_design_rules_board;		use et_design_rules_board;
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
		pac_geometry		=> pac_geometry_2,
		pac_polygons		=> pac_polygons,											 
		pac_offsetting		=> pac_polygon_offsetting,
		size_min			=> 0.01,
		size_max			=> 100.0,
		size_default		=> 1.0,
		line_width_min		=> linewidth_fab_min,
		line_width_max		=> linewidth_fab_max,
		line_width_default	=> 0.005);

	
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
