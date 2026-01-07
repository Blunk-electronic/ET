------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                    FLOATING CONDUCTORS / PACKAGE                         --
--                                                                          --
--                              S p e c                                     --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
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


with et_mirroring;				use et_mirroring;
with et_pcb_sides;				use et_pcb_sides;
with et_board_geometry;			use et_board_geometry;
with et_board_coordinates;		use et_board_coordinates;
with et_pcb_stack;				use et_pcb_stack;
with et_board_text;				use et_board_text;

with et_design_rules_board;				use et_design_rules_board;
with et_conductor_segment;				use et_conductor_segment;

with et_conductor_text;					use et_conductor_text;
with et_conductor_text.packages;		use et_conductor_text.packages;

with et_logging;						use et_logging;


package et_conductors_floating_package is
	
	use pac_geometry_brd;

	use pac_geometry_2;
	use pac_polygons;
	use pac_contours;
	use pac_text_board;



	-- All objects of this category are floating. Means they
	-- have no connection to a pad or a track (net):
	type type_conductor_objects is record -- CS rename to type_conductors ?
		lines 		: pac_conductor_lines.list;
		arcs		: pac_conductor_arcs.list;
		circles		: pac_conductor_circles.list;
		texts		: et_conductor_text.packages.pac_conductor_texts.list;
	end record;

	
	-- Since NON ELECTRIC conductor objects of a package can be on both sides 
	-- of the board we need this type. There is no reason for NON ELECTRIC 
	-- conductor objects in inner layers. So we deal with top and bottom side only:
	type type_conductor_objects_both_sides is record -- CS rename to type_conductors_both_sides ?
		top		: type_conductor_objects;
		bottom	: type_conductor_objects;
	end record;


	-- CS procedure add_line, arc, circle, text

	procedure add_text (
		conductors	: in out type_conductor_objects_both_sides;
		text		: in type_conductor_text;
		face		: in type_face);


	
	

	-- Mirrors the given non-electric conductor objects 
	-- along the given axis:
	procedure mirror_conductor_objects (
		conductors	: in out type_conductor_objects;
		axis		: in type_mirror := MIRROR_ALONG_Y_AXIS);

	
	-- Rotates the given non-electric conductor objects 
	-- by the given angle about the origin:
	procedure rotate_conductor_objects (
		conductors	: in out type_conductor_objects;
		angle		: in type_rotation_model);

	-- Moves the given non-electric conductor objects 
	-- by the given offset:
	procedure move_conductor_objects (
		conductors	: in out type_conductor_objects;
		offset		: in type_vector_model);

	
	-- Converts the given non-electric conductor objects to polygons.
	-- NOTE regarding circles: The inside of circles is ignored. Only the outer
	--  edge of a conductor circle is converted to a polygon.
	function to_polygons (
		conductors	: in type_conductor_objects;
		tolerance	: in type_distance_positive)
		return pac_polygon_list.list;
	

	
end et_conductors_floating_package;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
