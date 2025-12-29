------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           BOARD OUTLINE                                  --
--                                                                          --
--                              S p e c                                     --
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
--
-- DESCRIPTION:
-- 
-- This package is about the outlines of the board (PCB).
-- The term "outlines" refers to 
-- 1. the outer contour
-- 2. holes. Holes may have any shape.
--
--  to do:
--


with ada.containers; 				use ada.containers;
with ada.containers.doubly_linked_lists;

with et_mirroring;					use et_mirroring;
with et_board_geometry;				use et_board_geometry;
with et_board_text;					use et_board_text;
with et_contour_to_polygon;
with et_logging;					use et_logging;


package et_board_outline is

	use pac_geometry_brd;

	use pac_geometry_2;
	use pac_contours;
	use pac_polygons;
	
	use pac_text_board;


	-- As a safety measure we derive dedicated types for
	-- the outer and inner edge of the PCB from the general contour type.
	
	-- There is only one outer contour of a PCB:
	type type_outer_contour is new type_contour with null record;


	
	-- There can be many holes inside the PCB area:
	-- This is a single hole:
	type type_hole is new type_contour with null record;

	-- These are all holes inside the PCB area:
	package pac_holes is new doubly_linked_lists (type_hole);
	use pac_holes;


	-- CS procedure add_hole (holes : in pac_holes.list, hole);



	
	
	-- Iterates the holes.
	-- Aborts the process when the proceed-flag goes false:
	procedure iterate (
		holes	: in pac_holes.list;
		process	: not null access procedure (position : in pac_holes.cursor);
		proceed	: not null access boolean);

	

	-- Mirrors a list of holes along the given axis:
	procedure mirror_holes (
		holes	: in out pac_holes.list;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS);


	-- Rotates a list of holes about the origin by the given angle:
	procedure rotate_holes (
		holes	: in out pac_holes.list;
		angle	: in type_rotation_model);


	-- Moves a list of holes by the gvien offset:
	procedure move_holes (
		holes	: in out pac_holes.list;
		offset	: in type_vector_model);

	
	-- Converts a list of holes to a list of polygons:
	function to_polygons (
		holes		: in pac_holes.list;
		tolerance	: in type_distance_positive)
		return pac_polygon_list.list;


	-- Offsets a list of holes.
	-- The parameter "offset" is always positive because
	-- holes can only become greater:
	procedure offset_holes (
		holes		: in out pac_polygon_list.list;
		offset		: in type_distance_positive;
		log_threshold	: in type_log_level);




	-- The board contour consists of the outer contour
	-- and holes:
	type type_board_outline is record
		outline	: type_outer_contour;
		holes	: pac_holes.list;
	end record;

	-- CS
	-- The DRC shall:
	-- - detect gaps in outline
	-- - detect texts inside board area and output an error

	

	
end et_board_outline;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
