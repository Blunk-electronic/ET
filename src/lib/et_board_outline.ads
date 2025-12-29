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


with et_board_geometry;				use et_board_geometry;
with et_board_holes;				use et_board_holes;
with et_logging;					use et_logging;


package et_board_outline is

	-- use pac_geometry_brd;

	use pac_geometry_2;
	use pac_contours;
	-- use pac_polygons;
	

	-- As a safety measure we derive dedicated types for
	-- the outer and inner edge of the PCB from the general contour type.
	
	-- There is only one outer contour of a PCB:
	type type_outer_contour is new type_contour with null record;


	procedure dummy;
	


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
