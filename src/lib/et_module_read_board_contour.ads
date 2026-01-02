------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                    MODULE READ / BOARD CONTOUR                           --
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
--                                                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab with in your edtior to 4.

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
-- 1. This package is about all kinds of contours in the board drawing.

--
--
-- ToDo:
-- - clean up
--
--
--

with et_board_geometry;			use et_board_geometry;
with et_generic_modules;		use et_generic_modules;
with et_string_processing;		use et_string_processing;
with et_logging;				use et_logging;



package et_module_read_board_contour is


	use pac_geometry_2;
	use pac_contours;
	

	contour_line	: type_line;
	contour_arc		: type_arc;
	contour_circle	: type_circle;
	contour			: type_contour;


	
	procedure read_contour_line (
		line : type_fields_of_line);

		
	procedure read_contour_arc (
		line : type_fields_of_line);

	
	procedure read_contour_circle (
		line : type_fields_of_line);

	

	procedure insert_contour_line;

	procedure insert_contour_arc;

	procedure insert_contour_circle;


	procedure check_contour (
		log_threshold : in type_log_level);
		
	
end et_module_read_board_contour;

	


	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
