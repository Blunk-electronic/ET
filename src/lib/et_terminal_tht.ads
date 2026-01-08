------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                         TERMINAL THROUGH HOLE                            --
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
-- to do:
--


with et_board_geometry;			use et_board_geometry;
with et_terminal_stopmask;		use et_terminal_stopmask;
with et_logging;				use et_logging;


package et_terminal_tht is

	use pac_geometry_2;
	use pac_contours;



	procedure log_plated_millings (
		millings 		: in type_contour;
		log_threshold	: in type_log_level);

	
	plated_millings_default : type_contour;
	-- CS this variable should never be changed.
	-- Find a way to make it a constant.



		
	-- The shape on the top side
	-- is not nessecarily the same as on the bottom side.
	type type_pad_outline_tht is record
		top		: type_contour; 
		bottom	: type_contour; 
	end record;






	-- A THT pad has stopmask on top and bottom side.
	-- The shape on the top side
	-- is not nessecarily the same as on the bottom side:
	type type_stopmask_tht is record
		top		: type_stopmask_shape;
		bottom	: type_stopmask_shape;
	end record;

	
	
	
end et_terminal_tht;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
