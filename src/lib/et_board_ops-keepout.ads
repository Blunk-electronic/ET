------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   BOARD OPERATIONS / KEEPOUT                             --
--                                                                          --
--                               S p e c                                    --
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   ToDo: 

with et_keepout;				use et_keepout;


package et_board_ops.keepout is


	-- Draws a keepout zone.
	-- The given zone can consist of a single segment or a
	-- fragment of a zone contour.
	-- 1. If the given zone is a single segment or a fragment
	--    then the procedure serches for already existing zones
	--    which are incomplete (or open) and tries to append or prepend
	--    the given zone to one of the existing open zones.
	-- 2. If this attempt fails, then the given zone is regarded as 
	--    a new zone.
	-- 3. If all existing zones are already closed, then the given zone
	--    is regarded a a new zone and added to the existing zones.
	procedure draw_zone (
		module_cursor	: in pac_generic_modules.cursor;
		zone			: in type_keepout_zone;
		face			: in type_face;
		log_threshold	: in type_log_level);

	
	
end et_board_ops.keepout;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
