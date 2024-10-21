------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                      BOARD OPERATIONS FRAME                              --
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


with et_logging;					use et_logging;
with et_frames;


package et_board_ops.frame is

	use pac_generic_modules;

	
	-- Moves the lower-left corner (which is the origin)
	-- of the drawing frame to the given point:
	procedure move_drawing_frame (
		module_cursor	: in pac_generic_modules.cursor;
		coordinates		: in type_coordinates; -- relative/absolute		
		point			: in et_frames.type_position; -- x/y
		log_threshold	: in type_log_level);


	-- Returns the position of the lower-left corner
	-- of the drawing frame:
	function get_frame_position (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)								
		return et_frames.type_position;
	

	-- Sets the position of the lower-left corner
	-- of the drawing frame:
	procedure set_frame_position (
		module_cursor	: in pac_generic_modules.cursor;
		position		: in et_frames.type_position;
		log_threshold	: in type_log_level);


	-- CS subprograms to get and set the title block position ?
	-- currently this is defined in the frame template file *.frb
	
end et_board_ops.frame;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
