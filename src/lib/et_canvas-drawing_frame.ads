------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          CANVAS DRAWING FRAME                            --
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

-- with et_logging;				use et_logging;

with et_frames;					use et_frames;
with et_canvas.text;

generic
	
package et_canvas.drawing_frame is

	package pac_draw_text is new et_canvas.text;
	
	
	-- Converts a type_distance (used with frames) to
	-- a distance in the model domain:
	function to_distance (
		d : in et_frames.type_distance)
		return pac_geometry.type_distance;

	
	-- Converts a type_position (used with frames) to
	-- a model vector:
	function to_vector (
		p : in et_frames.type_position)
		return type_vector_model;

	

	-- This procedure draws the outer and inner border
	-- and the quadrant bars of the frame:
	procedure draw_frame (
		frame : in type_frame_general);

	
end et_canvas.drawing_frame;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16


