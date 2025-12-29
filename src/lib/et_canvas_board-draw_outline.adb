------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          BOARD DRAW OUTLINE                              --
--                                                                          --
--                               B o d y                                    --
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

with ada.text_io;					use ada.text_io;

with et_primitive_objects;			use et_primitive_objects;
with et_board_outline;				use et_board_outline;
with et_colors.board;				use et_colors.board;


separate (et_canvas_board)

procedure draw_outline is
	
	use et_board_geometry.pac_contours;
	use pac_segments;

	use pac_draw_contours;
	
	
	
	procedure query_outline_segments (
		module_name	: in pac_module_name.bounded_string;
		module		: in type_generic_module)
	is begin
		draw_contour (
			contour	=> module.board.board_contour.outline,
			filled	=> NO,
			width	=> zero);
	end query_outline_segments;

	
	
	procedure query_holes (
		module_name	: in pac_module_name.bounded_string;
		module		: in type_generic_module) 
	is
		use pac_holes;
		
		procedure query_hole (c : in pac_holes.cursor) is 
			-- CS h : type_hole renames element (c);
		begin
			draw_contour (
				contour	=> element (c),
				filled	=> NO,
				width	=> zero);

		end query_hole;

		
	begin
		iterate (module.board.board_contour.holes, query_hole'access);
	end query_holes;

	
begin
	--put_line ("draw board outline ...");

	-- All outline segments, holes and texts will be 
	-- drawn with the same color:
	set_color_outline;

	
	pac_generic_modules.query_element (
		position	=> active_module,
		process		=> query_outline_segments'access);

	
	pac_generic_modules.query_element (
		position	=> active_module,
		process		=> query_holes'access);


	-- Draw the contour that is being drawn:
	draw_live_zone (LAYER_CAT_OUTLINE);
	draw_live_zone (LAYER_CAT_HOLE);

	
end draw_outline;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
