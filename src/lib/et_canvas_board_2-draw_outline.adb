------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                          BOARD DRAW OUTLINE                              --
--                                                                          --
--                               B o d y                                    --
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

with ada.text_io;				use ada.text_io;
with et_schematic;

with et_pcb_contour;			use et_pcb_contour;
with et_colors.board;			use et_colors.board;


separate (et_canvas_board_2)

procedure draw_outline is
	
	use pac_contours;
	use pac_segments;

	
	procedure query_segment (c : in pac_segments.cursor) is 
		-- CS s : type_segment renames element (c);
	begin
		case element (c).shape is
			when LINE =>
				draw_line (
					line	=> element (c).segment_line,
					width	=> 0.0); -- don't care

			when ARC =>
				draw_arc (
					arc		=> element (c).segment_arc,
					width	=> 0.0); -- don't care
		end case;
	end query_segment;

	
	
	procedure query_outline_segments (
		module_name	: in pac_module_name.bounded_string;
		module		: in type_generic_module)
	is begin
		if module.board.contours.outline.contour.circular then

			draw_circle (
				circle	=> module.board.contours.outline.contour.circle,
				filled	=> NO, -- circles in outline are never filled
				width	=> 0.0); -- don't care
		else
			iterate (module.board.contours.outline.contour.segments, query_segment'access);
		end if;
	end query_outline_segments;

	
	
	procedure query_holes (
		module_name	: in pac_module_name.bounded_string;
		module		: in type_generic_module) 
	is
		use pac_holes;
		
		procedure query_hole (c : in pac_holes.cursor) is 
			-- CS h : type_hole renames element (c);
		begin
			if element (c).contour.circular then

				draw_circle (
					circle	=> element (c).contour.circle,
					filled	=> NO, -- holes are never filled
					width	=> 0.0); -- don't care
				
			else
				iterate (element (c).contour.segments, query_segment'access);
			end if;
		end query_hole;

		
	begin
		iterate (module.board.contours.holes, query_hole'access);
	end query_holes;

	
begin -- draw_outline
	-- put_line ("draw board outline ...");

	-- All outline segments, holes and texts will be 
	-- drawn with the same color:
	set_color_outline;

	-- All outline segments and holes be 
	-- drawn with the same linewidth:
	set_linewidth (pcb_contour_line_width);

	-- The line width of texts is a property of a particular text and is
	-- NOT set here.
	
	pac_generic_modules.query_element (
		position	=> active_module,
		process		=> query_outline_segments'access);

	
	pac_generic_modules.query_element (
		position	=> active_module,
		process		=> query_holes'access);


	-- Draw the contour that is being drawn:
    draw_live_zone (LAYER_CAT_OUTLINE);


	stroke;
	
end draw_outline;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
