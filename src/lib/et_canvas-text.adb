------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             CANVAS TEXT                                  --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2024                                                       --
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

-- with ada.strings.bounded;
-- with ada.strings;
-- with ada.strings.fixed;
-- 


package body et_canvas.text is

	procedure dummy is begin null; end;


	
	procedure draw_vector_text (
		text	: in pac_text.type_vector_text;
		width	: in pac_geometry.type_distance_positive)
	is
		use pac_text;
		use pac_character_lines;

		-- Drawing a vector-text is just a matter of
		-- drawing many lines. So we iterate the given
		-- lines of the text and draw them one by one.
		
		procedure query_line (
			c : in pac_character_lines.cursor)
		is
			-- The line of a character must now be
			-- converted to a type_line:
			lf : type_character_line renames element (c);
			lc : type_line;
		begin
			lc.start_point := to_point (lf.start_point);
			lc.end_point := to_point (lf.end_point);

			set_linewidth (width);
			
			draw_line (
				line	=> lc,
				width	=> 0.0); -- don't care
				
			stroke;
		end query_line;
		
		
	begin
		-- set_line_join (context.cr, cairo_line_join_miter); -- CS
		
		iterate (text, query_line'access);
	end draw_vector_text;

	
end et_canvas.text;
